module StrDsl where 
    import Data.List
    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import Types

    -- define the semantics
    data T = Term String deriving (Show, Eq, Ord)
    data X = Input String deriving (Show, Eq, Ord)
    data S = Concat S S | LeftS S I | RightS S I | Substr S I I | Replace S I I S | Trim S | Repeat S I | Substitute S S S | ToText I | X X | T T | ConstStr String deriving (Show, Eq, Ord)
    data I = Add I I | Sub I I | Find S S | Find2 S S I | Len S | Const Int deriving (Show, Eq, Ord)
    data B = Equals S S | GreaterThan I I | GreaterThanOrEqual I I deriving (Show, Eq, Ord)
    data E = S S | I I | B B | Placeholder deriving (Show, Eq, Ord)

    -- convert regular to terminal type 
    convertVToEList :: (V, E) -> [E]
    convertVToEList (v,_) = 
        case v of 
            Vs s -> map (\ch -> S (T (Term [ch]))) s
            Vi i -> [I (Const i)]
            _ -> []

    -- return all terminals
    terminals :: ([([V], V)], [String], E) -> [E]
    terminals (ioList, argList, _) = 
        let baseChars :: [String] = ["", " ", ",", ".", "!", "?", "(", ")", "[", "]", "<", ">", "{", "}", "-", "+", "_", "/", "$", "#", ":", ";", "@", "%", "0"] in
        let baseInts :: [Int] = [0, 1, 2, 3, 99] in
        let inputTermList :: [[E]] = map (\(inputVList,outputV) -> (convertVToEList (outputV, Placeholder)) ++ (concat (map (\x -> convertVToEList (x, Placeholder)) inputVList))) ioList in
        -- return all terminals
        let summedTerminals :: [E] = (map (\x -> S (T (Term x))) baseChars) ++ (map (\i -> I (Const i)) baseInts) ++ (map (\x -> S (X (Input x))) argList) ++ (concat inputTermList) in 
        Set.toList (Set.fromList summedTerminals)

    growEList :: [E] -> [E]
    growEList [] = []
    growEList elist = 
        let slist = filter (\x -> case x of S _ -> True; _ -> False) elist in
        let ilist = filter (\x -> case x of I _ -> True; _ -> False) elist in

        -- handle Str Operations
        let concatElist = concat (map (\(S x) -> map (\(S y) -> S (Concat x y)) slist) slist) in
        let leftElist = concat (map (\(S x) -> map (\(I y) -> S (LeftS x y)) ilist) slist) in
        let rightElist = concat (map (\(S x) -> map (\(I y) -> S (RightS x y)) ilist) slist) in
        let trimElist = map (\(S x) -> S (Trim x)) slist in
        let repeatElist = concat (map (\(S x) -> map (\(I y) -> S (Repeat x y)) ilist) slist) in
        let toTextElist = map (\(I x) -> S (ToText x)) ilist in
        
        -- handle Int Operations
        let addeList = concat (map (\(I x) -> map (\(I y) -> I (Add x y)) ilist) ilist) in
        let subeList = concat (map (\(I x) -> map (\(I y) -> I (Sub x y)) ilist) ilist) in
        let findeList = concat (map (\(S x) -> map (\(S y) -> I (Find x y)) slist) slist) in
        let leneList = map (\(S x) -> I (Len x)) slist in

        -- return updated expr list
        elist ++ 
        concatElist ++ leftElist ++ rightElist ++ trimElist ++ repeatElist ++ toTextElist ++
        addeList ++ subeList ++ findeList ++ leneList

    -- eval function
    evalE :: (E, Map.Map String V) -> V
    evalE (expr, hm) = 
        case expr of
            S s ->
                case s of 
                    Concat s1 s2 ->
                        let (Vs s1eval) = evalE (S s1, hm) in
                        let (Vs s2eval) = evalE (S s2, hm) in
                        Vs (s1eval ++ s2eval)
                    LeftS lefts i -> 
                        let (Vs seval) = evalE (S lefts, hm) in
                        let (Vi ieval) = evalE (I i, hm) in
                        Vs (take ieval seval)
                    RightS rights i ->
                        let (Vs seval) = evalE (S rights, hm) in
                        let (Vi ieval) = evalE (I i, hm) in
                        Vs (drop (length seval - ieval) seval) 
                    Trim trims -> 
                        let (Vs seval) = evalE (S trims, hm) in
                        Vs (dropWhile (==' ') seval)
                    Repeat repeats i ->
                        let (Vs seval) = evalE (S repeats, hm) in
                        let (Vi ieval) = evalE (I i, hm) in
                        Vs (concat (replicate ieval seval))
                    ToText i -> 
                        let (Vi ieval) = evalE (I i, hm) in
                        Vs (show ieval)
                    X (Input k) -> 
                        case Map.lookup k hm of
                            Just (Vs seval) -> Vs seval
                            _ -> None
                    T (Term t) -> Vs t
                    _ -> None
            I i -> 
                case i of 
                    Add i1 i2 -> 
                        let (Vi i1eval) = evalE (I i1, hm) in
                        let (Vi i2eval) = evalE (I i2, hm) in
                        Vi (i1eval + i2eval)
                    Sub i1 i2 -> 
                        let (Vi i1eval) = evalE (I i1, hm) in
                        let (Vi i2eval) = evalE (I i2, hm) in
                        Vi (i1eval - i2eval)
                    Find s1 s2 ->     
                        let (Vs s1eval) = evalE (S s1, hm) in
                        let (Vs s2eval) = evalE (S s2, hm) in
                        let val = findIndex (isPrefixOf s1eval) (tails s2eval) in 
                        case val of
                            Nothing -> None
                            Just ifind -> Vi ifind
                    Len s -> 
                        let (Vs seval) = evalE (S s, hm) in
                        Vi (length seval)
                    Const i ->
                        Vi i
                    _ -> None
            B b -> 
                case b of 
                    Equals s1 s2 -> 
                        let (Vs s1eval) = evalE (S s1, hm) in
                        let (Vs s2eval) = evalE (S s2, hm) in
                        Vb (s1eval == s2eval)
                    GreaterThan i1 i2 -> 
                        let (Vi i1eval) = evalE (I i1, hm) in
                        let (Vi i2eval) = evalE (I i2, hm) in
                        Vb (i1eval > i2eval)
                    GreaterThanOrEqual i1 i2 -> 
                        let (Vi i1eval) = evalE (I i1, hm) in
                        let (Vi i2eval) = evalE (I i2, hm) in
                        Vb (i1eval >= i2eval)
            _ -> None

    instance DSLInterface E where 
        convertVToExprList = convertVToEList
        getTerminals = terminals
        growProgramList = growEList
        eval = evalE