module StrDsl where 
    import Data.List
    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import Types

    -- define the semantics
    newtype T = Term String deriving (Show, Eq, Ord)
    newtype X = Input String deriving (Show, Eq, Ord)
    data S = Concat S S | LeftS S I | RightS S I | Substr S I I | Replace S I I S | Trim S | Repeat S I | Substitute S S S | ToText I | X X | T T | ConstStr String deriving (Show, Eq, Ord)
    data I = Add I I | Sub I I | Find S S | Find2 S S I | Len S | Const Int deriving (Show, Eq, Ord)
    data B = Equals S S | GreaterThan I I | GreaterThanOrEqual I I deriving (Show, Eq, Ord)
    data E = S S | I I | B B | Placeholder deriving (Show, Eq, Ord)

    -- more complicated operation types
    data Op = ConcatOp | LeftSOp | RightSOp | TrimOp | RepeatOp | ToTextOp | AddOp | SubOp | FindOp | LenOp | EqualsOp | GreaterThanOp | GreaterThanOrEqualOp deriving (Show, Eq, Ord)
    data Type = Es | Ei | Eb deriving (Show, Eq, Ord)
    opToTypeMap :: (Op, Type) -> Map.Map Op [Type] 
    opToTypeMap (_, _) = Map.fromList [(ConcatOp, [Es, Es]), (LeftSOp, [Es, Ei]), (RightSOp, [Es, Ei]), (FindOp, [Ei, Ei])]
    opList :: Op -> [Op] 
    opList _ = [ConcatOp, LeftSOp, RightSOp, FindOp, LenOp]
    typeList :: Type -> [Type] 
    typeList _ = [Es, Ei, Eb]

    isEofType :: (E, Type) -> Bool
    isEofType (e,t) = case (e,t) of (S _, Es) -> True; (I _, Ei) -> True; (B _, Eb) -> True; _ -> False

    -- convert regular to terminal type 
    convertVToEList :: (V, E) -> [E]
    convertVToEList (v,_) = 
        case v of 
            Vs s -> map (\ch -> S (T (Term [ch]))) s
            Vi i -> [I (Const i)]
            _ -> []

    -- handle unwrap op
    handleUnwrapOp :: (Op, [E]) -> E
    handleUnwrapOp (op, elist) = 
        case op of
            ConcatOp -> case (head elist, last elist) of 
                (S x, S y) -> S (Concat x y) 
                _ -> Placeholder
            LeftSOp -> case (head elist, last elist) of 
                (S x, I y) -> S (LeftS x y) 
                _ -> Placeholder
            RightSOp -> case (head elist, last elist) of 
                (S x, I y) -> S (RightS x y) 
                _ -> Placeholder
            TrimOp -> case head elist of 
                S x -> S (Trim x) 
                _ -> Placeholder
            RepeatOp -> case (head elist, last elist) of 
                (S x, I y) -> S (Repeat x y) 
                _ -> Placeholder
            ToTextOp -> case head elist of 
                I y -> S (ToText y)
                _ -> Placeholder
            AddOp -> case (head elist, last elist) of 
                (I x, I y) -> I (Add x y) 
                _ -> Placeholder
            SubOp -> case (head elist, last elist) of 
                (I x, I y) -> I (Sub x y) 
                _ -> Placeholder
            FindOp -> case (head elist, last elist) of 
                (S x, S y) -> I (Find x y) 
                _ -> Placeholder
            LenOp -> case head elist of 
                (S x) -> I (Len x) 
                _ -> Placeholder
            _ -> Placeholder

    -- return all terminals
    terminals :: ([([V], V)], [String], E) -> [E]
    terminals (ioList, argList, _) = 
        let baseChars :: [String] = ["", " ", ",", ".", "!", "?", "(", ")", "[", "]", "<", ">", "{", "}", "-", "+", "_", "/", "$", "#", ":", ";", "@", "%", "0"] in
        let baseInts :: [Int] = [0, 1, 2, 3, 99] in
        let inputTermList :: [[E]] = map (\(inputVList,outputV) -> convertVToEList (outputV, Placeholder) ++ concatMap (\x -> convertVToEList (x, Placeholder)) inputVList) ioList in
        -- return all terminals
        let summedTerminals :: [E] = map (S . X . Input) argList ++ map (S . T . Term) baseChars ++ map (I . Const) baseInts ++ concat inputTermList in 
        Set.toList (Set.fromList summedTerminals)

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
                        maybe None Vi val
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

    instance DSLExprUtils E where 
        convertVToExprList = convertVToEList
        getTerminals = terminals
        eval = evalE

    instance DSLOpList Op where 
        getOpList = opList

    instance DSLTypeList Type where 
        getTypeList = typeList

    instance DSLOpTypeMap Op Type where 
        getMap = opToTypeMap

    instance DSLTypeCheck E Type where 
        isExprofType = isEofType

    instance DSLOpConversion E Op where 
        convertOpExprListToExpr = handleUnwrapOp