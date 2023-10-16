import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

ioList :: [([String], V)] = [(["hello"], Vs "h"), (["world"], Vs "w")]
argList :: [String] = ["x"]

data T = Term String deriving (Show)
data X = Input String deriving (Show)
data S = Concat S S | LeftS S I | RightS S I | Substr S I I | Replace S I I S | Trim S | Repeat S I | Substitute S S S | ToText I | X X | T T | ConstStr String deriving (Show)
data I = Add I I | Sub I I | Find S S | Find2 S S I | Len S | Const Int deriving (Show)
data B = Equals S S | GreaterThan I I | GreaterThanOrEqual I I deriving (Show)
data E = S S | I I | B B deriving (Show)

data V = Vs String | Vi Int | Vb Bool | None deriving (Show, Eq)

-- convert string to list of strings
strToList :: String -> [String]
strToList "" = []  
strToList (x:xs) = [x] : strToList xs

-- convert arg list into input terminals
buildArgList :: [String] -> [E]
buildArgList [] = []
buildArgList (x:xs) = S (X (Input x)) : buildArgList xs

-- convert io list into input string terminals
convertVToStrList :: V -> [String]
convertVToStrList v = 
    case v of 
        Vs s -> strToList s
        Vi i -> strToList (show i)
        _ -> []

buildInputStrList :: [([String], V)] -> [String]
buildInputStrList [] = []
buildInputStrList ((inputStrList,outputV):xs) = ((convertVToStrList outputV) ++ concat (map strToList inputStrList)) ++ (buildInputStrList xs)

-- convert regular to terminal type 
convertTStrToTExpr :: String -> E
convertTIntToTExpr :: Int -> E
convertTStrToTExpr s = S (T (Term s))
convertTIntToTExpr i = I (Const i)

-- return all terminals
terminals :: ([([String], V)], [String]) -> [E]
terminals (ioList, argList) = 
    let baseChars :: [String] = ["", " ", ",", ".", "!", "?", "(", ")", "[", "]", "<", ">", "{", "}", "-", "+", "_", "/", "$", "#", ":", ";", "@", "%", "0"] in
    let baseInts :: [Int] = [0, 1, 2, 3, 99] in
    let resArgList :: [E] = buildArgList argList in
    let resInputList :: [String] = Set.toList (Set.fromList (buildInputStrList ioList)) in
    -- return all terminals
    (map convertTStrToTExpr baseChars) ++ 
    (map convertTIntToTExpr baseInts) ++ 
    resArgList ++ 
    (map convertTStrToTExpr resInputList)

-- grow operation
growProgramList :: [E] -> [E]
growProgramList [] = []
growProgramList elist = 
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
argsMap :: Map.Map String String = Map.fromList [("x", "hello")]
eval :: (E, Map.Map String String) -> V
eval (expr, hm) = 
    case expr of
        S s ->
            case s of 
                Concat s1 s2 ->
                    let (Vs s1eval) = eval (S s1, hm) in
                    let (Vs s2eval) = eval (S s2, hm) in
                    Vs (s1eval ++ s2eval)
                LeftS lefts i -> 
                    let (Vs seval) = eval (S lefts, hm) in
                    let (Vi ieval) = eval (I i, hm) in
                    Vs (take ieval seval)
                RightS rights i ->
                    let (Vs seval) = eval (S rights, hm) in
                    let (Vi ieval) = eval (I i, hm) in
                    Vs (drop (length seval - ieval) seval) 
                Trim trims -> 
                    let (Vs seval) = eval (S trims, hm) in
                    Vs (dropWhile (==' ') seval)
                Repeat repeats i ->
                    let (Vs seval) = eval (S repeats, hm) in
                    let (Vi ieval) = eval (I i, hm) in
                    Vs (concat (replicate ieval seval))
                ToText i -> 
                    let (Vi ieval) = eval (I i, hm) in
                    Vs (show ieval)
                X (Input k) -> 
                    case Map.lookup k hm of
                        Nothing -> Vs k -- this case shouldn't be reached?
                        Just seval -> Vs seval
                T (Term t) -> Vs t
                _ -> None
        I i -> 
            case i of 
                Add i1 i2 -> 
                    let (Vi i1eval) = eval (I i1, hm) in
                    let (Vi i2eval) = eval (I i2, hm) in
                    Vi (i1eval + i2eval)
                Sub i1 i2 -> 
                    let (Vi i1eval) = eval (I i1, hm) in
                    let (Vi i2eval) = eval (I i2, hm) in
                    Vi (i1eval - i2eval)
                Find s1 s2 ->     
                    let (Vs s1eval) = eval (S s1, hm) in
                    let (Vs s2eval) = eval (S s2, hm) in
                    let val = findIndex (isPrefixOf s1eval) (tails s2eval) in 
                    case val of
                        Nothing -> None
                        Just ifind -> Vi ifind
                Len s -> 
                    let (Vs seval) = eval (S s, hm) in
                    Vi (length seval)
                Const i ->
                    Vi i
                _ -> None
        B b -> 
            case b of 
                Equals s1 s2 -> 
                    let (Vs s1eval) = eval (S s1, hm) in
                    let (Vs s2eval) = eval (S s2, hm) in
                    Vb (s1eval == s2eval)
                GreaterThan i1 i2 -> 
                    let (Vi i1eval) = eval (I i1, hm) in
                    let (Vi i2eval) = eval (I i2, hm) in
                    Vb (i1eval > i2eval)
                GreaterThanOrEqual i1 i2 -> 
                    let (Vi i1eval) = eval (I i1, hm) in
                    let (Vi i2eval) = eval (I i2, hm) in
                    Vb (i1eval >= i2eval)
        
-- check if expression is correct
evalExprCorrectness :: (E, [([String], V)], [String]) -> Bool
evalExprCorrectness (expr, ioList, args) = 
    foldl (\x y -> x && y) True
    (map
        (\(inputs, output) -> 
            let mp' = Map.fromList (zip args inputs) in
            eval (expr, mp') == output)
    ioList)

evalonIoList :: (E, [[String]], [String]) -> [V]
evalonIoList (expr, ioList, args) = 
    map 
    (\inp -> 
        let mp' = Map.fromList (zip args inp) in
        eval (expr, mp'))
    ioList

-- eliminate eval equivalents
-- TODO: the algorithm is actually able to find, we just need to speed up this step
elimObservationalEquivalence :: ([E], [[String]], [String]) -> [E]
elimObservationalEquivalence ([], _, _) = []
elimObservationalEquivalence ((ehd:etl), inputList, args) = 
    let hdRes = evalonIoList (ehd, inputList, args) in
    let filteredElist = filter (\x -> not ((evalonIoList (x, inputList, args)) == hdRes)) etl in 
    ehd : (elimObservationalEquivalence (filteredElist, inputList, args))

-- write the overall algorithm

checkPList :: ([E], [([String], V)], [String], Set V) -> Maybe E
checkPList ([], _, _) = Nothing
checkPList ((p:plist'), ioList, args, valStore) = 
    if evalExprCorrectness (p, ioList, args) then Just p 
    else checkPList (plist', ioList, args)

runRecCheck :: ([E], [([String], V)], [[String]], [String]) -> E
runRecCheck (pList, ioList, inpList, args) = 
    case checkPList (pList, ioList, args) of 
        Nothing -> 
            let pList' = growProgramList pList in
            let valStore = Set.empty in 
            

            let pList'' = elimObservationalEquivalence (pList', inpList, args) in
            runRecCheck (pList'', ioList, inpList, args)
        Just p -> p

synth :: ([([String], V)], [String]) -> E
synth (ioList, args) = 
    runRecCheck (terminals (ioList, args), ioList, map (\(x, _) -> x) ioList, args)

-- test the DSL Interface (for writing generics)
{- data DSLInterface = DSLInterface {

    exprToSyntax
    getTerminals
    getOperations
    eval
} -}