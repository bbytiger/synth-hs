import qualified Data.Set as Set
import qualified Data.Map as Map

import Types

-- check if expression is correct
evalExprCorrectness :: DSLInterface a => (a, [([V], V)], [String]) -> Bool
evalExprCorrectness (expr, ioList, args) = 
    foldl (\x y -> x && y) True
    (map
        (\(inputs, output) -> 
            let mp' = Map.fromList (zip args inputs) in
            eval (expr, mp') == output)
    ioList)

evalonIoList :: DSLInterface a => (a, [[V]], [String]) -> [V]
evalonIoList (expr, ioList, args) = 
    map 
    (\inp -> 
        let mp' = Map.fromList (zip args inp) in
        eval (expr, mp'))
    ioList

-- eliminate eval equivalents
elimObservationalEquivalence :: DSLInterface a => ([a], [[V]], [String], Set.Set [V]) -> [a]
elimObservationalEquivalence ([], _, _, _) = []
elimObservationalEquivalence ((ehd:etl), inputList, args, valStore) = 
    let hdRes = evalonIoList (ehd, inputList, args) in 
    if Set.member hdRes valStore then elimObservationalEquivalence (etl, inputList, args, valStore)
    else ehd : (elimObservationalEquivalence (etl, inputList, args, Set.insert hdRes valStore))

-- the overall algorithm
checkPList :: DSLInterface a => ([a], [([V], V)], [String]) -> Maybe a
checkPList ([], _, _) = Nothing
checkPList ((p:plist'), ioList, args) = 
    if evalExprCorrectness (p, ioList, args) then Just p 
    else checkPList (plist', ioList, args)

runRecCheck :: DSLInterface a => ([a], [([V], V)], [[V]], [String]) -> a
runRecCheck (pList, ioList, inpList, args) = 
    case checkPList (pList, ioList, args) of 
        Nothing -> 
            let pList' = growProgramList pList in
            let pList'' = elimObservationalEquivalence (pList', inpList, args, Set.empty) in
            runRecCheck (pList'', ioList, inpList, args)
        Just p -> p

synth :: DSLInterface a => ([([V], V)], [String], a) -> a
synth (ioList, args, t) = 
    runRecCheck (getTerminals (ioList, args, t), ioList, map (\(x, _) -> x) ioList, args)