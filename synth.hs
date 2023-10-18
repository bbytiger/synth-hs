module Synth where 
    import Data.List
    import qualified Data.Set as Set
    import qualified Data.Map as Map

    import Types

    -- grow program list 
    handleOpCombinations :: (DSLExprUtils e, DSLOpTypeMap op t, Ord t) => (op, [t], Map.Map t [e], [[e]]) -> [[e]]
    handleOpCombinations (_, [], _, acc) = acc
    handleOpCombinations (op, thd:ttl, eMapByType, acc) = 
        case Map.lookup thd eMapByType of
            Just eTypelist -> handleOpCombinations (op, ttl, eMapByType, concatMap (\x -> map (\y -> x ++ [y]) eTypelist) acc)
            Nothing -> []

    growProgramList :: (Ord t, Ord op, DSLExprUtils e, DSLTypeList t, DSLOpList op, DSLOpTypeMap op t, DSLTypeCheck e t, DSLOpConversion e op) => ([e], [t], [op], Map.Map op [t]) -> [e]
    growProgramList ([],_,_,_) = []
    growProgramList (elist,tlist,oplist,optypmap) = 
        let slist = map (\t -> (t, filter (\x -> isExprofType (x, t)) elist)) tlist in
        let eMapByType = Map.fromList slist in
        elist ++ 
        concatMap (\op -> 
            case Map.lookup op optypmap of 
                Just tlist -> let generatedCombinations = handleOpCombinations (op, tlist, eMapByType, [[]]) in 
                            map (\c -> convertOpExprListToExpr (op, c)) generatedCombinations
                Nothing -> []) oplist

    -- check if expression is correct
    evalExprCorrectness :: DSLExprUtils a => (a, [([V], V)], [String]) -> Bool
    evalExprCorrectness (expr, ioList, args) = 
        foldl (\x y -> x && y) True
        (map
            (\(inputs, output) -> 
                let mp' = Map.fromList (zip args inputs) in
                eval (expr, mp') == output)
        ioList)

    evalonIoList :: DSLExprUtils a => (a, [[V]], [String]) -> [V]
    evalonIoList (expr, ioList, args) = 
        map 
        (\inp -> 
            let mp' = Map.fromList (zip args inp) in
            eval (expr, mp'))
        ioList

    -- eliminate eval equivalents
    elimObservationalEquivalence :: DSLExprUtils a => ([a], [[V]], [String], Set.Set [V]) -> [a]
    elimObservationalEquivalence ([], _, _, _) = []
    elimObservationalEquivalence (ehd:etl, inputList, args, valStore) = 
        let hdRes = evalonIoList (ehd, inputList, args) in 
        if Set.member hdRes valStore then elimObservationalEquivalence (etl, inputList, args, valStore)
        else ehd : elimObservationalEquivalence (etl, inputList, args, Set.insert hdRes valStore)

    -- the overall algorithm
    checkPList :: DSLExprUtils a => ([a], [([V], V)], [String]) -> Maybe a
    checkPList ([], _, _) = Nothing
    checkPList (p:plist', ioList, args) = 
        if evalExprCorrectness (p, ioList, args) then Just p 
        else checkPList (plist', ioList, args)

    runRecCheck :: (Ord t, Ord op, Ord e, DSLExprUtils e, DSLTypeList t, DSLOpList op, DSLOpTypeMap op t, DSLTypeCheck e t, DSLOpConversion e op) => ([e], [([V], V)], [[V]], [String], [t], [op], Map.Map op [t]) -> e
    runRecCheck (pList, ioList, inpList, args, tlist, oplist, optypmap) = 
        case checkPList (pList, ioList, args) of 
            Nothing -> 
                let sortedPList = sort pList in
                let pList' = growProgramList (sortedPList, tlist, oplist, optypmap) in
                let pList'' = elimObservationalEquivalence (pList', inpList, args, Set.empty) in
                runRecCheck (pList'', ioList, inpList, args, tlist, oplist, optypmap)
            Just p -> p

    synth :: (Ord t, Ord op, Ord e, DSLExprUtils e, DSLTypeList t, DSLOpList op, DSLOpTypeMap op t, DSLTypeCheck e t, DSLOpConversion e op) => ([([V], V)], [String], e, op, t) -> e
    synth (ioList, args, e, op, t) = 
        let terminals = getTerminals (ioList, args, e) in
        let inpList = map fst ioList in
        let tlist = getTypeList t in 
        let oplist = getOpList op in 
        let optypmap = getMap (op, t) in
        runRecCheck (terminals, ioList, inpList, args, tlist, oplist, optypmap)