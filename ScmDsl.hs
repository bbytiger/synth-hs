module ScmDsl where 

    import qualified Data.Set as Set
    import qualified Data.Map as Map

    import Types

    -- define the semantics
    data L = Cons E L | Append L L | ConstL [V] | InputList String deriving (Show, Eq, Ord)
    data I = Times I I | Plus I I | Add1 I | ConstInt Int | InputInt String deriving (Show, Eq, Ord)
    data S = ConstStr String | InputStr String deriving (Show, Eq, Ord)
    data B = Not B | ConstBool Bool | InputBool String deriving (Show, Eq, Ord)
    data E = Car L | Cdr L | L L | I I | B B | S S | Placeholder deriving (Show, Eq, Ord)

    -- more complicated operation types
    data Op = ConsLOp | ConsSOp | ConsIOp | ConsBOp | CarOp | CdrOp | AppendOp | TimesOp | PlusOp | Add1Op | NotOp deriving (Show, Eq, Ord)
    data Type = El | Es | Ei | Eb deriving (Show, Eq, Ord)
    opList :: Op -> [Op] 
    opList _ = [ConsLOp, ConsSOp, ConsIOp, ConsBOp, CarOp, CdrOp, AppendOp, TimesOp, PlusOp, Add1Op, NotOp]
    typeList :: Type -> [Type] 
    typeList _ = [El, Es, Ei, Eb]
    opToTypeMap :: (Op, Type) -> Map.Map Op [Type] 
    opToTypeMap (_, _) = Map.fromList [
        (ConsLOp, [El, El]), 
        (ConsSOp, [Es, El]),
        (ConsIOp, [Ei, El]), 
        (ConsBOp, [Eb, El]),
        (CarOp, [El]),
        (CdrOp, [El]),
        (AppendOp, [El, El]),
        (TimesOp, [Ei, Ei]),
        (PlusOp, [Ei, Ei]),
        (Add1Op, [Ei]),
        (NotOp, [Eb])]

    -- convert regular to terminal type 
    convertVToEList :: (V, E) -> [E]
    convertVToEList (v,_) = 
        case v of 
            Vs s -> map (\ch -> S (ConstStr [ch])) s
            Vi i -> [I (ConstInt i)]
            Vb b -> [B (ConstBool b)]
            Vl l -> L (ConstL l) : concatMap (\v -> convertVToEList (v, Placeholder)) l
            _ -> []

    -- return all terminal
    scmTerminals :: ([([V], V)], [String], E) -> [E]
    scmTerminals (ioList, argList, _) = 
        let baseBools :: [Bool] = [True, False] in
        let baseChars :: [String] = ["", " ", ",", ".", "!", "?", "(", ")", "[", "]", "<", ">", "{", "}", "-", "+", "_", "/", "$", "#", ":", ";", "@", "%", "0"] in
        let baseInts :: [Int] = [0, 1, 2, 3, 99] in
        let inputTermList :: [E] = concatMap (\(vlist,v) -> convertVToEList (v, Placeholder) ++ concatMap (\x -> convertVToEList (x, Placeholder)) vlist) ioList in
        let zippedArgList :: [(String, V)] = zip argList (head (map fst ioList)) in
        let argInputList :: [E] = map
                                    (\(argName, argV)
                                            -> case argV of
                                                Vi _ -> I (InputInt argName)
                                                Vs _ -> S (InputStr argName)
                                                Vb _ -> B (InputBool argName)
                                                Vl _ -> L (InputList argName)
                                                _ -> Placeholder)
                                     zippedArgList in
        -- return all terminals
        let summedTerminals :: [E] = argInputList ++ map (B . ConstBool) baseBools ++ map (S . ConstStr) baseChars ++ map (I . ConstInt) baseInts ++ inputTermList in 
        Set.toList (Set.fromList summedTerminals)

    -- eval function
    evalScm :: (E, Map.Map String V) -> V
    evalScm (expr, hm) = 
        case expr of
            Car l -> 
                let (Vl l1eval) = evalScm (L l, hm) in
                head l1eval
            Cdr l -> 
                let (Vl l1eval) = evalScm (L l, hm) in
                last l1eval
            L l ->
                case l of
                    Cons e l1 -> 
                        let (Vl l1eval) = evalScm (L l1, hm) in
                        Vl (evalScm (e, hm) : l1eval)
                    Append l1 l2 -> 
                        let (Vl l1eval) = evalScm (L l1, hm) in
                        let (Vl l2eval) = evalScm (L l2, hm) in
                        Vl (l1eval ++ l2eval)
                    ConstL constl -> 
                        Vl constl
                    InputList s -> 
                        case Map.lookup s hm of
                            Just v -> v
                            _ -> None
            I i -> 
                case i of 
                    Times i1 i2 -> 
                        let (Vi i1eval) = evalScm (I i1, hm) in
                        let (Vi i2eval) = evalScm (I i2, hm) in
                        Vi (i1eval * i2eval)
                    Plus i1 i2 -> 
                        let (Vi i1eval) = evalScm (I i1, hm) in
                        let (Vi i2eval) = evalScm (I i2, hm) in
                        Vi (i1eval + i2eval)
                    Add1 i ->     
                        let (Vi ieval) = evalScm (I i, hm) in Vi (ieval + 1)
                    ConstInt i -> 
                        Vi i
                    InputInt s -> 
                        case Map.lookup s hm of
                            Just v -> v
                            _ -> None
            B b -> 
                case b of 
                    Not b -> 
                        let (Vb bval) = evalScm (B b, hm) in
                        Vb (not bval)
                    ConstBool b -> 
                        Vb b
                    InputBool s -> 
                        case Map.lookup s hm of
                            Just v -> v
                            _ -> None
            S s -> 
                case s of 
                    ConstStr s1 ->
                        Vs s1
                    InputStr s1 ->
                        case Map.lookup s1 hm of
                            Just v -> v
                            _ -> None
            _ -> None

    handleUnwrapOp :: (Op, [E]) -> E
    handleUnwrapOp (op, elist) = 
        case op of
            ConsLOp -> case (head elist, last elist) of 
                (e, L y) -> L (Cons e y) 
                _ -> Placeholder
            ConsSOp -> case (head elist, last elist) of 
                (e, L y) -> L (Cons e y) 
                _ -> Placeholder
            ConsIOp -> case (head elist, last elist) of 
                (e, L y) -> L (Cons e y) 
                _ -> Placeholder
            ConsBOp -> case (head elist, last elist) of 
                (e, L y) -> L (Cons e y) 
                _ -> Placeholder
            CarOp -> case head elist of 
                L x -> Car x
                _ -> Placeholder
            CdrOp -> case head elist of 
                L x -> Cdr x
                _ -> Placeholder
            AppendOp -> case (head elist, last elist) of 
                (L x, L y) -> L (Append x y) 
                _ -> Placeholder
            TimesOp -> case (head elist, last elist) of 
                (I x, I y) -> I (Times x y) 
                _ -> Placeholder
            PlusOp -> case (head elist, last elist) of 
                (I x, I y) -> I (Times x y) 
                _ -> Placeholder
            Add1Op -> case head elist of 
                I x -> I (Add1 x) 
                _ -> Placeholder
            NotOp -> case head elist of 
                B x -> B (Not x) 
                _ -> Placeholder

    -- define typecheck
    isEofType :: (E, Type, Map.Map String V) -> Bool
    isEofType (e,t,hm) = 
        case (evalScm (e, hm), t) of 
            (Vs _, Es) -> True; (Vi _, Ei) -> True; (Vb _, Eb) -> True; (Vl _, El) -> True; _ -> False

    instance DSLExprUtils E where
        getTerminals = scmTerminals
        eval = evalScm

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