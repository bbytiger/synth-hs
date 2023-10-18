 module Types where 
    import qualified Data.Map as Map

    -- define basic types
    data V = Vs String | Vi Int | Vb Bool | Vl [V] | None deriving (Show, Eq, Ord)

    -- generic DSL interface
    {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

    class DSLExprUtils e where 
        getTerminals :: ([([V], V)], [String], e) -> [e]
        eval :: (e, Map.Map String V) -> V

    class DSLTypeList t where
        getTypeList :: t -> [t]
    
    class DSLOpList op where
        getOpList :: op -> [op]

    class DSLOpTypeMap op t where 
        getMap :: (op, t) -> Map.Map op [t]

    class DSLTypeCheck e t where 
        isExprofType :: (e, t) -> Bool

    class DSLOpConversion e op where
        convertOpExprListToExpr :: (op, [e]) -> e