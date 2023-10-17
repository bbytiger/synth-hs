module Types where 
    import qualified Data.Map as Map

    -- define basic types
    data V = Vs String | Vi Int | Vb Bool | None deriving (Show, Eq, Ord)

    -- generic DSL interface
    class DSLInterface a where 
        -- exprToSyntax :: a -> String
        convertVToExprList :: (V, a) -> [a]
        getTerminals :: ([([V], V)], [String], a) -> [a]
        growProgramList :: [a] -> [a]
        eval :: (a, Map.Map String V) -> V