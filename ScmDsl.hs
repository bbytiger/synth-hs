module ScmDsl where 

    -- define the semantics
    data L = Cons E E | Car L | Cdr L | Append L L deriving (Show)
    data I = Times I I | Plus I I | Add1 I | Const Int deriving (Show)
    data F = Func String deriving (Show)
    data S = Const String deriving (Show)
    data B = Not B deriving (Show)
    data E = L L | I I | B B | S S deriving (Show)

