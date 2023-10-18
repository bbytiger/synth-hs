import Test.Hspec

import Synth
import Types
import StrDsl
import ScmDsl

spec :: Spec
spec =
       describe "synthTest" $ do
       -- for string DSL
              describe "strdsl" $ do
                     it "left" $ do 
                            synth ([([Vs "hello"], Vs "h"), ([Vs "world"], Vs "w")], ["x"], StrDsl.Placeholder, StrDsl.ConcatOp, StrDsl.Es) 
                            `shouldBe` StrDsl.S (StrDsl.LeftS (StrDsl.X (StrDsl.Input "x")) (StrDsl.Const 1))
                     it "right" $ do 
                            synth ([([Vs "hello"], Vs "o"), ([Vs "world"], Vs "d")], ["x"], StrDsl.Placeholder, StrDsl.ConcatOp, StrDsl.Es) 
                            `shouldBe` StrDsl.S (StrDsl.RightS (StrDsl.X (StrDsl.Input "x")) (StrDsl.Const 1))
                     it "concat" $ do 
                            synth ([([Vs "hello", Vs "you"], Vs "helloyou"), ([Vs "world", Vs "domination"], Vs "worlddomination")], ["x", "y"], StrDsl.Placeholder, StrDsl.ConcatOp, StrDsl.Es) 
                            `shouldBe` StrDsl.S (StrDsl.Concat (StrDsl.X (StrDsl.Input "x")) (StrDsl.X (StrDsl.Input "y")))
                     it "concat-complex" $ do 
                            synth ([([Vs "hello", Vs "you"], Vs "hello you"), ([Vs "world", Vs "domination"], Vs "world domination")], ["x", "y"], StrDsl.Placeholder, StrDsl.ConcatOp, StrDsl.Es) 
                            `shouldBe` StrDsl.S (StrDsl.Concat (StrDsl.Concat (StrDsl.X (StrDsl.Input "x")) (StrDsl.T (StrDsl.Term " "))) (StrDsl.X (StrDsl.Input "y")))
                     it "concat-left-right" $ do
                            synth ([([Vs "hello"], Vs "ho"), ([Vs "world"], Vs "wd")], ["x"], StrDsl.Placeholder, StrDsl.ConcatOp, StrDsl.Es) 
                            `shouldBe` StrDsl.S (StrDsl.Concat (StrDsl.LeftS (StrDsl.X (StrDsl.Input "x")) (StrDsl.Const 1)) (StrDsl.RightS (StrDsl.X (StrDsl.Input "x")) (StrDsl.Const 1)))
       -- for scheme subset
              describe "scmdsl" $ do
                     it "fib" $ do 
                            synth ([([Vi 0, Vi 1, Vi 2], Vi 1), 
                                   ([Vi 1, Vi 1, Vi 3], Vi 2), 
                                   ([Vi 1, Vi 2, Vi 4], Vi 3), 
                                   ([Vi 2, Vi 3, Vi 5], Vi 5),
                                   ([Vi 3, Vi 5, Vi 6], Vi 8),
                                   ([Vi 5, Vi 8, Vi 7], Vi 13)], ["?n-2","?rec","n"], ScmDsl.Placeholder, ScmDsl.ConsLOp, ScmDsl.El) 
                                   `shouldBe` ScmDsl.I (ScmDsl.Plus (ScmDsl.InputInt "?n-2") (ScmDsl.InputInt "?rec"))
                     it "even-only" $ do 
                            synth ([([Vl [Vi 2, Vi 4], Vl [Vi 1, Vi 2, Vi 3, Vi 4]], Vl [Vi 2, Vi 4]), 
                                   ([Vl [Vi 4], Vl [Vi 3, Vi 4]], Vl [Vi 4])], ["?rec","l"], ScmDsl.Placeholder, ScmDsl.ConsLOp, ScmDsl.El) 
                                   `shouldBe` ScmDsl.L (ScmDsl.InputList "?rec")

main :: IO ()
main = hspec spec