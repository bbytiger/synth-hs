import Test.Hspec

import Synth
import Types
import StrDsl

-- for string DSL
spec :: Spec
spec =
       describe "strdsl" $ do
              it "left" $ do 
                     (synth ([([Vs "hello"], Vs "h"), ([Vs "world"], Vs "w")], ["x"], StrDsl.Placeholder) `shouldBe` S (LeftS (X (Input "x")) (Const 1)))
              it "right" $ do 
                     (synth ([([Vs "hello"], Vs "o"), ([Vs "world"], Vs "d")], ["x"], StrDsl.Placeholder) `shouldBe` S (RightS (X (Input "x")) (Const 1)))
              it "concat" $ do 
                     (synth ([([Vs "hello", Vs "you"], Vs "helloyou"), ([Vs "world", Vs "domination"], Vs "worlddomination")], ["x", "y"], StrDsl.Placeholder) `shouldBe` S (Concat (X (Input "x")) (X (Input "y"))))
              it "concat-complex" $ do 
                     (synth ([([Vs "hello", Vs "you"], Vs "hello you"), ([Vs "world", Vs "domination"], Vs "world domination")], ["x", "y"], StrDsl.Placeholder) `shouldBe` S (Concat (X (Input "x")) (Concat (T (Term " ")) (X (Input "y")))))
              it "concat-left-right" $ do 
                     (synth ([([Vs "hello"], Vs "ho"), ([Vs "world"], Vs "wd")], ["x"], StrDsl.Placeholder) `shouldBe` S (Concat (LeftS (X (Input "x")) (Const 1)) (RightS (X (Input "x")) (Const 1))))

-- for scheme subset


main :: IO ()
main = hspec spec