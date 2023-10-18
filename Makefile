installhspec:
	cabal update && cabal install --package-env=. --lib hspec hspec-contrib 

test:
	runhaskell test/Main.hs