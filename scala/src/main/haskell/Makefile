all: 
	make clean
	make L1
	make L2

L1: 
	ghc L1L2AST.hs L1L2Parser.hs Read.hs Testhelpers.hs L1.hs

L2: 
	ghc L1L2AST.hs L1L2Parser.hs Read.hs Testhelpers.hs L2.hs

clean:
	rm -f *.o *.hi *.exe rm L1 L2 L1Tests

test:
	make
	./run-haskell-L1.sh
	mv L1TestFiles/*.S L1TestFiles/haskell-results
	diff L1TestFiles/haskell-results L1TestFiles/scala-results
