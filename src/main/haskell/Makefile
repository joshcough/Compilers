all: compile

compile:
	make clean
	ghc L1L2AST.hs L1L2Parser.hs L1.hs L2AST.hs Read.hs Testhelpers.hs

clean:
	rm -f *.o *.hi *.exe rm L1 L1Tests

test:
	make
	./run-haskell-L1.sh
	mv L1TestFiles/*.S L1TestFiles/haskell-results
	diff L1TestFiles/haskell-results L1TestFiles/scala-results
