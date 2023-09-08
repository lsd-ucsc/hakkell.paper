run: build
	./Node +RTS --info | grep "RTS way"
	./Node 5

build:
	ghc -O -threaded -rtsopts -with-rtsopts=-N Node.hs

clean:
	rm -v *.{hi,o} Node
