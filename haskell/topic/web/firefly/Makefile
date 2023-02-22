.PHONY: build test run clean

build:
	hpack
	cabal build

test:
	hpack
	export PATH=/nix/store/bbvh6i08q7cv399nyg2j940rvy82c2ck-hspec-discover-2.8.5/bin:$$PATH; \
        cabal test

run:
	hpack
	cabal run main

clean:
	hpack
	cabal clean
