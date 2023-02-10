OS=$(shell uname -s)

ifeq (${OS},Darwin)
  ELD=--extra-lib-dirs=/Volumes/HC/.sync/.esync/package-manager/nix/haskell-calling-rust/aisamanra/rust/target/debug
else ifeq (${OS},Linux)
  ELD=--extra-lib-dirs=/Volumes/HC/.sync/.esync/package-manager/nix/haskell-calling-rust/aisamanra/rust/target/debug
endif

clean:
	hpack
	cabal clean

build:
	echo ELD ${ELD}
	hpack
	cabal ${ELD} build

run:
	echo ELD ${ELD}
	cabal ${ELD} run main
