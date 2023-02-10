OS=$(shell uname -s)

ifeq (${OS},Darwin)
  ELD=--extra-lib-dirs=${HOME}/.sync/.esync/package-manager/nix/haskell-calling-rust/hc-template/rust/target/debug
else ifeq (${OS},Linux)
  ELD=--extra-lib-dirs=${HOME}/.sync/.esync/package-manager/nix/haskell-calling-rust/hc-template/rust/target/debug
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
	hpack
	cabal ${ELD} run main
