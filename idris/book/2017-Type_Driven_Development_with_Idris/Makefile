AGDA_SRC_DIR=src
GEN_DIR=src-generated

compile: compile-haskell

compile-agda:
	(pushd $(AGDA_SRC_DIR); \
	agda --compile --ghc-dont-call-ghc \
             --compile-dir=../$(GEN_DIR) \
             P139_read_vect.agda; \
	popd)

compile-haskell: compile-agda
	stack build

run:
	stack exec m

clean:
	rm src/*.ibc
