AGDA_SRC_DIR=src-agda
GEN_DIR=src-generated

compile: compile-haskell

compile-agda:
	(pushd $(AGDA_SRC_DIR); \
	agda --compile --ghc-dont-call-ghc \
             --compile-dir=../$(GEN_DIR) \
             xxx.agda; \
	popd)

compile-haskell: compile-agda
	stack build

run:
	stack exec m




