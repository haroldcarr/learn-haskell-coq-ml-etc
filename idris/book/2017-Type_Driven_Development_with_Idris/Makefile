AGDA_SRC_DIR=src
GEN_DIR=src-generated

compile: compile-haskell

compile-agda:
	(pushd $(AGDA_SRC_DIR); \
	agda --compile --ghc-dont-call-ghc \
             --compile-dir=../$(GEN_DIR) \
             P139_read_vect.agda; \
	agda --compile --ghc-dont-call-ghc \
             --compile-dir=../$(GEN_DIR) \
             P236-257-predicates-hangman.agda; \
	popd)

compile-haskell: compile-agda
	stack build

p139:
	stack exec m -- P139

p236:
	stack exec m -- P236

clean:
	rm src/*.ibc
