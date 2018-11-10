# Created       : 2013 Nov 23 (Sat) 09:55:30 by carr.
# Last Modified : 2013 Dec 03 (Tue) 16:49:39 by carr.

all:
	cabal update
	cabal install doctest
	cabal install QuickCheck
	cabal configure --enable-tests
	cabal build
	cabal test

# End of file.
