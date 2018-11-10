# Created       : 2013 Oct 03 (Thu) 15:02:11 by carr.
# Last Modified : 2014 Mar 06 (Thu) 14:06:25 by Harold Carr.

H_FILES = \
	FP00ListsTest.o \
	FP01RecFunTest.o \
	FP02FunSetsTest.o \
	FP02FunSetsWorksheet.o \
	FP03ObjSetsTweetSetTest.o \
	FP04PatMatTest.o \
	FP06ForCompTest.o \
	FP07BloxorzTest.o \
        FRP01QuickCheck.o

test : $(H_FILES)

# TODO: fake dependency - I never compile these
%.o : %.hs
	runghc $<

hlint :
	hlint .

clean :
	rm *.o *.hi

# End of file.
