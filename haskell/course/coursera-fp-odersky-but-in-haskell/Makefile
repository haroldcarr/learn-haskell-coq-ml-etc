# Created       : 2013 Oct 03 (Thu) 15:02:11 by carr.
# Last Modified : 2013 Oct 30 (Wed) 13:35:30 by carr.

H_FILES = \
	X00ListsTest.o \
	X01RecFunTest.o \
	X02FunSetsTest.o \
	X02FunSetsWorksheet.o \
	X03ObjSetsTweetSetTest.o \
	X04PatMatTest.o \
	X06ForCompTest.o \
	X07BloxorzTest.o

test : $(H_FILES)

# TODO: fake dependency - I never compile these
%.o : %.hs
	runghc $<

hlint :
	hlint .

clean :
	rm *.o *.hi

# End of file.
