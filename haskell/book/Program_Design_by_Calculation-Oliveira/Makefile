#
# Created       : 2014 Jul 20 (Sun) 10:44:43 by Harold Carr.
# Last Modified : 2014 Jul 22 (Tue) 18:53:53 by Harold Carr.
#

# http://tex.stackexchange.com/questions/11866/compile-a-latex-document-into-a-png-image-thats-as-short-as-possible/11880#11880
# http://www.felixl.de/commu.pdf

PNG_FILES = constant-fusion.png \
            e2-1a.png \
            e2-1b.png \
            either.png \
            function-composition.png \
	    function-composition-id-is-unit.png \
	    pair.png \
	    product-fusion.png \
	    product.png \
            test.png \
            times-absorption.png \
            times-reflexion.png

%.png : %.tex
	latex -shell-escape $?
	bn=$(basename $?); rm $$bn.aux $$bn.dvi $$bn.log $$bn.ps

all : ${PNG_FILES}

clean :
	rm pdbc.hs pdbc.html pdbc.pdf pdbc.tex

# End of file.
