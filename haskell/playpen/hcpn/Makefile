OS = $(shell uname -o)
ifeq (${OS},Cygwin)
  HCPNdir ?= $(shell cygpath -m ${CURDIR})
else
  HCPNdir ?= ${CURDIR}
endif

all: bin src/DefaultLocation.hs bin/ghciHCPN bin/NetEdit bin/SimpleHCPN.o bin/GuiHCPN.o

bin:
	mkdir bin

src/DefaultLocation.hs:
	echo ${OS}
	echo "module DefaultLocation where" > src/DefaultLocation.hs
	echo "defaultLocation = \"${HCPNdir}\"" >> src/DefaultLocation.hs

bin/ghciHCPN:
	echo ${OS}
	echo "#!/bin/sh" > bin/ghciHCPN
	echo "HCPNdir=${HCPNdir}" >> bin/ghciHCPN
	echo 'ghci -package wx -i$$HCPNdir/src -odir $$HCPNdir/bin -hidir $$HCPNdir/bin $$* 2>&1 | tee log.txt' >> bin/ghciHCPN

bin/NetEdit: src/*.hs
	ghc --make -package wx -isrc -odir bin -hidir bin -o bin/NetEdit Main

bin/SimpleHCPN.o: src/SimpleHCPN.hs 
	ghc --make -package wx -isrc -odir bin -hidir bin SimpleHCPN

bin/GuiHCPN.o: src/GuiHCPN.hs 
	ghc --make -package wx -isrc -odir bin -hidir bin GuiHCPN

archive: src/* examples/* Makefile
	cd ..;tar cvzf hcpn/HCPN_`date +%d%m%Y`.tar.gz --exclude='*.swp' hcpn/src hcpn/examples hcpn/Makefile hcpn/html hcpn/README
