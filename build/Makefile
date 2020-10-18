COMPILER = fpc
UNITS = -Fu../sources -Fu./fpTOML -Fu./fpTOML/sources
OUTPUT = ./output
EXECUTABLE = ./FPM
PROGRAM = ../sources/FPM.pas
OPTIONS = -vbr -Xs -O2 -o${EXECUTABLE} -XR/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk

all: build

build:
	git clone https://github.com/genericptr/fpTOML.git
	mkdir -p ${OUTPUT}
	${COMPILER} ${OPTIONS} ${UNITS} -FU${OUTPUT} ${PROGRAM}

clean:
	rm -r ${OUTPUT}
	rm ${EXECUTABLE}
	rm -r ./fpTOML

install:
	cp -i ${EXECUTABLE} /usr/local/bin/fpm