COMPILER = fpc
UNITS = -Fu..\sources -Fu.\fpTOML -Fu.\fpTOML\sources
OUTPUT = .\output
EXECUTABLE = .\FPM.exe
PROGRAM = ..\sources\FPM.pas
HIDE_WARNINGS = -vm6058 # hide "not inlined" warnings
OPTIONS = -vbr -Xs -O2 ${HIDE_WARNINGS} -o${EXECUTABLE}

all: build

build:
#   git clone https://github.com/genericptr/fpTOML.git
# 	mkdir ${OUTPUT}
		${COMPILER} ${OPTIONS} ${UNITS} -FU${OUTPUT} ${PROGRAM}

# clean:
#   rm -r ${OUTPUT}
#   rm ${EXECUTABLE}
#   rm -r ./fpTOML

# install:
#   cp -i ${EXECUTABLE} /usr/local/bin/fpm