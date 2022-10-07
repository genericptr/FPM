COMPILER = fpc
UNITS = -Fu../sources -Fu./fpTOML -Fu./fpTOML/sources
OUTPUT = ./output
EXECUTABLE = ./FPM
PROGRAM = ../sources/FPM.pas
SDK_PATH = `xcrun --show-sdk-path`
HIDE_WARNINGS = -vm6058 # hide "not inlined" warnings
OPTIONS = -vbr -Xs -O2 ${HIDE_WARNINGS} -o${EXECUTABLE} -XR${SDK_PATH}

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