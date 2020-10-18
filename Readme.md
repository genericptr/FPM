# FPM - Free Pascal Maker

Yet another FPC make system.

FPM uses [TOML](http://toml.io) config files to build an FPC command line. You can use FPM with or instead of make or  batch files.

```toml
[variables]

  MY_UNITS = "./path/to"
  
[settings]

  configuration = "debug"

  unitPaths = [
    "${MY_UNITS}/untits"
  ]
  includePaths = []
  libraryPaths = []
  
  compiler = "/usr/local/lib/fpc/${LATEST}/${ARCH}"
  program = "./sources/FPM.pas"
  output = './output'
  executable = "./FPM"

  options = ["-vbr",
             "-WM10.10",
             "-XR${SDK}"
             ]

[configuration.debug]

  options = ["-gw", "-godwarfcpp", "-Sa"]

[configuration.release]

  options = ["-Xs", "-O2"]

```
Features:

- Variables using Bash syntax
- Targets
- Configurations

### How to install:

  `make all install` will install  `fpm` into `/usr/local/bin/fpm`

### Usage:
  
  Same as make files:
  Create a `fpconfig.toml` file in the directory of your project then `cd` to the directory and use `fpm`.

### ⛓ Dependencies:

 - FPC 3.3.1 (trunk) or higher
 - [fpTOML](https://github.com/genericptr/fpTOML)

### 🛠 TODO:

 - Packaging for Darwin (.app bundle)
 - iOS targets with Xcode and tools integration
 - Inheritance from external files and default config file location
