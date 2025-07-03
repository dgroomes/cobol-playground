# cobol-playground

**NOTE**: This is AI sloppy (although very much appreciated). I need to edit it down.

📚 Learning and exploring COBOL with GnuCOBOL.


## Overview

COBOL (Common Business-Oriented Language) is a programming language designed for business data processing needs. Despite being created in 1959, it remains widely used in financial institutions and government systems. This playground explores COBOL using GnuCOBOL, an open-source COBOL compiler that translates COBOL code to C before compiling to native machine code.

This repository demonstrates fundamental COBOL concepts through practical examples, focusing on the language's unique syntax and structure.

**NOTE**: This project was developed on macOS. It is for my own personal use.


## Standalone subprojects

This repository illustrates different concepts, patterns and examples via standalone subprojects. Each subproject is completely independent of the others and do not depend on the root project. This _standalone subproject constraint_ forces the subprojects to be complete and maximizes the reader's chances of successfully running, understanding, and re-using the code.

The subprojects include:


### `hello-world/`

A "hello world"-style COBOL program that demonstrates essential COBOL syntax and structure.

See the README in [hello-world/](hello-world/).


### `database/`

TODO: Explore database connectivity options with GnuCOBOL.


### `ebcdic/` 

TODO: Investigate EBCDIC file handling capabilities.


## Instructions

Follow these instructions to get started with GnuCOBOL on macOS.

1. Pre-requisite: Homebrew
   * GnuCOBOL can be installed via Homebrew on macOS
2. Install GnuCOBOL
   * ```shell
     brew install gnucobol
     ```
3. Verify installation
   * ```shell
     cobc --version
     ```
   * You should see output like:
   * ```text
     cobc (GnuCOBOL) 3.2.0
     ...
     ```


## C Toolchain Requirements

GnuCOBOL translates COBOL source code to C, then uses the system's C compiler to produce executables. On macOS, this typically uses the Clang compiler that comes with Xcode Command Line Tools. If you have Homebrew installed, you likely already have the necessary C toolchain. Otherwise, install it with:

```shell
xcode-select --install
```


## Wish List

General clean-ups, TODOs and things I wish to implement for this project:

* [ ] Explore database connectivity options (official vs third-party solutions)
* [ ] Investigate EBCDIC file handling capabilities
* [ ] Learn about COBOL's file handling features (sequential, indexed, relative)
* [ ] Understand COBOL's decimal arithmetic and its advantages for financial calculations
* [ ] Explore calling C functions from COBOL programs
* [ ] IN PROGRESS (AI drafted; need to prune it down) Create comprehensive hello-world example with full COBOL boilerplate


## Reference

* [Wikipedia: *GnuCOBOL*][gnucobol-wiki]
* [GnuCOBOL Official Site][gnucobol-official]
* [GnuCOBOL Programmer's Guide][gnucobol-guide]
* [COBOL Language Reference][cobol-ref]

[gnucobol-wiki]: https://en.wikipedia.org/wiki/GnuCOBOL
[gnucobol-official]: https://www.gnu.org/software/gnucobol/
[gnucobol-guide]: https://gnucobol.sourceforge.io/guides.html
[cobol-ref]: https://www.ibm.com/docs/en/cobol-zos
