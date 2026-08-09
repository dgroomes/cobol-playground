# cobol-playground

📚 Learning and exploring [COBOL][cobol-wiki] with [GnuCOBOL][gnucobol-wiki].


## Overview

I'd like to better learn COBOL. This repository is me doing that by example programs.

Most COBOL in the world is hosted on IBM mainframes:

- Compiled using [IBM's Enterprise COBOL compiler][ibm-cobol]
- Running under the [IBM z/OS][ibm-z-os-wiki] operating system
- Physicallly running on [IBM Z][ibm-z-wiki] mainframe hardware

That platform is mostly inaccessible to individual learners, like what I'm representing right now with this `cobol-playground`.
Instead, we can build and run COBOL on Linux/macOS using GnuCOBOL. That's what I'm using.

Note: I recommend [IBM Z Xplore][ibm-z-xplore] to learn IBM Z in earnest and on the real platform. It covers some COBOL, and a ton
of other content. 


## GnuCOBOL Installation

I'm on macOS and I followed these steps to install GnuCOBOL.

1. Pre-requisite: Homebrew
2. Install GnuCOBOL
   - ```shell
     brew install gnucobol
     ```
3. Verify the installation
   - ```shell
     cobc --version
     ```
   - You should see output like the following.
   - ```text
     cobc (GnuCOBOL) 3.2.0
     ```


## Standalone subprojects

This repository illustrates different concepts, patterns and examples via standalone subprojects. Each subproject is completely independent of the others and do not depend on the root project. This _standalone subproject constraint_ forces the subprojects to be complete and maximizes the reader's chances of successfully running, understanding, and re-using the code.

The subprojects include:


### `hello-world/`

A "hello world"-style COBOL program that demonstrates essential COBOL syntax and structure.

See the README in [hello-world/](hello-world/).


### `gix-sql/`

Exploring SQL support for GnuCOBOL with GixSQL.

See the README in [gix-sql/](gix-sql/).


## Wish List

General clean-ups, TODOs and things I wish to implement for this project:

- [x] DONE Re-write README for my own voice. Plan out wish list itmes.
- [ ] Explore calling C functions from a COBOL program
- [ ] (AI drafted; need to prune it down) Create comprehensive hello-world example with full COBOL boilerplate
- [x] DONE I want a [GixSQL](https://github.com/mridoni/gixsql) example
      - (Answer: yes) Can/should I try running in a container? I'm curious if building gixsql from source is best, and to do that I don't really want to try that on macOS. Plus containers + Claude Code is a good synergy already.
- [ ] More layouts. I want to better understand what struct-like things (a layout?) can be expressed via COBOL.


## Reference

- [GnuCOBOL Official Site][gnucobol-official]
- [Wikipedia: *COBOL*][cobol-wiki]
- [Wikipedia: *GnuCOBOL*][gnucobol-wiki]

[cobol-wiki]: https://en.wikipedia.org/wiki/COBOL
[ibm-cobol]: https://www.ibm.com/products/cobol-compiler-zos?utm_source=chatgpt.com
[ibm-z-os-wiki]: https://en.wikipedia.org/wiki/Z/OS
[ibm-z-xplore]: https://www.ibm.com/products/z/resources/zxplore
[ibm-z-wiki]: https://en.wikipedia.org/wiki/IBM_Z
[gnucobol-official]: https://www.gnu.org/software/gnucobol/
[gnucobol-wiki]: https://en.wikipedia.org/wiki/GnuCOBOL
