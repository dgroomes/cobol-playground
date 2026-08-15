# hello-world

A "hello world" COBOL program.


## Overview

This is a contrived COBOL progam. It doesn't move the needle in teaching COBOL concepts, and instead you should look at the excellent resources from IBM and GnuCOBOL:

- IBM's Enterprise COBOL docs, particularly the [*Programming Guide*][ibm-cobol-programming-guide]
- The [GnuCOBOL Guides][gnu-cobol-guides], particularly Gary and Vincent's *Programmer's Guide* and *Quick Reference* 

This subproject is a waypoint for me on my own COBOL learning path. Having a combination of instructions, doc references, my own notes, and a runnable COBOL program is a useful first milestone.


## Instructions

Follow these instructions to install GnuCOBOL, and build and run the example program. I'm on macOS and I used Homebrew to install GnuCOBOL.

1. Install GnuCOBOL with Homebrew
   - ```shell
     brew install gnucobol
     ```
2. Verify the installation
   - ```shell
     cobc --version
     ```
   - You should see output like the following.
   - ```text
     cobc (GnuCOBOL) 3.2.0
     ```
3. Compile the COBOL program
   - ```shell
     cobc -x hello.cob
     ```
   - This generates an executable named `hello`
4. Run the program
   - ```shell
     ./hello
     ```
   - You should see output like the following.
   * ```text
     COBOL says hello!
     ```


## Wish List

General clean-ups, TODOs and things I wish to implement for this project:

- [x] DONE Clean up the AI output and use my own voice.
  - DONE Clean up README
  - DONE Cleanup hello.cob


## Reference

* [IBM COBOL: *Programming Guide*][ibm-cobol-programming-guide]
* [GnuCOBOL Guides][gnu-cobol-guides]

[ibm-cobol-programming-guide]: https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=programming-guide
[gnu-cobol-guides]: https://gnucobol.sourceforge.io/guides.html
