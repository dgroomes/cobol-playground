# hello-world

A "hello world"-style COBOL program that demonstrates essential COBOL syntax and structure.


## Overview

This subproject showcases fundamental COBOL language features including:

* The four-division structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
* COBOL's column-based formatting requirements
* Variable declarations and data types
* Basic I/O operations with DISPLAY
* String manipulation with MOVE statements
* Arithmetic operations
* Conditional logic with IF statements


## Instructions

Follow these instructions to build and run the example program.

1. Compile the COBOL program
   * ```shell
     cobc -x hello.cob
     ```
   * This generates an executable named `hello`
2. Run the program
   * ```shell
     ./hello
     ```
   * You should see output like:
   * ```text
     Hello, COBOL World! 
      
     Today's message: COBOL LIVES ON!
      
     Welcome, COBOL Learner       !
      
     Fun fact: COBOL has been around for                          066 years!
     That's older than most programming languages!
     ```


## Code Structure

The `hello.cob` file demonstrates:
- **IDENTIFICATION DIVISION**: Program metadata
- **ENVIRONMENT DIVISION**: System-specific configuration (empty in this example)
- **DATA DIVISION**: Variable declarations with COBOL's unique picture clauses
- **PROCEDURE DIVISION**: The actual program logic

Key COBOL concepts illustrated:
- Picture clauses (PIC) for defining data types and sizes
- Level numbers for data hierarchy
- MOVE statements for data assignment
- COMPUTE for arithmetic operations
- String handling with reference modification


## COBOL Column Rules

Traditional COBOL follows strict column rules:
- Columns 1-6: Sequence numbers (optional)
- Column 7: Indicator area (* for comments, - for continuation)
- Columns 8-11: Area A (division headers, section names, paragraph names, 01-level items)
- Columns 12-72: Area B (most COBOL statements)
- Columns 73-80: Identification area (ignored by compiler)

GnuCOBOL supports both traditional fixed-format and modern free-format source code.


## Reference

* [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/guides.html)
* [COBOL Language Reference](https://www.ibm.com/docs/en/cobol-zos)
