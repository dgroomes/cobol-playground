# gix-sql

---
**NOTICE**: This is still partly AI sloppy (although pretty close to what I want). I need to edit it down more. 

---

Exploring SQL support for GnuCOBOL with GixSQL.


## Overview

This subproject demonstrates how to use embedded SQL in COBOL programs through GixSQL, an open-source SQL preprocessor. The example uses SQLite as the database backend and showcases:

* Embedded SQL syntax with `EXEC SQL ... END-EXEC` blocks
* Database connections and disconnections
* Creating tables and inserting data
* Retrieving data with SELECT statements
* Host variable integration between COBOL and SQL
* Error handling with SQLCA (SQL Communication Area)

GixSQL acts as a preprocessor that transforms embedded SQL statements into standard COBOL CALL statements, allowing seamless database operations within COBOL programs.


## Instructions

Follow these instructions to build and run the example program on a Linux system.

1. Pre-requisite: Linux environment
2. Install dependencies
   * ```bash
     ./install-dependencies.sh
     ```
3. Build GixSQL from source
   * ```bash
     ./build-gixsql.sh
     ```
4. Run the example
   * ```bash
     ./run-example.sh
     ```
   * You should see output like:
   * ```text
     GixSQL SQLite Hello World Demo
     ==============================
     ✓ Connected to SQLite database
     ✓ Table created successfully
     ✓ Message inserted: Hello World from COBOL + GixSQL + SQLite!
     ✓ Retrieved from database at: 2024-XX-XX HH:MM:SS
     ✓ Database test passed - 1 row found
     Demo completed successfully!
     ```


## Instructions (Docker)

If you're not on a Linux system or prefer containerized execution:

1. Build the Docker image:
   * ```shell
     docker build --tag gix-sql-demo .
     ```
   * This builds GixSQL from source inside the container, which takes a few minutes the first time.
2. Run the example:
   * ```shell
     docker run --rm gix-sql-demo
     ```


## Wish List

General clean-ups, TODOs and things I wish to implement for this example:

* [x] DONE get it built and working (thank you LLM). Right now, I need to do another verification run of the Dockerfile-based flow.
* [ ] Demonstrate prepared statements
* [ ] Show cursor usage for result sets
* [ ] Rename to `.cob` instead of `.cbl`, that's more conventional for GnuCOBOL
* [ ] Don't use the "hello world" naming because I already have it in the other project. Think of another domain.


## Reference

* [GixSQL GitHub Repository](https://github.com/mridoni/gixsql)
* [SQLite Documentation](https://www.sqlite.org/docs.html)
* [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/guides.html)
