      * This is a rough first draft by an LLM and is not designed to be immediately usable
      * hello.cob - A comprehensive COBOL hello world program
      * Demonstrates fundamental COBOL syntax and structure
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. COBOL-LEARNER.
       DATE-WRITTEN. 2025-07-03.
       
      * The ENVIRONMENT DIVISION is optional for simple programs
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Level numbers organize data hierarchically
      * 01 is the highest level (like a record or structure)
       01 WS-MESSAGE-AREA.
          05 WS-GREETING        PIC X(20) VALUE "Hello, COBOL World!".
          05 WS-SECONDARY-MSG   PIC X(15) VALUE "COBOL LIVES ON!".
          
       01 WS-USER-DATA.
          05 WS-USER-NAME       PIC X(20).
          05 WS-USER-INPUT      PIC X(20).
          
       01 WS-CALCULATIONS.
          05 WS-CURRENT-YEAR    PIC 9(4) VALUE 2025.
          05 WS-COBOL-BIRTH     PIC 9(4) VALUE 1959.
          05 WS-COBOL-AGE       PIC 9(3).
          
       01 WS-DISPLAY-ITEMS.
          05 WS-FUN-FACT        PIC X(50).
          05 WS-AGE-MESSAGE     PIC X(50).
          
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      * Display basic greeting
           DISPLAY WS-GREETING.
           DISPLAY " ".
           
      * Demonstrate MOVE statement
           DISPLAY "Today's message: " WS-SECONDARY-MSG.
           DISPLAY " ".
           
      * MOVE demonstrates data transfer
           MOVE "COBOL Learner" TO WS-USER-NAME.
           DISPLAY "Welcome, " WS-USER-NAME "!".
           DISPLAY " ".
           
      * Arithmetic with COMPUTE
           COMPUTE WS-COBOL-AGE = WS-CURRENT-YEAR - WS-COBOL-BIRTH.
           
      * String manipulation with MOVE and literal
           MOVE "COBOL has been around for" TO WS-FUN-FACT.
           DISPLAY "Fun fact: " WS-FUN-FACT SPACE WS-COBOL-AGE " years!".
           
      * Conditional logic
           IF WS-COBOL-AGE > 50
               DISPLAY "That's older than most programming languages!"
           ELSE
               DISPLAY "Still going strong!"
           END-IF.
           
      * Program termination
           STOP RUN.
