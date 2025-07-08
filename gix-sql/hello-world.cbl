       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD-SQL.
       AUTHOR. GixSQL Example.
       
      *****************************************************************
      * This program demonstrates embedded SQL in COBOL using GixSQL  *
      * It connects to a SQLite database, creates a table, inserts    *
      * data, retrieves it, and verifies the operation succeeded.     *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LINUX.
       OBJECT-COMPUTER. LINUX.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * SQL Communication Area - provides SQL status information
           EXEC SQL 
               INCLUDE SQLCA 
           END-EXEC.
           
      * Database connection info
       01  WS-DATABASE-NAME        PIC X(50) VALUE 'hello.db'.
           
      * Host variables for SQL operations
      * These variables are used to exchange data between COBOL and SQL
       01  WS-MESSAGE-RECORD.
           05  WS-ID               PIC 9(4) VALUE 1.
           05  WS-MESSAGE          PIC X(50).
           05  WS-TIMESTAMP        PIC X(30).
           
       01  WS-ROW-COUNT            PIC 9(4) VALUE ZERO.
       01  WS-DISPLAY-COUNT        PIC Z,ZZ9.
       
      * Program status flags
       01  WS-PROGRAM-STATUS       PIC X VALUE 'G'.
           88  WS-PROGRAM-OK       VALUE 'G'.
           88  WS-PROGRAM-ERROR    VALUE 'E'.
           
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INITIALIZE
           PERFORM 200-CONNECT-DATABASE
           PERFORM 300-CREATE-TABLE
           PERFORM 400-INSERT-DATA
           PERFORM 500-RETRIEVE-DATA
           PERFORM 600-VERIFY-DATA
           PERFORM 900-CLEANUP
           STOP RUN.
           
       100-INITIALIZE.
           DISPLAY "GixSQL SQLite Hello World Demo"
           DISPLAY "=============================="
           MOVE "Hello World from COBOL + GixSQL + SQLite!" 
               TO WS-MESSAGE.
               
       200-CONNECT-DATABASE.
      * Connect to SQLite database
      * For SQLite, the database name is the file path
      * User/password are ignored but required by syntax
           EXEC SQL
               CONNECT TO :WS-DATABASE-NAME USER 'dummy' USING 'dummy'
           END-EXEC.
           
           IF SQLCODE = 0
               DISPLAY "✓ Connected to SQLite database"
           ELSE
               DISPLAY "✗ Connection failed. SQLCODE: " SQLCODE
               MOVE 'E' TO WS-PROGRAM-STATUS
               PERFORM 900-CLEANUP
           END-IF.
           
       300-CREATE-TABLE.
           IF WS-PROGRAM-OK
      * Create table if it doesn't exist
      * Note: SQLite automatically adds ROWID as primary key
               EXEC SQL
                   CREATE TABLE IF NOT EXISTS messages (
                       id INTEGER PRIMARY KEY,
                       message TEXT NOT NULL,
                       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                   )
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY "✓ Table created successfully"
               ELSE
                   DISPLAY "✗ Table creation failed. SQLCODE: " 
                           SQLCODE
                   MOVE 'E' TO WS-PROGRAM-STATUS
               END-IF
           END-IF.
           
       400-INSERT-DATA.
           IF WS-PROGRAM-OK
      * Insert our hello world message
      * Using host variables (preceded by colon in SQL)
               EXEC SQL
                   INSERT INTO messages (id, message)
                   VALUES (:WS-ID, :WS-MESSAGE)
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY "✓ Message inserted: " WS-MESSAGE
               ELSE
                   DISPLAY "✗ Insert failed. SQLCODE: " SQLCODE
                   MOVE 'E' TO WS-PROGRAM-STATUS
               END-IF
           END-IF.
           
       500-RETRIEVE-DATA.
           IF WS-PROGRAM-OK
      * Retrieve the message we just inserted
      * The datetime function is SQLite-specific
               EXEC SQL
                   SELECT message, 
                          datetime('now', 'localtime')
                   INTO :WS-MESSAGE, :WS-TIMESTAMP
                   FROM messages
                   WHERE id = :WS-ID
               END-EXEC
               
               IF SQLCODE = 0
                   DISPLAY "✓ Retrieved from database at: " 
                           WS-TIMESTAMP
               ELSE
                   DISPLAY "✗ Retrieval failed. SQLCODE: " SQLCODE
                   MOVE 'E' TO WS-PROGRAM-STATUS
               END-IF
           END-IF.
           
       600-VERIFY-DATA.
           IF WS-PROGRAM-OK
      * Count rows to verify our insert worked
               EXEC SQL
                   SELECT COUNT(*) 
                   INTO :WS-ROW-COUNT
                   FROM messages
               END-EXEC
               
               IF SQLCODE = 0
                   MOVE WS-ROW-COUNT TO WS-DISPLAY-COUNT
                   DISPLAY "✓ Database test passed - " 
                           WS-DISPLAY-COUNT " row found"
               ELSE
                   DISPLAY "✗ Verification failed. SQLCODE: " SQLCODE
                   MOVE 'E' TO WS-PROGRAM-STATUS
               END-IF
           END-IF.
           
       900-CLEANUP.
      * Always disconnect from the database
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
           
           IF WS-PROGRAM-OK
               DISPLAY "Demo completed successfully!"
           ELSE
               DISPLAY "Demo completed with errors."
               MOVE 1 TO RETURN-CODE
           END-IF.