       IDENTIFICATION DIVISION.
       PROGRAM-ID. BatchProcessor.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BATCH-FILE ASSIGN TO 'batch_transactions.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO 'batch_report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BATCH-FILE.
       01  BATCH-RECORD.
           05  TXN-TYPE          PIC X(6).
           05  TXN-AMOUNT        PIC 9(6)V99.
           05  TXN-ID            PIC X(10).

       FD  REPORT-FILE.
       01  REPORT-LINE           PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE 'N'.
       01  WS-RECORD-COUNT       PIC 9(5) VALUE 0.
       01  WS-SUCCESS-COUNT      PIC 9(5) VALUE 0.
       01  WS-ERROR-COUNT        PIC 9(5) VALUE 0.
       01  WS-TOTAL-CREDITS      PIC 9(8)V99 VALUE 0.
       01  WS-TOTAL-DEBITS       PIC 9(8)V99 VALUE 0.
       01  WS-OPERATION-TYPE     PIC X(6).
       01  WS-AMOUNT             PIC 9(6)V99.
       01  WS-BALANCE            PIC 9(6)V99.
       
       01  REPORT-HEADER.
           05  FILLER            PIC X(80) VALUE ALL '='.
       01  REPORT-TITLE.
           05  FILLER            PIC X(25) VALUE SPACES.
           05  FILLER            PIC X(30) 
               VALUE 'BATCH PROCESSING REPORT'.
           05  FILLER            PIC X(25) VALUE SPACES.
       01  REPORT-COLUMNS.
           05  FILLER            PIC X(12) VALUE 'TXN ID'.
           05  FILLER            PIC X(10) VALUE 'TYPE'.
           05  FILLER            PIC X(15) VALUE 'AMOUNT'.
           05  FILLER            PIC X(10) VALUE 'STATUS'.
           05  FILLER            PIC X(33) VALUE SPACES.
       
       01  DETAIL-LINE.
           05  DL-TXN-ID         PIC X(12).
           05  DL-TYPE           PIC X(10).
           05  DL-AMOUNT         PIC ZZZ,ZZ9.99.
           05  FILLER            PIC X VALUE SPACE.
           05  DL-STATUS         PIC X(10).
           05  FILLER            PIC X(32) VALUE SPACES.
       
       01  SUMMARY-LINE.
           05  SL-LABEL          PIC X(30).
           05  SL-VALUE          PIC ZZZ,ZZ9.99.
           05  FILLER            PIC X(35) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-BATCH
           PERFORM PROCESS-TRANSACTIONS
           PERFORM GENERATE-SUMMARY
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZE-BATCH.
           OPEN INPUT BATCH-FILE
           OPEN OUTPUT REPORT-FILE
           
           WRITE REPORT-LINE FROM REPORT-HEADER
           WRITE REPORT-LINE FROM REPORT-TITLE
           WRITE REPORT-LINE FROM REPORT-HEADER
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM REPORT-COLUMNS
           WRITE REPORT-LINE FROM REPORT-HEADER
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           DISPLAY "Batch processing started..."
           DISPLAY " ".

       PROCESS-TRANSACTIONS.
           PERFORM UNTIL WS-EOF = 'Y'
               READ BATCH-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-SINGLE-TRANSACTION
               END-READ
           END-PERFORM.

       PROCESS-SINGLE-TRANSACTION.
           ADD 1 TO WS-RECORD-COUNT
           MOVE TXN-TYPE TO WS-OPERATION-TYPE
           MOVE TXN-AMOUNT TO WS-AMOUNT
           
           MOVE TXN-ID TO DL-TXN-ID
           MOVE TXN-TYPE TO DL-TYPE
           MOVE TXN-AMOUNT TO DL-AMOUNT
           
           EVALUATE WS-OPERATION-TYPE
               WHEN 'CREDIT'
                   PERFORM PROCESS-CREDIT
                   MOVE 'SUCCESS' TO DL-STATUS
                   ADD 1 TO WS-SUCCESS-COUNT
                   ADD WS-AMOUNT TO WS-TOTAL-CREDITS
                   
               WHEN 'DEBIT '
                   PERFORM PROCESS-DEBIT
                   
               WHEN 'TOTAL '
                   PERFORM PROCESS-INQUIRY
                   MOVE 'SUCCESS' TO DL-STATUS
                   ADD 1 TO WS-SUCCESS-COUNT
                   
               WHEN OTHER
                   MOVE 'ERROR' TO DL-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY "Error: Invalid transaction type - " 
                           TXN-TYPE " for " TXN-ID
           END-EVALUATE
           
           WRITE REPORT-LINE FROM DETAIL-LINE.

       PROCESS-CREDIT.
           CALL 'DataProgram' USING 'READ', WS-BALANCE
           ADD WS-AMOUNT TO WS-BALANCE
           CALL 'DataProgram' USING 'WRITE', WS-BALANCE
           DISPLAY "Processed: " TXN-ID " - CREDIT " 
                   WS-AMOUNT " - New Balance: " WS-BALANCE.

       PROCESS-DEBIT.
           CALL 'DataProgram' USING 'READ', WS-BALANCE
           IF WS-BALANCE >= WS-AMOUNT
               SUBTRACT WS-AMOUNT FROM WS-BALANCE
               CALL 'DataProgram' USING 'WRITE', WS-BALANCE
               MOVE 'SUCCESS' TO DL-STATUS
               ADD 1 TO WS-SUCCESS-COUNT
               ADD WS-AMOUNT TO WS-TOTAL-DEBITS
               DISPLAY "Processed: " TXN-ID " - DEBIT " 
                       WS-AMOUNT " - New Balance: " WS-BALANCE
           ELSE
               MOVE 'INSUF-FND' TO DL-STATUS
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY "Error: " TXN-ID " - Insufficient funds"
           END-IF.

       PROCESS-INQUIRY.
           CALL 'DataProgram' USING 'READ', WS-BALANCE
           MOVE WS-BALANCE TO DL-AMOUNT
           DISPLAY "Processed: " TXN-ID " - INQUIRY - Balance: " 
                   WS-BALANCE.

       GENERATE-SUMMARY.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM REPORT-HEADER
           MOVE 'BATCH PROCESSING SUMMARY' TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM REPORT-HEADER
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Transactions:' TO SL-LABEL
           MOVE WS-RECORD-COUNT TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Successful:' TO SL-LABEL
           MOVE WS-SUCCESS-COUNT TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Errors:' TO SL-LABEL
           MOVE WS-ERROR-COUNT TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Credits:' TO SL-LABEL
           MOVE WS-TOTAL-CREDITS TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Total Debits:' TO SL-LABEL
           MOVE WS-TOTAL-DEBITS TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           CALL 'DataProgram' USING 'READ', WS-BALANCE
           MOVE 'Final Balance:' TO SL-LABEL
           MOVE WS-BALANCE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           DISPLAY " "
           DISPLAY "Batch processing completed!"
           DISPLAY "Total transactions: " WS-RECORD-COUNT
           DISPLAY "Successful: " WS-SUCCESS-COUNT
           DISPLAY "Errors: " WS-ERROR-COUNT
           DISPLAY "Report generated: batch_report.txt".

       CLEANUP.
           CLOSE BATCH-FILE
           CLOSE REPORT-FILE.
