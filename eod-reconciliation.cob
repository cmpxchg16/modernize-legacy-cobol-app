       IDENTIFICATION DIVISION.
       PROGRAM-ID. EODReconciliation.
       AUTHOR. Banking Operations Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO 'eod_transactions.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RECON-REPORT ASSIGN TO 'eod_reconciliation_report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-LOG ASSIGN TO 'eod_errors.log'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TXN-DATE          PIC X(8).
           05  TXN-TIME          PIC X(6).
           05  TXN-TYPE          PIC X(6).
           05  TXN-AMOUNT        PIC 9(8)V99.
           05  TXN-REF           PIC X(15).
           05  TXN-CHANNEL       PIC X(10).

       FD  RECON-REPORT.
       01  REPORT-LINE           PIC X(132).

       FD  ERROR-LOG.
       01  ERROR-LINE            PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE 'N'.
       01  WS-OPENING-BALANCE    PIC 9(10)V99 VALUE 1000.00.
       01  WS-CLOSING-BALANCE    PIC 9(10)V99.
       01  WS-CALCULATED-BALANCE PIC 9(10)V99.
       01  WS-DIFFERENCE         PIC S9(10)V99.
       
       01  WS-TRANSACTION-COUNT  PIC 9(7) VALUE 0.
       01  WS-CREDIT-COUNT       PIC 9(7) VALUE 0.
       01  WS-DEBIT-COUNT        PIC 9(7) VALUE 0.
       01  WS-ERROR-COUNT        PIC 9(5) VALUE 0.
       
       01  WS-TOTAL-CREDITS      PIC 9(12)V99 VALUE 0.
       01  WS-TOTAL-DEBITS       PIC 9(12)V99 VALUE 0.
       01  WS-LARGEST-CREDIT     PIC 9(10)V99 VALUE 0.
       01  WS-LARGEST-DEBIT      PIC 9(10)V99 VALUE 0.
       
       01  WS-CURRENT-DATE.
           05  WS-YEAR           PIC 9(4).
           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99.
       01  WS-CURRENT-TIME.
           05  WS-HOUR           PIC 99.
           05  WS-MINUTE         PIC 99.
           05  WS-SECOND         PIC 99.
       
       01  WS-FORMATTED-DATE     PIC X(10).
       01  WS-FORMATTED-TIME     PIC X(8).
       01  WS-RECON-STATUS       PIC X(20).
       
       01  HEADER-1.
           05  FILLER            PIC X(132) VALUE ALL '='.
       01  HEADER-2.
           05  FILLER            PIC X(40) VALUE SPACES.
           05  FILLER            PIC X(52) 
               VALUE 'END-OF-DAY RECONCILIATION REPORT'.
           05  FILLER            PIC X(40) VALUE SPACES.
       01  HEADER-3.
           05  FILLER            PIC X(15) VALUE 'Report Date: '.
           05  H3-DATE           PIC X(10).
           05  FILLER            PIC X(10) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Report Time: '.
           05  H3-TIME           PIC X(8).
           05  FILLER            PIC X(74) VALUE SPACES.
       
       01  SECTION-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  SH-TITLE          PIC X(50).
           05  FILLER            PIC X(77) VALUE SPACES.
       
       01  DETAIL-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  DL-LABEL          PIC X(40).
           05  DL-VALUE          PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER            PIC X(72) VALUE SPACES.
       
       01  COUNT-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  CL-LABEL          PIC X(40).
           05  CL-COUNT          PIC ZZZ,ZZ9.
           05  FILLER            PIC X(80) VALUE SPACES.
       
       01  STATUS-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(25) VALUE 'RECONCILIATION STATUS: '.
           05  SL-STATUS         PIC X(20).
           05  FILLER            PIC X(82) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-JOB
           PERFORM PROCESS-TRANSACTIONS
           PERFORM PERFORM-RECONCILIATION
           PERFORM GENERATE-REPORT
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZE-JOB.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
           STRING WS-MONTH '/' WS-DAY '/' WS-YEAR
               DELIMITED BY SIZE INTO WS-FORMATTED-DATE
           STRING WS-HOUR ':' WS-MINUTE ':' WS-SECOND
               DELIMITED BY SIZE INTO WS-FORMATTED-TIME
           
           OPEN INPUT TRANSACTION-FILE
           OPEN OUTPUT RECON-REPORT
           OPEN OUTPUT ERROR-LOG
           
           CALL 'DataProgram' USING 'READ', WS-OPENING-BALANCE
           MOVE WS-OPENING-BALANCE TO WS-CALCULATED-BALANCE
           
           DISPLAY "========================================="
           DISPLAY "END-OF-DAY RECONCILIATION JOB STARTED"
           DISPLAY "========================================="
           DISPLAY "Date: " WS-FORMATTED-DATE
           DISPLAY "Time: " WS-FORMATTED-TIME
           DISPLAY "Opening Balance: $" WS-OPENING-BALANCE
           DISPLAY " ".

       PROCESS-TRANSACTIONS.
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRANSACTION-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-SINGLE-TXN
               END-READ
           END-PERFORM.

       PROCESS-SINGLE-TXN.
           ADD 1 TO WS-TRANSACTION-COUNT
           
           EVALUATE TXN-TYPE
               WHEN 'CREDIT'
                   PERFORM PROCESS-CREDIT-TXN
               WHEN 'DEBIT '
                   PERFORM PROCESS-DEBIT-TXN
               WHEN OTHER
                   PERFORM LOG-ERROR
           END-EVALUATE.

       PROCESS-CREDIT-TXN.
           ADD 1 TO WS-CREDIT-COUNT
           ADD TXN-AMOUNT TO WS-TOTAL-CREDITS
           ADD TXN-AMOUNT TO WS-CALCULATED-BALANCE
           
           IF TXN-AMOUNT > WS-LARGEST-CREDIT
               MOVE TXN-AMOUNT TO WS-LARGEST-CREDIT
           END-IF.

       PROCESS-DEBIT-TXN.
           ADD 1 TO WS-DEBIT-COUNT
           ADD TXN-AMOUNT TO WS-TOTAL-DEBITS
           SUBTRACT TXN-AMOUNT FROM WS-CALCULATED-BALANCE
           
           IF TXN-AMOUNT > WS-LARGEST-DEBIT
               MOVE TXN-AMOUNT TO WS-LARGEST-DEBIT
           END-IF.

       LOG-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           STRING "ERROR: Invalid transaction type [" TXN-TYPE
                  "] for reference " TXN-REF
               DELIMITED BY SIZE INTO ERROR-LINE
           WRITE ERROR-LINE.

       PERFORM-RECONCILIATION.
           CALL 'DataProgram' USING 'READ', WS-CLOSING-BALANCE
           COMPUTE WS-DIFFERENCE = 
               WS-CLOSING-BALANCE - WS-CALCULATED-BALANCE
           
           IF WS-DIFFERENCE = 0
               MOVE 'BALANCED' TO WS-RECON-STATUS
               DISPLAY "Reconciliation: BALANCED"
           ELSE
               MOVE 'OUT OF BALANCE' TO WS-RECON-STATUS
               DISPLAY "Reconciliation: OUT OF BALANCE"
               DISPLAY "Difference: $" WS-DIFFERENCE
           END-IF.

       GENERATE-REPORT.
           WRITE REPORT-LINE FROM HEADER-1
           WRITE REPORT-LINE FROM HEADER-2
           WRITE REPORT-LINE FROM HEADER-1
           MOVE WS-FORMATTED-DATE TO H3-DATE
           MOVE WS-FORMATTED-TIME TO H3-TIME
           WRITE REPORT-LINE FROM HEADER-3
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'BALANCE RECONCILIATION' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Opening Balance:' TO DL-LABEL
           MOVE WS-OPENING-BALANCE TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Total Credits:' TO DL-LABEL
           MOVE WS-TOTAL-CREDITS TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Total Debits:' TO DL-LABEL
           MOVE WS-TOTAL-DEBITS TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Calculated Closing Balance:' TO DL-LABEL
           MOVE WS-CALCULATED-BALANCE TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Actual Closing Balance:' TO DL-LABEL
           MOVE WS-CLOSING-BALANCE TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Difference:' TO DL-LABEL
           MOVE WS-DIFFERENCE TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-RECON-STATUS TO SL-STATUS
           WRITE REPORT-LINE FROM STATUS-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TRANSACTION STATISTICS' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Transactions Processed:' TO CL-LABEL
           MOVE WS-TRANSACTION-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE 'Credit Transactions:' TO CL-LABEL
           MOVE WS-CREDIT-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE 'Debit Transactions:' TO CL-LABEL
           MOVE WS-DEBIT-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE 'Errors Encountered:' TO CL-LABEL
           MOVE WS-ERROR-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TRANSACTION ANALYSIS' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Largest Credit Transaction:' TO DL-LABEL
           MOVE WS-LARGEST-CREDIT TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Largest Debit Transaction:' TO DL-LABEL
           MOVE WS-LARGEST-DEBIT TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM HEADER-1
           
           DISPLAY " "
           DISPLAY "Report generated: eod_reconciliation_report.txt"
           IF WS-ERROR-COUNT > 0
               DISPLAY "Errors logged: eod_errors.log"
           END-IF
           DISPLAY "========================================="
           DISPLAY "END-OF-DAY RECONCILIATION JOB COMPLETED"
           DISPLAY "=========================================".

       CLEANUP.
           CLOSE TRANSACTION-FILE
           CLOSE RECON-REPORT
           CLOSE ERROR-LOG.
