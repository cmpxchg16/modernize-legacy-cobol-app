       IDENTIFICATION DIVISION.
       PROGRAM-ID. EODReconciliationEnterprise.
       AUTHOR. Banking Operations Team - Enterprise Edition.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO 'eod_transactions.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RECON-REPORT ASSIGN TO 
               'eod_reconciliation_enterprise_report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-LOG ASSIGN TO 'eod_enterprise_errors.log'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT AUDIT-LOG ASSIGN TO 'eod_enterprise_audit.log'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT GL-POSTING ASSIGN TO 'gl_postings.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT JOB-CONTROL ASSIGN TO 'job-control.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CHECKPOINT-FILE ASSIGN TO 'eod_checkpoint.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CHECKPOINT-STATUS.
           SELECT PROCESSED-TXN-LOG ASSIGN TO 'processed_txn_log.dat'
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
       01  ERROR-LINE            PIC X(150).

       FD  AUDIT-LOG.
       01  AUDIT-LINE            PIC X(200).

       FD  GL-POSTING.
       01  GL-RECORD.
           05  GL-RUN-ID         PIC X(20).
           05  GL-TXN-REF        PIC X(15).
           05  GL-ACCOUNT        PIC X(10).
           05  GL-DEBIT-AMOUNT   PIC 9(10)V99.
           05  GL-CREDIT-AMOUNT  PIC 9(10)V99.
           05  GL-DESCRIPTION    PIC X(50).
           05  GL-POST-DATE      PIC X(8).
           05  GL-POST-TIME      PIC X(6).

       FD  JOB-CONTROL.
       01  JOB-CONTROL-RECORD.
           05  JC-JOB-ID         PIC 9(9).
           05  JC-JOB-NAME       PIC X(15).
           05  JC-START-TIME     PIC X(14).
           05  JC-STATUS         PIC X(10).
           05  JC-END-TIME       PIC X(14).
           05  JC-RECORDS-PROC   PIC 9(9).
           05  JC-RECORDS-ERROR  PIC 9(9).

       FD  CHECKPOINT-FILE.
       01  CHECKPOINT-RECORD.
           05  CP-LAST-TXN-REF   PIC X(15).
           05  CP-RECORDS-PROC   PIC 9(9).
           05  CP-TIMESTAMP      PIC X(14).
           05  CP-BALANCE        PIC S9(10)V99.

       FD  PROCESSED-TXN-LOG.
       01  PROCESSED-TXN-RECORD.
           05  PT-RUN-ID         PIC X(20).
           05  PT-TXN-REF        PIC X(15).
           05  PT-TIMESTAMP      PIC X(14).
           05  PT-HASH           PIC X(32).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE 'N'.
       01  WS-RUN-ID             PIC X(20).
       01  WS-JOB-ID             PIC 9(9).
       01  WS-RESTART-MODE       PIC X VALUE 'N'.
       01  WS-LAST-CHECKPOINT    PIC X(15).
       01  WS-CHECKPOINT-STATUS  PIC XX.
       
       01  WS-OPENING-BALANCE    PIC 9(10)V99 VALUE 1000.00.
       01  WS-CLOSING-BALANCE    PIC 9(10)V99.
       01  WS-CALCULATED-BALANCE PIC 9(10)V99.
       01  WS-DIFFERENCE         PIC S9(10)V99.
       
       01  WS-TRANSACTION-COUNT  PIC 9(9) VALUE 0.
       01  WS-CREDIT-COUNT       PIC 9(9) VALUE 0.
       01  WS-DEBIT-COUNT        PIC 9(9) VALUE 0.
       01  WS-ERROR-COUNT        PIC 9(7) VALUE 0.
       01  WS-DUPLICATE-COUNT    PIC 9(7) VALUE 0.
       01  WS-CHECKPOINT-COUNTER PIC 9(5) VALUE 0.
       01  WS-CHECKPOINT-INTERVAL PIC 9(5) VALUE 100.
       01  WS-AUDIT-SEQ-NUM      PIC 9(9) VALUE 1.
       
       01  WS-TOTAL-CREDITS      PIC 9(12)V99 VALUE 0.
       01  WS-TOTAL-DEBITS       PIC 9(12)V99 VALUE 0.
       01  WS-LARGEST-CREDIT     PIC 9(10)V99 VALUE 0.
       01  WS-LARGEST-DEBIT      PIC 9(10)V99 VALUE 0.
       
       01  WS-INPUT-HASH-TOTAL   PIC 9(14)V99 VALUE 0.
       01  WS-OUTPUT-HASH-TOTAL  PIC 9(14)V99 VALUE 0.
       01  WS-EXPECTED-RECORDS   PIC 9(9) VALUE 0.
       
       01  WS-START-TIMESTAMP.
           05  WS-START-DATE     PIC X(8).
           05  WS-START-TIME     PIC X(6).
       01  WS-END-TIMESTAMP.
           05  WS-END-DATE       PIC X(8).
           05  WS-END-TIME       PIC X(6).
       01  WS-CURRENT-TIMESTAMP  PIC X(14).
       
       01  WS-FORMATTED-DATE     PIC X(10).
       01  WS-FORMATTED-TIME     PIC X(8).
       01  WS-RECON-STATUS       PIC X(20).
       01  WS-CONTROL-TOTAL-STATUS PIC X(20).
       
       01  WS-GL-CASH-ACCOUNT    PIC X(10) VALUE '1010-CASH'.
       01  WS-GL-REVENUE-ACCOUNT PIC X(10) VALUE '4000-REV'.
       01  WS-GL-EXPENSE-ACCOUNT PIC X(10) VALUE '5000-EXP'.
       
       01  HEADER-1.
           05  FILLER            PIC X(132) VALUE ALL '='.
       01  HEADER-2.
           05  FILLER            PIC X(30) VALUE SPACES.
           05  FILLER            PIC X(72) 
               VALUE 'ENTERPRISE END-OF-DAY RECONCILIATION REPORT'.
           05  FILLER            PIC X(30) VALUE SPACES.
       01  HEADER-3.
           05  FILLER            PIC X(10) VALUE 'Run ID: '.
           05  H3-RUN-ID         PIC X(20).
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(10) VALUE 'Job ID: '.
           05  H3-JOB-ID         PIC 9(9).
           05  FILLER            PIC X(78) VALUE SPACES.
       01  HEADER-4.
           05  FILLER            PIC X(15) VALUE 'Report Date: '.
           05  H4-DATE           PIC X(10).
           05  FILLER            PIC X(10) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Report Time: '.
           05  H4-TIME           PIC X(8).
           05  FILLER            PIC X(74) VALUE SPACES.
       
       01  SECTION-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  SH-TITLE          PIC X(60).
           05  FILLER            PIC X(67) VALUE SPACES.
       
       01  DETAIL-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  DL-LABEL          PIC X(40).
           05  DL-VALUE          PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER            PIC X(72) VALUE SPACES.
       
       01  COUNT-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  CL-LABEL          PIC X(40).
           05  CL-COUNT          PIC ZZZ,ZZZ,ZZ9.
           05  FILLER            PIC X(72) VALUE SPACES.
       
       01  STATUS-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  SL-LABEL          PIC X(30).
           05  SL-STATUS         PIC X(20).
           05  FILLER            PIC X(77) VALUE SPACES.
       
       01  METRICS-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  ML-LABEL          PIC X(40).
           05  ML-VALUE          PIC ZZZ,ZZ9.99.
           05  FILLER            PIC X(77) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-JOB
           PERFORM CHECK-RESTART-MODE
           PERFORM PROCESS-TRANSACTIONS
           PERFORM PERFORM-RECONCILIATION
           PERFORM VALIDATE-CONTROL-TOTALS
           PERFORM GENERATE-REPORT
           PERFORM FINALIZE-JOB
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZE-JOB.
           ACCEPT WS-START-DATE FROM DATE YYYYMMDD
           ACCEPT WS-START-TIME FROM TIME
           STRING WS-START-DATE WS-START-TIME
               DELIMITED BY SIZE INTO WS-CURRENT-TIMESTAMP
           
           PERFORM GENERATE-RUN-ID
           PERFORM GENERATE-JOB-ID
           PERFORM CREATE-JOB-CONTROL-RECORD
           
           STRING WS-START-DATE(5:2) '/' WS-START-DATE(7:2) '/' 
                  WS-START-DATE(1:4)
               DELIMITED BY SIZE INTO WS-FORMATTED-DATE
           STRING WS-START-TIME(1:2) ':' WS-START-TIME(3:2) ':' 
                  WS-START-TIME(5:2)
               DELIMITED BY SIZE INTO WS-FORMATTED-TIME
           
           OPEN INPUT TRANSACTION-FILE
           OPEN OUTPUT RECON-REPORT
           OPEN OUTPUT ERROR-LOG
           OPEN OUTPUT AUDIT-LOG
           OPEN OUTPUT GL-POSTING
           OPEN EXTEND PROCESSED-TXN-LOG
           
           CALL 'DataProgram' USING 'READ', WS-OPENING-BALANCE
           MOVE WS-OPENING-BALANCE TO WS-CALCULATED-BALANCE
           
           PERFORM WRITE-AUDIT-LOG
           
           DISPLAY "========================================="
           DISPLAY "ENTERPRISE EOD RECONCILIATION JOB STARTED"
           DISPLAY "========================================="
           DISPLAY "Run ID: " WS-RUN-ID
           DISPLAY "Job ID: " WS-JOB-ID
           DISPLAY "Date: " WS-FORMATTED-DATE
           DISPLAY "Time: " WS-FORMATTED-TIME
           DISPLAY "Opening Balance: $" WS-OPENING-BALANCE
           DISPLAY "Checkpoint Interval: " WS-CHECKPOINT-INTERVAL
           DISPLAY " ".

       GENERATE-RUN-ID.
           STRING 'EOD-' WS-START-DATE '-' WS-START-TIME
               DELIMITED BY SIZE INTO WS-RUN-ID.

       GENERATE-JOB-ID.
           ACCEPT WS-JOB-ID FROM TIME.

       CREATE-JOB-CONTROL-RECORD.
           OPEN EXTEND JOB-CONTROL
           MOVE WS-JOB-ID TO JC-JOB-ID
           MOVE 'EOD-RECON' TO JC-JOB-NAME
           MOVE WS-CURRENT-TIMESTAMP TO JC-START-TIME
           MOVE 'STARTED' TO JC-STATUS
           MOVE SPACES TO JC-END-TIME
           MOVE 0 TO JC-RECORDS-PROC
           MOVE 0 TO JC-RECORDS-ERROR
           WRITE JOB-CONTROL-RECORD
           CLOSE JOB-CONTROL.

       WRITE-AUDIT-LOG.
           STRING WS-AUDIT-SEQ-NUM '|' WS-CURRENT-TIMESTAMP '|' 
                  WS-RUN-ID '|JOB-STARTED|Opening Balance: ' 
                  WS-OPENING-BALANCE
               DELIMITED BY SIZE INTO AUDIT-LINE
           WRITE AUDIT-LINE
           ADD 1 TO WS-AUDIT-SEQ-NUM.

       CHECK-RESTART-MODE.
           MOVE 'N' TO WS-RESTART-MODE
           OPEN INPUT CHECKPOINT-FILE
           IF WS-CHECKPOINT-STATUS = '00'
               READ CHECKPOINT-FILE
                   AT END
                       MOVE 'N' TO WS-RESTART-MODE
                   NOT AT END
                       MOVE 'Y' TO WS-RESTART-MODE
                       MOVE CP-LAST-TXN-REF TO WS-LAST-CHECKPOINT
                       MOVE CP-RECORDS-PROC TO WS-TRANSACTION-COUNT
                       MOVE CP-BALANCE TO WS-CALCULATED-BALANCE
                       DISPLAY "RESTART MODE: Resuming from checkpoint"
                       DISPLAY "Last processed: " WS-LAST-CHECKPOINT
                       DISPLAY "Records already processed: " 
                               WS-TRANSACTION-COUNT
               END-READ
               CLOSE CHECKPOINT-FILE
           END-IF.

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
           IF WS-RESTART-MODE = 'Y'
               IF TXN-REF NOT > WS-LAST-CHECKPOINT
                   EXIT PARAGRAPH
               ELSE
                   MOVE 'N' TO WS-RESTART-MODE
               END-IF
           END-IF
           
           PERFORM CHECK-DUPLICATE
           IF WS-DUPLICATE-COUNT = 0
               ADD 1 TO WS-TRANSACTION-COUNT
               ADD TXN-AMOUNT TO WS-INPUT-HASH-TOTAL
               
               EVALUATE TXN-TYPE
                   WHEN 'CREDIT'
                       PERFORM PROCESS-CREDIT-TXN
                   WHEN 'DEBIT '
                       PERFORM PROCESS-DEBIT-TXN
                   WHEN OTHER
                       PERFORM LOG-ERROR
               END-EVALUATE
               
               PERFORM LOG-PROCESSED-TXN
               PERFORM CHECK-CHECKPOINT
           END-IF.

       CHECK-DUPLICATE.
           MOVE 0 TO WS-DUPLICATE-COUNT.

       PROCESS-CREDIT-TXN.
           ADD 1 TO WS-CREDIT-COUNT
           ADD TXN-AMOUNT TO WS-TOTAL-CREDITS
           ADD TXN-AMOUNT TO WS-CALCULATED-BALANCE
           ADD TXN-AMOUNT TO WS-OUTPUT-HASH-TOTAL
           
           IF TXN-AMOUNT > WS-LARGEST-CREDIT
               MOVE TXN-AMOUNT TO WS-LARGEST-CREDIT
           END-IF
           
           PERFORM CREATE-GL-POSTING-CREDIT.

       PROCESS-DEBIT-TXN.
           ADD 1 TO WS-DEBIT-COUNT
           ADD TXN-AMOUNT TO WS-TOTAL-DEBITS
           SUBTRACT TXN-AMOUNT FROM WS-CALCULATED-BALANCE
           ADD TXN-AMOUNT TO WS-OUTPUT-HASH-TOTAL
           
           IF TXN-AMOUNT > WS-LARGEST-DEBIT
               MOVE TXN-AMOUNT TO WS-LARGEST-DEBIT
           END-IF
           
           PERFORM CREATE-GL-POSTING-DEBIT.

       CREATE-GL-POSTING-CREDIT.
           MOVE WS-RUN-ID TO GL-RUN-ID
           MOVE TXN-REF TO GL-TXN-REF
           MOVE WS-GL-CASH-ACCOUNT TO GL-ACCOUNT
           MOVE TXN-AMOUNT TO GL-DEBIT-AMOUNT
           MOVE 0 TO GL-CREDIT-AMOUNT
           STRING 'CREDIT-' TXN-CHANNEL DELIMITED BY SIZE 
               INTO GL-DESCRIPTION
           MOVE TXN-DATE TO GL-POST-DATE
           MOVE TXN-TIME TO GL-POST-TIME
           WRITE GL-RECORD
           
           MOVE WS-GL-REVENUE-ACCOUNT TO GL-ACCOUNT
           MOVE 0 TO GL-DEBIT-AMOUNT
           MOVE TXN-AMOUNT TO GL-CREDIT-AMOUNT
           WRITE GL-RECORD.

       CREATE-GL-POSTING-DEBIT.
           MOVE WS-RUN-ID TO GL-RUN-ID
           MOVE TXN-REF TO GL-TXN-REF
           MOVE WS-GL-EXPENSE-ACCOUNT TO GL-ACCOUNT
           MOVE TXN-AMOUNT TO GL-DEBIT-AMOUNT
           MOVE 0 TO GL-CREDIT-AMOUNT
           STRING 'DEBIT-' TXN-CHANNEL DELIMITED BY SIZE 
               INTO GL-DESCRIPTION
           MOVE TXN-DATE TO GL-POST-DATE
           MOVE TXN-TIME TO GL-POST-TIME
           WRITE GL-RECORD
           
           MOVE WS-GL-CASH-ACCOUNT TO GL-ACCOUNT
           MOVE 0 TO GL-DEBIT-AMOUNT
           MOVE TXN-AMOUNT TO GL-CREDIT-AMOUNT
           WRITE GL-RECORD.

       LOG-PROCESSED-TXN.
           MOVE WS-RUN-ID TO PT-RUN-ID
           MOVE TXN-REF TO PT-TXN-REF
           MOVE WS-CURRENT-TIMESTAMP TO PT-TIMESTAMP
           MOVE SPACES TO PT-HASH
           WRITE PROCESSED-TXN-RECORD.

       CHECK-CHECKPOINT.
           ADD 1 TO WS-CHECKPOINT-COUNTER
           IF WS-CHECKPOINT-COUNTER >= WS-CHECKPOINT-INTERVAL
               PERFORM SAVE-CHECKPOINT
               MOVE 0 TO WS-CHECKPOINT-COUNTER
           END-IF.

       SAVE-CHECKPOINT.
           OPEN OUTPUT CHECKPOINT-FILE
           MOVE TXN-REF TO CP-LAST-TXN-REF
           MOVE WS-TRANSACTION-COUNT TO CP-RECORDS-PROC
           MOVE WS-CURRENT-TIMESTAMP TO CP-TIMESTAMP
           MOVE WS-CALCULATED-BALANCE TO CP-BALANCE
           WRITE CHECKPOINT-RECORD
           CLOSE CHECKPOINT-FILE
           
           DISPLAY "Checkpoint saved at record " WS-TRANSACTION-COUNT.

       LOG-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           STRING WS-AUDIT-SEQ-NUM '|' WS-CURRENT-TIMESTAMP '|' 
                  WS-RUN-ID '|ERROR|Invalid transaction type [' 
                  TXN-TYPE '] for reference ' TXN-REF
               DELIMITED BY SIZE INTO ERROR-LINE
           WRITE ERROR-LINE
           ADD 1 TO WS-AUDIT-SEQ-NUM.

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

       VALIDATE-CONTROL-TOTALS.
           IF WS-INPUT-HASH-TOTAL = WS-OUTPUT-HASH-TOTAL
               MOVE 'VALID' TO WS-CONTROL-TOTAL-STATUS
               DISPLAY "Control Totals: VALID"
           ELSE
               MOVE 'INVALID' TO WS-CONTROL-TOTAL-STATUS
               DISPLAY "Control Totals: INVALID"
               DISPLAY "Input Hash: " WS-INPUT-HASH-TOTAL
               DISPLAY "Output Hash: " WS-OUTPUT-HASH-TOTAL
           END-IF.

       GENERATE-REPORT.
           WRITE REPORT-LINE FROM HEADER-1
           WRITE REPORT-LINE FROM HEADER-2
           WRITE REPORT-LINE FROM HEADER-1
           MOVE WS-RUN-ID TO H3-RUN-ID
           MOVE WS-JOB-ID TO H3-JOB-ID
           WRITE REPORT-LINE FROM HEADER-3
           MOVE WS-FORMATTED-DATE TO H4-DATE
           MOVE WS-FORMATTED-TIME TO H4-TIME
           WRITE REPORT-LINE FROM HEADER-4
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
           MOVE 'Reconciliation Status:' TO SL-LABEL
           MOVE WS-RECON-STATUS TO SL-STATUS
           WRITE REPORT-LINE FROM STATUS-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'CONTROL TOTALS' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Input Hash Total:' TO DL-LABEL
           MOVE WS-INPUT-HASH-TOTAL TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE 'Output Hash Total:' TO DL-LABEL
           MOVE WS-OUTPUT-HASH-TOTAL TO DL-VALUE
           WRITE REPORT-LINE FROM DETAIL-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Control Total Status:' TO SL-LABEL
           MOVE WS-CONTROL-TOTAL-STATUS TO SL-STATUS
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
           
           MOVE 'Duplicate Transactions:' TO CL-LABEL
           MOVE WS-DUPLICATE-COUNT TO CL-COUNT
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
           DISPLAY "Reports generated:"
           DISPLAY "  eod_reconciliation_enterprise_report.txt"
           DISPLAY "  gl_postings.dat"
           DISPLAY "  eod_enterprise_audit.log"
           IF WS-ERROR-COUNT > 0
               DISPLAY "  eod_enterprise_errors.log"
           END-IF.

       FINALIZE-JOB.
           ACCEPT WS-END-DATE FROM DATE YYYYMMDD
           ACCEPT WS-END-TIME FROM TIME
           STRING WS-END-DATE WS-END-TIME
               DELIMITED BY SIZE INTO WS-CURRENT-TIMESTAMP
           
           PERFORM UPDATE-JOB-CONTROL-RECORD
           PERFORM DELETE-CHECKPOINT-FILE
           
           DISPLAY "========================================="
           DISPLAY "ENTERPRISE EOD RECONCILIATION COMPLETED"
           DISPLAY "========================================="
           DISPLAY "Run ID: " WS-RUN-ID
           DISPLAY "Status: " WS-RECON-STATUS
           DISPLAY "Control Totals: " WS-CONTROL-TOTAL-STATUS.

       UPDATE-JOB-CONTROL-RECORD.
           OPEN EXTEND JOB-CONTROL
           MOVE WS-JOB-ID TO JC-JOB-ID
           MOVE 'EOD-RECON' TO JC-JOB-NAME
           MOVE WS-START-TIMESTAMP TO JC-START-TIME
           MOVE 'COMPLETED' TO JC-STATUS
           MOVE WS-CURRENT-TIMESTAMP TO JC-END-TIME
           MOVE WS-TRANSACTION-COUNT TO JC-RECORDS-PROC
           MOVE WS-ERROR-COUNT TO JC-RECORDS-ERROR
           WRITE JOB-CONTROL-RECORD
           CLOSE JOB-CONTROL.

       DELETE-CHECKPOINT-FILE.
           OPEN OUTPUT CHECKPOINT-FILE
           CLOSE CHECKPOINT-FILE.

       CLEANUP.
           CLOSE TRANSACTION-FILE
           CLOSE RECON-REPORT
           CLOSE ERROR-LOG
           CLOSE AUDIT-LOG
           CLOSE GL-POSTING
           CLOSE PROCESSED-TXN-LOG.
