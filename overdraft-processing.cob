       IDENTIFICATION DIVISION.
       PROGRAM-ID. OverdraftProcessing.
       AUTHOR. Banking Operations Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'account_balances.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OVERDRAFT-REPORT ASSIGN TO 'overdraft_report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NOTICE-FILE ASSIGN TO 'overdraft_notices.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FEE-POSTINGS ASSIGN TO 'overdraft_fee_postings.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER       PIC X(12).
           05  ACCT-TYPE         PIC X(8).
           05  CURRENT-BALANCE   PIC S9(10)V99.
           05  MINIMUM-BALANCE   PIC 9(10)V99.
           05  LAST-OD-DATE      PIC X(8).

       FD  OVERDRAFT-REPORT.
       01  REPORT-LINE           PIC X(132).

       FD  NOTICE-FILE.
       01  NOTICE-LINE           PIC X(80).

       FD  FEE-POSTINGS.
       01  FEE-RECORD.
           05  FEE-ACCT-NUMBER   PIC X(12).
           05  FEE-AMOUNT        PIC 9(5)V99.
           05  FEE-TYPE          PIC X(15).
           05  FEE-DATE          PIC X(8).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE 'N'.
       01  WS-CURRENT-DATE.
           05  WS-YEAR           PIC 9(4).
           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99.
       01  WS-FORMATTED-DATE     PIC X(10).
       
       01  WS-ACCOUNT-COUNT      PIC 9(7) VALUE 0.
       01  WS-OVERDRAFT-COUNT    PIC 9(7) VALUE 0.
       01  WS-TOTAL-OD-AMOUNT    PIC 9(12)V99 VALUE 0.
       01  WS-TOTAL-FEES         PIC 9(10)V99 VALUE 0.
       
       01  WS-OVERDRAFT-FEE      PIC 9(5)V99 VALUE 35.00.
       01  WS-DAILY-OD-FEE       PIC 9(5)V99 VALUE 5.00.
       01  WS-MAX-DAILY-FEES     PIC 9(3) VALUE 10.
       01  WS-DAYS-OVERDRAWN     PIC 9(3).
       01  WS-CALCULATED-FEE     PIC 9(5)V99.
       
       01  HEADER-1.
           05  FILLER            PIC X(132) VALUE ALL '='.
       01  HEADER-2.
           05  FILLER            PIC X(40) VALUE SPACES.
           05  FILLER            PIC X(52) 
               VALUE 'OVERDRAFT PROCESSING AND FEE ASSESSMENT REPORT'.
           05  FILLER            PIC X(40) VALUE SPACES.
       01  HEADER-3.
           05  FILLER            PIC X(15) VALUE 'Processing Date: '.
           05  H3-DATE           PIC X(10).
           05  FILLER            PIC X(107) VALUE SPACES.
       
       01  SECTION-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  SH-TITLE          PIC X(50).
           05  FILLER            PIC X(77) VALUE SPACES.
       
       01  DETAIL-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(12) VALUE 'Account'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(8) VALUE 'Type'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Balance'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(10) VALUE 'OD Amount'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(10) VALUE 'Fee'.
           05  FILLER            PIC X(60) VALUE SPACES.
       
       01  DETAIL-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  DL-ACCOUNT        PIC X(12).
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-TYPE           PIC X(8).
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-BALANCE        PIC -ZZZ,ZZ9.99.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-OD-AMOUNT      PIC ZZZ,ZZ9.99.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-FEE            PIC ZZZ.99.
           05  FILLER            PIC X(60) VALUE SPACES.
       
       01  SUMMARY-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  SL-LABEL          PIC X(40).
           05  SL-VALUE          PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER            PIC X(72) VALUE SPACES.
       
       01  COUNT-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  CL-LABEL          PIC X(40).
           05  CL-COUNT          PIC ZZZ,ZZ9.
           05  FILLER            PIC X(80) VALUE SPACES.
       
       01  NOTICE-HEADER.
           05  FILLER            PIC X(80) VALUE ALL '*'.
       01  NOTICE-TITLE.
           05  FILLER            PIC X(20) VALUE SPACES.
           05  FILLER            PIC X(40) 
               VALUE 'OVERDRAFT NOTICE - IMMEDIATE ATTENTION'.
           05  FILLER            PIC X(20) VALUE SPACES.
       01  NOTICE-DETAIL.
           05  ND-TEXT           PIC X(80).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-JOB
           PERFORM PROCESS-ACCOUNTS
           PERFORM GENERATE-REPORT
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZE-JOB.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           STRING WS-MONTH '/' WS-DAY '/' WS-YEAR
               DELIMITED BY SIZE INTO WS-FORMATTED-DATE
           
           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT OVERDRAFT-REPORT
           OPEN OUTPUT NOTICE-FILE
           OPEN OUTPUT FEE-POSTINGS
           
           DISPLAY "========================================="
           DISPLAY "OVERDRAFT PROCESSING JOB STARTED"
           DISPLAY "========================================="
           DISPLAY "Processing Date: " WS-FORMATTED-DATE
           DISPLAY "Overdraft Fee: $" WS-OVERDRAFT-FEE
           DISPLAY "Daily OD Fee: $" WS-DAILY-OD-FEE
           DISPLAY " ".

       PROCESS-ACCOUNTS.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM CHECK-OVERDRAFT
               END-READ
           END-PERFORM.

       CHECK-OVERDRAFT.
           ADD 1 TO WS-ACCOUNT-COUNT
           
           IF CURRENT-BALANCE < 0
               PERFORM PROCESS-OVERDRAFT-ACCOUNT
           END-IF.

       PROCESS-OVERDRAFT-ACCOUNT.
           ADD 1 TO WS-OVERDRAFT-COUNT
           COMPUTE WS-TOTAL-OD-AMOUNT = 
               WS-TOTAL-OD-AMOUNT + (CURRENT-BALANCE * -1)
           
           PERFORM CALCULATE-OVERDRAFT-FEE
           PERFORM CREATE-FEE-POSTING
           PERFORM GENERATE-CUSTOMER-NOTICE
           
           ADD WS-CALCULATED-FEE TO WS-TOTAL-FEES
           
           DISPLAY "Overdraft detected: Account " ACCT-NUMBER
                   " Balance: $" CURRENT-BALANCE
                   " Fee: $" WS-CALCULATED-FEE.

       CALCULATE-OVERDRAFT-FEE.
           MOVE WS-OVERDRAFT-FEE TO WS-CALCULATED-FEE
           
           IF LAST-OD-DATE NOT = SPACES
               PERFORM CALCULATE-DAYS-OVERDRAWN
               IF WS-DAYS-OVERDRAWN > 0
                   COMPUTE WS-CALCULATED-FEE = WS-OVERDRAFT-FEE +
                       (WS-DAILY-OD-FEE * WS-DAYS-OVERDRAWN)
                   IF WS-DAYS-OVERDRAWN > WS-MAX-DAILY-FEES
                       COMPUTE WS-CALCULATED-FEE = WS-OVERDRAFT-FEE +
                           (WS-DAILY-OD-FEE * WS-MAX-DAILY-FEES)
                   END-IF
               END-IF
           END-IF.

       CALCULATE-DAYS-OVERDRAWN.
           MOVE 3 TO WS-DAYS-OVERDRAWN.

       CREATE-FEE-POSTING.
           MOVE ACCT-NUMBER TO FEE-ACCT-NUMBER
           MOVE WS-CALCULATED-FEE TO FEE-AMOUNT
           MOVE 'OVERDRAFT-FEE' TO FEE-TYPE
           STRING WS-YEAR WS-MONTH WS-DAY
               DELIMITED BY SIZE INTO FEE-DATE
           WRITE FEE-RECORD.

       GENERATE-CUSTOMER-NOTICE.
           WRITE NOTICE-LINE FROM NOTICE-HEADER
           WRITE NOTICE-LINE FROM NOTICE-TITLE
           WRITE NOTICE-LINE FROM NOTICE-HEADER
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           
           STRING 'Account Number: ' ACCT-NUMBER
               DELIMITED BY SIZE INTO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           STRING 'Date: ' WS-FORMATTED-DATE
               DELIMITED BY SIZE INTO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           
           MOVE 'OVERDRAFT ALERT: Your account has a negative balance.'
               TO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           STRING 'Current Balance: -$' 
               FUNCTION ABS(CURRENT-BALANCE)
               DELIMITED BY SIZE INTO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           
           MOVE 'An overdraft fee has been assessed to your account:'
               TO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           STRING 'Overdraft Fee: $' WS-CALCULATED-FEE
               DELIMITED BY SIZE INTO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           
           MOVE 'Please deposit funds immediately to avoid additional'
               TO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           MOVE 'fees and potential account closure.'
               TO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           
           MOVE 'For assistance, contact us at (555) 123-4567.'
               TO ND-TEXT
           WRITE NOTICE-LINE FROM NOTICE-DETAIL
           
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           WRITE NOTICE-LINE FROM NOTICE-HEADER
           MOVE SPACES TO NOTICE-LINE
           WRITE NOTICE-LINE
           WRITE NOTICE-LINE.

       GENERATE-REPORT.
           WRITE REPORT-LINE FROM HEADER-1
           WRITE REPORT-LINE FROM HEADER-2
           WRITE REPORT-LINE FROM HEADER-1
           MOVE WS-FORMATTED-DATE TO H3-DATE
           WRITE REPORT-LINE FROM HEADER-3
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'FEE SCHEDULE' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Overdraft Fee (per occurrence):' TO SL-LABEL
           MOVE WS-OVERDRAFT-FEE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Daily Overdraft Fee:' TO SL-LABEL
           MOVE WS-DAILY-OD-FEE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'OVERDRAFT ACCOUNTS' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           WRITE REPORT-LINE FROM DETAIL-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           
           CLOSE ACCOUNT-FILE
           OPEN INPUT ACCOUNT-FILE
           MOVE 'N' TO WS-EOF
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CURRENT-BALANCE < 0
                           PERFORM WRITE-OVERDRAFT-DETAIL
                       END-IF
               END-READ
           END-PERFORM
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'PROCESSING SUMMARY' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Accounts Reviewed:' TO CL-LABEL
           MOVE WS-ACCOUNT-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE 'Accounts in Overdraft:' TO CL-LABEL
           MOVE WS-OVERDRAFT-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Overdraft Amount:' TO SL-LABEL
           MOVE WS-TOTAL-OD-AMOUNT TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Total Fees Assessed:' TO SL-LABEL
           MOVE WS-TOTAL-FEES TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM HEADER-1
           
           DISPLAY " "
           DISPLAY "Overdraft processing completed:"
           DISPLAY "  Accounts reviewed: " WS-ACCOUNT-COUNT
           DISPLAY "  Overdraft accounts: " WS-OVERDRAFT-COUNT
           DISPLAY "  Total fees: $" WS-TOTAL-FEES
           DISPLAY " "
           DISPLAY "Reports generated:"
           DISPLAY "  overdraft_report.txt"
           DISPLAY "  overdraft_notices.txt"
           DISPLAY "  overdraft_fee_postings.dat"
           DISPLAY "========================================="
           DISPLAY "OVERDRAFT PROCESSING JOB COMPLETED"
           DISPLAY "=========================================".

       WRITE-OVERDRAFT-DETAIL.
           MOVE ACCT-NUMBER TO DL-ACCOUNT
           MOVE ACCT-TYPE TO DL-TYPE
           MOVE CURRENT-BALANCE TO DL-BALANCE
           COMPUTE DL-OD-AMOUNT = CURRENT-BALANCE * -1
           
           PERFORM CALCULATE-OVERDRAFT-FEE
           MOVE WS-CALCULATED-FEE TO DL-FEE
           WRITE REPORT-LINE FROM DETAIL-LINE.

       CLEANUP.
           CLOSE ACCOUNT-FILE
           CLOSE OVERDRAFT-REPORT
           CLOSE NOTICE-FILE
           CLOSE FEE-POSTINGS.
