       IDENTIFICATION DIVISION.
       PROGRAM-ID. InterestCalculation.
       AUTHOR. Banking Operations Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'account_balances.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INTEREST-REPORT ASSIGN TO 'interest_calculation_report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INTEREST-POSTINGS ASSIGN TO 'interest_postings.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER       PIC X(12).
           05  ACCT-TYPE         PIC X(8).
           05  CURRENT-BALANCE   PIC 9(10)V99.
           05  MINIMUM-BALANCE   PIC 9(10)V99.
           05  LAST-INTEREST-DATE PIC X(8).

       FD  INTEREST-REPORT.
       01  REPORT-LINE           PIC X(132).

       FD  INTEREST-POSTINGS.
       01  POSTING-RECORD.
           05  POST-ACCT-NUMBER  PIC X(12).
           05  POST-AMOUNT       PIC 9(8)V99.
           05  POST-RATE         PIC 9V9999.
           05  POST-DATE         PIC X(8).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE 'N'.
       01  WS-CURRENT-DATE.
           05  WS-YEAR           PIC 9(4).
           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99.
       01  WS-FORMATTED-DATE     PIC X(10).
       
       01  WS-ACCOUNT-COUNT      PIC 9(7) VALUE 0.
       01  WS-ELIGIBLE-COUNT     PIC 9(7) VALUE 0.
       01  WS-TOTAL-INTEREST     PIC 9(10)V99 VALUE 0.
       01  WS-TOTAL-BALANCES     PIC 9(12)V99 VALUE 0.
       
       01  WS-INTEREST-AMOUNT    PIC 9(8)V99.
       01  WS-ANNUAL-RATE        PIC 9V9999.
       01  WS-DAILY-RATE         PIC 9V999999.
       01  WS-MINIMUM-REQUIRED   PIC 9(8)V99 VALUE 100.00.
       
       01  INTEREST-TIERS.
           05  TIER-1.
               10  T1-MIN-BALANCE PIC 9(8)V99 VALUE 0.
               10  T1-MAX-BALANCE PIC 9(8)V99 VALUE 999.99.
               10  T1-RATE        PIC 9V9999 VALUE 0.0050.
           05  TIER-2.
               10  T2-MIN-BALANCE PIC 9(8)V99 VALUE 1000.00.
               10  T2-MAX-BALANCE PIC 9(8)V99 VALUE 9999.99.
               10  T2-RATE        PIC 9V9999 VALUE 0.0150.
           05  TIER-3.
               10  T3-MIN-BALANCE PIC 9(8)V99 VALUE 10000.00.
               10  T3-MAX-BALANCE PIC 9(8)V99 VALUE 99999.99.
               10  T3-RATE        PIC 9V9999 VALUE 0.0250.
           05  TIER-4.
               10  T4-MIN-BALANCE PIC 9(8)V99 VALUE 100000.00.
               10  T4-MAX-BALANCE PIC 9(8)V99 VALUE 999999.99.
               10  T4-RATE        PIC 9V9999 VALUE 0.0350.
       
       01  HEADER-1.
           05  FILLER            PIC X(132) VALUE ALL '='.
       01  HEADER-2.
           05  FILLER            PIC X(35) VALUE SPACES.
           05  FILLER            PIC X(62) 
               VALUE 'DAILY INTEREST CALCULATION AND ACCRUAL REPORT'.
           05  FILLER            PIC X(35) VALUE SPACES.
       01  HEADER-3.
           05  FILLER            PIC X(15) VALUE 'Calculation Date: '.
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
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Balance'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(8) VALUE 'Rate %'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Interest'.
           05  FILLER            PIC X(55) VALUE SPACES.
       
       01  DETAIL-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  DL-ACCOUNT        PIC X(12).
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-TYPE           PIC X(8).
           05  FILLER            PIC X(5) VALUE SPACES.
           05  DL-BALANCE        PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-RATE           PIC Z9.9999.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  DL-INTEREST       PIC ZZZ,ZZ9.99.
           05  FILLER            PIC X(55) VALUE SPACES.
       
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
           OPEN OUTPUT INTEREST-REPORT
           OPEN OUTPUT INTEREST-POSTINGS
           
           DISPLAY "========================================="
           DISPLAY "DAILY INTEREST CALCULATION JOB STARTED"
           DISPLAY "========================================="
           DISPLAY "Calculation Date: " WS-FORMATTED-DATE
           DISPLAY "Interest Tiers:"
           DISPLAY "  Tier 1: $0 - $999.99 @ 0.50% APY"
           DISPLAY "  Tier 2: $1,000 - $9,999.99 @ 1.50% APY"
           DISPLAY "  Tier 3: $10,000 - $99,999.99 @ 2.50% APY"
           DISPLAY "  Tier 4: $100,000+ @ 3.50% APY"
           DISPLAY "Minimum Balance for Interest: $" WS-MINIMUM-REQUIRED
           DISPLAY " ".

       PROCESS-ACCOUNTS.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM CALCULATE-INTEREST
               END-READ
           END-PERFORM.

       CALCULATE-INTEREST.
           ADD 1 TO WS-ACCOUNT-COUNT
           ADD CURRENT-BALANCE TO WS-TOTAL-BALANCES
           
           IF CURRENT-BALANCE >= WS-MINIMUM-REQUIRED
               PERFORM DETERMINE-INTEREST-RATE
               PERFORM COMPUTE-DAILY-INTEREST
               PERFORM CREATE-POSTING-RECORD
               ADD 1 TO WS-ELIGIBLE-COUNT
               ADD WS-INTEREST-AMOUNT TO WS-TOTAL-INTEREST
           ELSE
               MOVE 0 TO WS-INTEREST-AMOUNT
               MOVE 0 TO WS-ANNUAL-RATE
           END-IF.

       DETERMINE-INTEREST-RATE.
           EVALUATE TRUE
               WHEN CURRENT-BALANCE >= T1-MIN-BALANCE AND
                    CURRENT-BALANCE <= T1-MAX-BALANCE
                   MOVE T1-RATE TO WS-ANNUAL-RATE
               WHEN CURRENT-BALANCE >= T2-MIN-BALANCE AND
                    CURRENT-BALANCE <= T2-MAX-BALANCE
                   MOVE T2-RATE TO WS-ANNUAL-RATE
               WHEN CURRENT-BALANCE >= T3-MIN-BALANCE AND
                    CURRENT-BALANCE <= T3-MAX-BALANCE
                   MOVE T3-RATE TO WS-ANNUAL-RATE
               WHEN CURRENT-BALANCE >= T4-MIN-BALANCE
                   MOVE T4-RATE TO WS-ANNUAL-RATE
               WHEN OTHER
                   MOVE 0 TO WS-ANNUAL-RATE
           END-EVALUATE.

       COMPUTE-DAILY-INTEREST.
           COMPUTE WS-DAILY-RATE = WS-ANNUAL-RATE / 365
           COMPUTE WS-INTEREST-AMOUNT ROUNDED = 
               CURRENT-BALANCE * WS-DAILY-RATE.

       CREATE-POSTING-RECORD.
           MOVE ACCT-NUMBER TO POST-ACCT-NUMBER
           MOVE WS-INTEREST-AMOUNT TO POST-AMOUNT
           MOVE WS-ANNUAL-RATE TO POST-RATE
           STRING WS-YEAR WS-MONTH WS-DAY
               DELIMITED BY SIZE INTO POST-DATE
           WRITE POSTING-RECORD.

       GENERATE-REPORT.
           WRITE REPORT-LINE FROM HEADER-1
           WRITE REPORT-LINE FROM HEADER-2
           WRITE REPORT-LINE FROM HEADER-1
           MOVE WS-FORMATTED-DATE TO H3-DATE
           WRITE REPORT-LINE FROM HEADER-3
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'INTEREST RATE TIERS' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Tier 1: $0.00 - $999.99' TO SL-LABEL
           MOVE T1-RATE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Tier 2: $1,000.00 - $9,999.99' TO SL-LABEL
           MOVE T2-RATE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Tier 3: $10,000.00 - $99,999.99' TO SL-LABEL
           MOVE T3-RATE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Tier 4: $100,000.00+' TO SL-LABEL
           MOVE T4-RATE TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'CALCULATION SUMMARY' TO SH-TITLE
           WRITE REPORT-LINE FROM SECTION-HEADER
           WRITE REPORT-LINE FROM HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Accounts Processed:' TO CL-LABEL
           MOVE WS-ACCOUNT-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE 'Accounts Eligible for Interest:' TO CL-LABEL
           MOVE WS-ELIGIBLE-COUNT TO CL-COUNT
           WRITE REPORT-LINE FROM COUNT-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE 'Total Account Balances:' TO SL-LABEL
           MOVE WS-TOTAL-BALANCES TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE 'Total Interest Calculated:' TO SL-LABEL
           MOVE WS-TOTAL-INTEREST TO SL-VALUE
           WRITE REPORT-LINE FROM SUMMARY-LINE
           
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM HEADER-1
           
           DISPLAY " "
           DISPLAY "Interest calculation completed:"
           DISPLAY "  Accounts processed: " WS-ACCOUNT-COUNT
           DISPLAY "  Eligible accounts: " WS-ELIGIBLE-COUNT
           DISPLAY "  Total interest: $" WS-TOTAL-INTEREST
           DISPLAY " "
           DISPLAY "Reports generated:"
           DISPLAY "  interest_calculation_report.txt"
           DISPLAY "  interest_postings.dat"
           DISPLAY "========================================="
           DISPLAY "INTEREST CALCULATION JOB COMPLETED"
           DISPLAY "=========================================".

       CLEANUP.
           CLOSE ACCOUNT-FILE
           CLOSE INTEREST-REPORT
           CLOSE INTEREST-POSTINGS.
