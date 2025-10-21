       IDENTIFICATION DIVISION.
       PROGRAM-ID. StatementGeneration.
       AUTHOR. Banking Operations Team.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'account_master.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO 'monthly_transactions.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STATEMENT-FILE ASSIGN TO 'monthly_statements.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-MASTER.
           05  AM-ACCT-NUMBER    PIC X(12).
           05  AM-CUSTOMER-NAME  PIC X(30).
           05  AM-ADDRESS-1      PIC X(30).
           05  AM-ADDRESS-2      PIC X(30).
           05  AM-CITY-STATE-ZIP PIC X(30).
           05  AM-PHONE          PIC X(12).
           05  AM-ACCT-TYPE      PIC X(8).
           05  AM-OPENING-DATE   PIC X(8).

       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TR-ACCT-NUMBER    PIC X(12).
           05  TR-DATE           PIC X(8).
           05  TR-TIME           PIC X(6).
           05  TR-TYPE           PIC X(6).
           05  TR-AMOUNT         PIC 9(8)V99.
           05  TR-DESCRIPTION    PIC X(30).
           05  TR-REFERENCE      PIC X(15).
           05  TR-CHANNEL        PIC X(10).

       FD  STATEMENT-FILE.
       01  STATEMENT-LINE        PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-EOF-ACCT           PIC X VALUE 'N'.
       01  WS-EOF-TXN            PIC X VALUE 'N'.
       01  WS-CURRENT-ACCOUNT    PIC X(12).
       01  WS-STATEMENT-PERIOD   PIC X(20) VALUE 'October 2024'.
       
       01  WS-OPENING-BALANCE    PIC 9(10)V99 VALUE 1000.00.
       01  WS-CLOSING-BALANCE    PIC 9(10)V99.
       01  WS-TOTAL-CREDITS      PIC 9(10)V99 VALUE 0.
       01  WS-TOTAL-DEBITS       PIC 9(10)V99 VALUE 0.
       01  WS-TRANSACTION-COUNT  PIC 9(5) VALUE 0.
       01  WS-SERVICE-CHARGE     PIC 9(5)V99 VALUE 15.00.
       01  WS-MINIMUM-BALANCE    PIC 9(8)V99 VALUE 500.00.
       
       01  WS-CURRENT-DATE.
           05  WS-YEAR           PIC 9(4).
           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99.
       01  WS-FORMATTED-DATE     PIC X(10).
       
       01  STATEMENT-HEADER-1.
           05  FILLER            PIC X(132) VALUE ALL '*'.
       01  STATEMENT-HEADER-2.
           05  FILLER            PIC X(25) VALUE SPACES.
           05  FILLER            PIC X(82) 
               VALUE 'FIRST NATIONAL BANK - MONTHLY ACCOUNT STATEMENT'.
           05  FILLER            PIC X(25) VALUE SPACES.
       01  STATEMENT-HEADER-3.
           05  FILLER            PIC X(25) VALUE SPACES.
           05  FILLER            PIC X(82) 
               VALUE '123 Main Street, Anytown, ST 12345 | (555) 123-4567'.
           05  FILLER            PIC X(25) VALUE SPACES.
       
       01  CUSTOMER-INFO-1.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Account Holder: '.
           05  CI1-NAME          PIC X(30).
           05  FILLER            PIC X(82) VALUE SPACES.
       01  CUSTOMER-INFO-2.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Account Number: '.
           05  CI2-ACCOUNT       PIC X(12).
           05  FILLER            PIC X(100) VALUE SPACES.
       01  CUSTOMER-INFO-3.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Statement Period: '.
           05  CI3-PERIOD        PIC X(20).
           05  FILLER            PIC X(92) VALUE SPACES.
       01  CUSTOMER-INFO-4.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Statement Date: '.
           05  CI4-DATE          PIC X(10).
           05  FILLER            PIC X(102) VALUE SPACES.
       
       01  BALANCE-SUMMARY.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(25) VALUE 'ACCOUNT BALANCE SUMMARY'.
           05  FILLER            PIC X(102) VALUE SPACES.
       01  BALANCE-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  BL-LABEL          PIC X(25).
           05  BL-AMOUNT         PIC $$$,$$$,$$9.99.
           05  FILLER            PIC X(88) VALUE SPACES.
       
       01  TRANSACTION-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(30) VALUE 'TRANSACTION HISTORY'.
           05  FILLER            PIC X(97) VALUE SPACES.
       01  TXN-COLUMN-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(8) VALUE 'Date'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(30) VALUE 'Description'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(15) VALUE 'Reference'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(10) VALUE 'Amount'.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  FILLER            PIC X(10) VALUE 'Balance'.
           05  FILLER            PIC X(42) VALUE SPACES.
       
       01  TXN-DETAIL-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  TDL-DATE          PIC X(8).
           05  FILLER            PIC X(3) VALUE SPACES.
           05  TDL-DESCRIPTION   PIC X(30).
           05  FILLER            PIC X(3) VALUE SPACES.
           05  TDL-REFERENCE     PIC X(15).
           05  FILLER            PIC X(3) VALUE SPACES.
           05  TDL-AMOUNT        PIC $$$,$$9.99.
           05  FILLER            PIC X(3) VALUE SPACES.
           05  TDL-BALANCE       PIC $$$,$$9.99.
           05  FILLER            PIC X(42) VALUE SPACES.
       
       01  FEES-HEADER.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  FILLER            PIC X(25) VALUE 'FEES AND CHARGES'.
           05  FILLER            PIC X(102) VALUE SPACES.
       
       01  NOTICE-LINE.
           05  FILLER            PIC X(5) VALUE SPACES.
           05  NL-TEXT           PIC X(80).
           05  FILLER            PIC X(47) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-JOB
           PERFORM PROCESS-ACCOUNTS
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZE-JOB.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           STRING WS-MONTH '/' WS-DAY '/' WS-YEAR
               DELIMITED BY SIZE INTO WS-FORMATTED-DATE
           
           OPEN INPUT ACCOUNT-FILE
           OPEN INPUT TRANSACTION-FILE
           OPEN OUTPUT STATEMENT-FILE
           
           DISPLAY "========================================="
           DISPLAY "MONTHLY STATEMENT GENERATION JOB STARTED"
           DISPLAY "========================================="
           DISPLAY "Statement Date: " WS-FORMATTED-DATE
           DISPLAY "Statement Period: " WS-STATEMENT-PERIOD
           DISPLAY " ".

       PROCESS-ACCOUNTS.
           PERFORM UNTIL WS-EOF-ACCT = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-ACCT
                   NOT AT END
                       PERFORM GENERATE-STATEMENT
               END-READ
           END-PERFORM.

       GENERATE-STATEMENT.
           MOVE AM-ACCT-NUMBER TO WS-CURRENT-ACCOUNT
           PERFORM RESET-COUNTERS
           PERFORM WRITE-STATEMENT-HEADER
           PERFORM PROCESS-ACCOUNT-TRANSACTIONS
           PERFORM CALCULATE-FINAL-BALANCE
           PERFORM WRITE-BALANCE-SUMMARY
           PERFORM WRITE-FEES-SECTION
           PERFORM WRITE-NOTICES
           PERFORM WRITE-STATEMENT-FOOTER
           
           DISPLAY "Statement generated for account: " AM-ACCT-NUMBER.

       RESET-COUNTERS.
           MOVE 0 TO WS-TOTAL-CREDITS
           MOVE 0 TO WS-TOTAL-DEBITS
           MOVE 0 TO WS-TRANSACTION-COUNT
           MOVE WS-OPENING-BALANCE TO WS-CLOSING-BALANCE.

       WRITE-STATEMENT-HEADER.
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-2
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-3
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           
           MOVE AM-CUSTOMER-NAME TO CI1-NAME
           WRITE STATEMENT-LINE FROM CUSTOMER-INFO-1
           MOVE AM-ADDRESS-1 TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           MOVE AM-ADDRESS-2 TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           MOVE AM-CITY-STATE-ZIP TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           
           MOVE AM-ACCT-NUMBER TO CI2-ACCOUNT
           WRITE STATEMENT-LINE FROM CUSTOMER-INFO-2
           MOVE WS-STATEMENT-PERIOD TO CI3-PERIOD
           WRITE STATEMENT-LINE FROM CUSTOMER-INFO-3
           MOVE WS-FORMATTED-DATE TO CI4-DATE
           WRITE STATEMENT-LINE FROM CUSTOMER-INFO-4
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1.

       PROCESS-ACCOUNT-TRANSACTIONS.
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE FROM TRANSACTION-HEADER
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           WRITE STATEMENT-LINE FROM TXN-COLUMN-HEADER
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           
           MOVE 'N' TO WS-EOF-TXN
           PERFORM UNTIL WS-EOF-TXN = 'Y'
               READ TRANSACTION-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-TXN
                   NOT AT END
                       IF TR-ACCT-NUMBER = WS-CURRENT-ACCOUNT
                           PERFORM PROCESS-TRANSACTION
                       END-IF
               END-READ
           END-PERFORM.

       PROCESS-TRANSACTION.
           ADD 1 TO WS-TRANSACTION-COUNT
           
           EVALUATE TR-TYPE
               WHEN 'CREDIT'
                   ADD TR-AMOUNT TO WS-TOTAL-CREDITS
                   ADD TR-AMOUNT TO WS-CLOSING-BALANCE
               WHEN 'DEBIT '
                   ADD TR-AMOUNT TO WS-TOTAL-DEBITS
                   SUBTRACT TR-AMOUNT FROM WS-CLOSING-BALANCE
           END-EVALUATE
           
           MOVE TR-DATE TO TDL-DATE
           MOVE TR-DESCRIPTION TO TDL-DESCRIPTION
           MOVE TR-REFERENCE TO TDL-REFERENCE
           MOVE TR-AMOUNT TO TDL-AMOUNT
           MOVE WS-CLOSING-BALANCE TO TDL-BALANCE
           WRITE STATEMENT-LINE FROM TXN-DETAIL-LINE.

       CALCULATE-FINAL-BALANCE.
           IF WS-CLOSING-BALANCE < WS-MINIMUM-BALANCE
               SUBTRACT WS-SERVICE-CHARGE FROM WS-CLOSING-BALANCE
           END-IF.

       WRITE-BALANCE-SUMMARY.
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE FROM BALANCE-SUMMARY
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           
           MOVE 'Opening Balance:' TO BL-LABEL
           MOVE WS-OPENING-BALANCE TO BL-AMOUNT
           WRITE STATEMENT-LINE FROM BALANCE-LINE
           
           MOVE 'Total Credits:' TO BL-LABEL
           MOVE WS-TOTAL-CREDITS TO BL-AMOUNT
           WRITE STATEMENT-LINE FROM BALANCE-LINE
           
           MOVE 'Total Debits:' TO BL-LABEL
           MOVE WS-TOTAL-DEBITS TO BL-AMOUNT
           WRITE STATEMENT-LINE FROM BALANCE-LINE
           
           IF WS-CLOSING-BALANCE < WS-MINIMUM-BALANCE
               MOVE 'Service Charge:' TO BL-LABEL
               MOVE WS-SERVICE-CHARGE TO BL-AMOUNT
               WRITE STATEMENT-LINE FROM BALANCE-LINE
           END-IF
           
           MOVE 'Closing Balance:' TO BL-LABEL
           MOVE WS-CLOSING-BALANCE TO BL-AMOUNT
           WRITE STATEMENT-LINE FROM BALANCE-LINE.

       WRITE-FEES-SECTION.
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE FROM FEES-HEADER
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           
           IF WS-CLOSING-BALANCE < WS-MINIMUM-BALANCE
               MOVE 'Monthly service charge applied due to minimum'
                   TO NL-TEXT
               WRITE STATEMENT-LINE FROM NOTICE-LINE
               MOVE 'balance requirement not met.' TO NL-TEXT
               WRITE STATEMENT-LINE FROM NOTICE-LINE
           ELSE
               MOVE 'No fees assessed this period.' TO NL-TEXT
               WRITE STATEMENT-LINE FROM NOTICE-LINE
           END-IF.

       WRITE-NOTICES.
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           MOVE 'IMPORTANT NOTICES:' TO NL-TEXT
           WRITE STATEMENT-LINE FROM NOTICE-LINE
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           
           MOVE 'Please review your statement carefully and report'
               TO NL-TEXT
           WRITE STATEMENT-LINE FROM NOTICE-LINE
           MOVE 'any discrepancies within 30 days.' TO NL-TEXT
           WRITE STATEMENT-LINE FROM NOTICE-LINE
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           MOVE 'For customer service, call (555) 123-4567'
               TO NL-TEXT
           WRITE STATEMENT-LINE FROM NOTICE-LINE
           MOVE 'or visit us online at www.firstnationalbank.com'
               TO NL-TEXT
           WRITE STATEMENT-LINE FROM NOTICE-LINE.

       WRITE-STATEMENT-FOOTER.
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           MOVE 'END OF STATEMENT' TO NL-TEXT
           WRITE STATEMENT-LINE FROM NOTICE-LINE
           WRITE STATEMENT-LINE FROM STATEMENT-HEADER-1
           MOVE SPACES TO STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE
           WRITE STATEMENT-LINE.

       CLEANUP.
           CLOSE ACCOUNT-FILE
           CLOSE TRANSACTION-FILE
           CLOSE STATEMENT-FILE
           
           DISPLAY " "
           DISPLAY "Statement generation completed."
           DISPLAY "Output file: monthly_statements.txt"
           DISPLAY "========================================="
           DISPLAY "STATEMENT GENERATION JOB COMPLETED"
           DISPLAY "=========================================".
