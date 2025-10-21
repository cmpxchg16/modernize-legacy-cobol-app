# Batch Processing Guide

## Overview

The batch processing module allows you to process multiple account transactions from a file in a single run, rather than entering them interactively. This is useful for:

- End-of-day transaction processing
- Bulk imports from external systems
- Automated reconciliation processes
- Testing with large datasets

## Architecture

The batch processing system consists of:

- **batch.cob**: Main batch processor program
- **batch_transactions.dat**: Input file containing transactions to process
- **batch_report.txt**: Output report generated after processing
- **data.cob**: Shared data storage module (reused from interactive system)

## File Format

### Input File (batch_transactions.dat)

The input file uses fixed-format records with the following structure:

```
Position  Length  Field           Description
1-6       6       Transaction Type CREDIT, DEBIT, or TOTAL (padded with spaces)
7-14      8       Amount          Format: 9(6)V99 - 8 digits, NO decimal point (e.g., 00050000 = $500.00)
15-24     10      Transaction ID  Unique identifier (e.g., TXN0000001)
```

**Example Records:**
```
CREDIT00050000TXN0000001
DEBIT 00030000TXN0000002
TOTAL 00000000TXN0000003
```

**Important:** The amount field uses an implied decimal point (V in COBOL). The last 2 digits represent cents:
- `00050000` = $500.00
- `00030000` = $300.00
- `00007550` = $75.50

**Transaction Types:**
- `CREDIT` - Add funds to the account
- `DEBIT ` - Withdraw funds from the account (note: requires trailing space to be 6 chars)
- `TOTAL ` - Query current balance (note: requires trailing space to be 6 chars)

**Important Notes:**
- Transaction types must be exactly 6 characters (pad with spaces if needed)
- Amounts must be exactly 8 digits with NO decimal point (implied decimal after 6th digit)
- Transaction IDs must be exactly 10 characters
- No delimiters or separators between fields
- All fields are fixed-width and must be padded to their exact length

## Compilation

Compile the batch processor along with the data module:

```bash
cobc -x batch.cob data.cob -o batchprocessor
```

## Running Batch Processing

1. **Prepare your input file** (`batch_transactions.dat`) with the transactions to process

2. **Run the batch processor**:
   ```bash
   ./batchprocessor
   ```

3. **Review the output**:
   - Console output shows real-time processing status
   - `batch_report.txt` contains detailed transaction log and summary

## Output Report

The batch processor generates a comprehensive report including:

### Transaction Details
- Transaction ID
- Transaction type
- Amount
- Status (SUCCESS, ERROR, INSUF-FND)

### Summary Statistics
- Total transactions processed
- Successful transactions
- Error count
- Total credits processed
- Total debits processed
- Final account balance

### Sample Report Output

```
================================================================================
                         BATCH PROCESSING REPORT
================================================================================

TXN ID      TYPE      AMOUNT         STATUS
================================================================================

TXN0000001  TOTAL     001,000.00 SUCCESS
TXN0000002  CREDIT    000,500.00 SUCCESS
TXN0000003  CREDIT    000,250.00 SUCCESS
TXN0000004  TOTAL     001,750.00 SUCCESS
TXN0000005  DEBIT     000,300.00 SUCCESS
TXN0000006  CREDIT    001,000.00 SUCCESS
TXN0000007  DEBIT     000,100.00 SUCCESS
TXN0000008  TOTAL     002,350.00 SUCCESS
TXN0000009  DEBIT     005,000.00 INSUF-FND
TXN0000010  CREDIT    000,075.50 SUCCESS

================================================================================
BATCH PROCESSING SUMMARY
================================================================================

Total Transactions:                     10
Successful:                              9
Errors:                                  1

Total Credits:                   001,825.50
Total Debits:                    000,400.00
Final Balance:                   002,425.50
```

## Error Handling

The batch processor handles the following error conditions:

1. **Insufficient Funds**: When a DEBIT transaction exceeds the current balance
   - Status: `INSUF-FND`
   - Transaction is rejected, balance remains unchanged

2. **Invalid Transaction Type**: When the transaction type is not recognized
   - Status: `ERROR`
   - Transaction is skipped

3. **File Not Found**: If `batch_transactions.dat` doesn't exist
   - Program terminates with file error

## Integration with Interactive System

The batch processor shares the same data storage module (`data.cob`) as the interactive system (`main.cob`). This means:

- Batch processing updates the same account balance used by the interactive system
- You can run batch processing and then check the balance interactively
- The initial balance is 1000.00 (same as interactive system)

## Example Usage Scenario

### Scenario: End-of-Day Processing

1. **Create batch file** with the day's transactions:
   ```
   CREDIT00050000TXN2024001
   CREDIT00025000TXN2024002
   DEBIT 00030000TXN2024003
   CREDIT00100000TXN2024004
   ```

2. **Run batch processor**:
   ```bash
   ./batchprocessor
   ```

3. **Console output**:
   ```
   Batch processing started...
   
   Processed: TXN2024001 - CREDIT 000500.00 - New Balance: 001500.00
   Processed: TXN2024002 - CREDIT 000250.00 - New Balance: 001750.00
   Processed: TXN2024003 - DEBIT 000300.00 - New Balance: 001450.00
   Processed: TXN2024004 - CREDIT 001000.00 - New Balance: 002450.00
   
   Batch processing completed!
   Total transactions: 4
   Successful: 4
   Errors: 0
   Report generated: batch_report.txt
   ```

4. **Review report** in `batch_report.txt` for audit trail

## Best Practices

1. **Backup**: Always keep a backup of your batch input file before processing
2. **Validation**: Validate your batch file format before running large batches
3. **Testing**: Test with a small sample file first
4. **Audit**: Review the generated report for any errors or unexpected results
5. **Reconciliation**: Compare batch totals with expected values

## Troubleshooting

### Problem: File not found error
**Solution**: Ensure `batch_transactions.dat` exists in the same directory as the executable

### Problem: All transactions show ERROR status
**Solution**: Check that transaction types are exactly 6 characters (pad with spaces)

### Problem: Amounts not processing correctly
**Solution**: Verify amounts are 8 digits with NO decimal point (e.g., 00050000 for $500.00, not 000500.00)

### Problem: Balance doesn't match expectations
**Solution**: Remember the initial balance is 1000.00; check if previous runs affected the balance

## Extending the Batch Processor

The batch processor can be extended to support:

- Multiple account processing
- Transaction validation rules
- Duplicate transaction detection
- Date/time stamping
- Error recovery and retry logic
- Integration with external systems
