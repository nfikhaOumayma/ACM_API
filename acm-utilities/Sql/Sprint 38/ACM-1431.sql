--- ISSUE FEE AMOUNT DECIMAL 
ALTER TABLE ACM_LOAN ALTER COLUMN  ISSUE_FEE_AMOUNT DECIMAL(16,4)
--- EFFECTIVE_INT_RATE DECIMAL
ALTER TABLE ACM_LOAN ALTER COLUMN  EFFECTIVE_INT_RATE DECIMAL(18,6)
--- APR DECIMAL
ALTER TABLE ACM_LOAN ALTER COLUMN  APR DECIMAL(18,6)