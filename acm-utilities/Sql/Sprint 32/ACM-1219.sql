alter table acm_product add ISSUEFEEAMOUNT1 decimal(16,4), 
ISSUEFEEAMOUNT2 decimal(16,4);
ALTER TABLE acm_product
ALTER COLUMN [ISSUE_FEE_PERCENTAGE_1] decimal(9,6);
ALTER TABLE acm_product
ALTER COLUMN[ISSUE_FEE_PERCENTAGE_2] decimal(9,6);
ALTER TABLE acm_product
ALTER COLUMN[ISSUE_FEE_PERCENTAGE_3] decimal(9,6);
ALTER TABLE acm_product
ALTER COLUMN [ISSUE_FEE_PERCENTAGE_4] decimal(9,6);
alter table acm_product add FLAT_INTEREST_RATE decimal(16,4);
 