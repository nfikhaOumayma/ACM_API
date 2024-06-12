ALTER TABLE ACM_EXCEPTION_REQUEST add BRANCH_ID INT;

update  a set BRANCH_ID = 
(select branch_id from acm_customer where ID_ACM_CUSTOMER = a.CUSTOMER_ID) from   ACM_EXCEPTION_REQUEST a