--------- ALTER TABLE : ACM_LOAN
--------- ADD COLUMN : ASS_CUSTOMER - ACM-563
ALTER TABLE ACM_LOAN ADD ASSIGN_CUSTOMER bit ;

-------------
--ACM-
ALTER TABLE ACM_LOAN ADD LOAN_OWNER_NAME VARCHAR ( 1000 ) NULL ;

-------------
--ACM-693
ALTER TABLE ACM_PRODUCT ADD DECIMAL_PLACES INT NULL ; 
ALTER TABLE ACM_PRODUCT ADD CURRENCY VARCHAR ( 256 ) NULL ;
