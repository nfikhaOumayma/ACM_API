-----------------
-- ACM_INCENTIVE_SETTING_CONSTANT :
-- ACM_INCENTIVE_REPAYMENT / ACM_INCENTIVE_REGESTRATION / ACM_INCENTIVE_OPERATION / ACM_INCENTIVE_LEGAL /  FREQUENCY / INCENTIVE_SETTING_TYPE / INCENTIVE_REGESTRATION_CUSTOMER_TYPE
-----------------
INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM_INCENTIVE_REPAYMENT','TOTAL_COLLECTED' ,'Total collected',1,GETDATE(),'ADMIN',0); 
INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM_INCENTIVE_REPAYMENT','ISSUED_LOANS' ,'Issued loans',1,GETDATE(),'ADMIN',0); 


INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM_INCENTIVE_REGESTRATION','PASSED_LOANS' ,'Passed loans',1,GETDATE(),'ADMIN',0); 


INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM_INCENTIVE_OPERATION','NB_ISSUED_LOANS' ,'NB Issued loans',1,GETDATE(),'ADMIN',0); 
INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM_INCENTIVE_OPERATION','TOTAL_ISSUED' ,'Total Issued',1,GETDATE(),'ADMIN',0); 

INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('FREQUENCY','MONTHLY' ,'Monthly',1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('FREQUENCY','DAILY' ,'Daily',1,GETDATE(),'ADMIN',0);

INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('INCENTIVE_SETTING_TYPE','FIXED' ,7,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('INCENTIVE_SETTING_TYPE','PERCENTAGE' ,8,1,GETDATE(),'ADMIN',0);

INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('INCENTIVE_REGESTRATION_CUSTOMER_TYPE_ID',9 ,9,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING_CONSTANT] ([CATEGORY],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('INCENTIVE_REGESTRATION_CUSTOMER_TYPE_ID',10 ,10,1,GETDATE(),'ADMIN',0);

-----------------
-- ACM_PRODUCT_CATEGORY 
-----------------
INSERT INTO  [ACM_PRODUCT_CATEGORY] ([PRODUCT_ID_LIST],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('1,2','MEL' ,'Categorie MEL for product MEL_1 & MEL_2',1,GETDATE(),'ADMIN',0); 
INSERT INTO  [ACM_PRODUCT_CATEGORY] ([PRODUCT_ID_LIST],[CODE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('3,4','VSE' ,'Categorie VSE for product VSE_1 & VSE_2',1,GETDATE(),'ADMIN',0); 
-----------------
-- ACM_INCENTIVE_SETTING : ACTIVE_CUSTOMER / PRODUCTIVITY / RISK_LEVEL / DISCOUNT_FROM_TOTAL
-----------------
 
 -- ACTIVE_CUSTOMER 
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'ACTIVE_CUSTOMER',1 ,50 ,1,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'ACTIVE_CUSTOMER',51 ,100 ,2,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'ACTIVE_CUSTOMER',101 ,150 ,3,1,GETDATE(),'ADMIN',0);

-- PRODUCTIVITY 
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'PRODUCTIVITY',7 ,10,1,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'PRODUCTIVITY',11 ,15,2,1,GETDATE(),'ADMIN',0);

-- RISK_LEVEL 
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'RISK_LEVEL',0,1,1,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO] ,[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'RISK_LEVEL',1,2,2,1,GETDATE(),'ADMIN',0);

-- DISCOUNT_FROM_TOTAL
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO],[DISCOUNT],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'DISCOUNT_FROM_TOTAL',0,95 ,100 ,1,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO],[DISCOUNT],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'DISCOUNT_FROM_TOTAL',95,98 ,50 ,2,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO],[DISCOUNT],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'DISCOUNT_FROM_TOTAL',98,99 ,25 ,3,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO],[DISCOUNT],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'DISCOUNT_FROM_TOTAL',98,99.5 ,10 ,4,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO],[DISCOUNT],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'DISCOUNT_FROM_TOTAL',99.5,100,0 ,5,1,GETDATE(),'ADMIN',0);
INSERT INTO  [ACM_INCENTIVE_SETTING]([PRODUCT_ID],[FREQUENCY_ID],[CATEGORY],[INCENTIVE_SETTING_FROM],[INCENTIVE_SETTING_TO],[DISCOUNT],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1 ,6,'DISCOUNT_FROM_TOTAL',100,999 ,0 ,6,1,GETDATE(),'ADMIN',0);

-----------------
-- ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL  
-----------------
INSERT INTO [ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL]([PRODUCT_ID],[FREQUENCY_ID],[INCENTIVE_ROLE],[MIN_AMOUNT],[MIN_NUMBER_CUSTOMER],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'SUPERVISOR',437500,35,1,1,GETDATE(),'ADMIN',0);
INSERT INTO [ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL]([PRODUCT_ID],[FREQUENCY_ID],[INCENTIVE_ROLE],[MIN_AMOUNT],[MIN_NUMBER_CUSTOMER],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'BRANCH_MANAGER',876000,70,2,1,GETDATE(),'ADMIN',0); 


-----------------
-- ACM_INCENTIVE_REPAYMENT  
-----------------
INSERT INTO [ACM_INCENTIVE_REPAYMENT]([PRODUCT_ID],[FREQUENCY_ID],[INCENTIVE_ROLE],[ACTIVE_CUSTOMER_ID],[PRODUCTIVITY_ID],[RISK_LEVEL_ID],[INCENTIVE_TYPE_ID],[INCENTIVE_VALUE],[BASED_ON_ID],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'LOAN_OFFICER',1,1,1,7,70,1,1,1,GETDATE(),'ADMIN',0);
INSERT INTO [ACM_INCENTIVE_REPAYMENT]([PRODUCT_ID],[FREQUENCY_ID],[INCENTIVE_ROLE],[ACTIVE_CUSTOMER_ID],[PRODUCTIVITY_ID],[RISK_LEVEL_ID],[INCENTIVE_TYPE_ID],[INCENTIVE_VALUE],[BASED_ON_ID],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'LOAN_OFFICER',1,1,2,7,0,2,2,1,GETDATE(),'ADMIN',0);
INSERT INTO [ACM_INCENTIVE_REPAYMENT]([PRODUCT_ID],[FREQUENCY_ID],[INCENTIVE_ROLE],[ACTIVE_CUSTOMER_ID],[PRODUCTIVITY_ID],[RISK_LEVEL_ID],[INCENTIVE_TYPE_ID],[INCENTIVE_VALUE],[BASED_ON_ID],[ORDRE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES (1,6,'LOAN_OFFICER',1,1,3,7,0.006,1,3,1,GETDATE(),'ADMIN',0);

-----------------
-- ACM_INCENTIVE_REGESTRATION  
-----------------
INSERT INTO [ACM_INCENTIVE_REGESTRATION]  ([PRODUCT_ID] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE] ,[CUSTOMER_TYPE_ID] ,[INCENTIVE_TYPE_ID] ,[INCENTIVE_VALUE] ,[BASED_ON_ID] ,[ORDRE] ,[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES (1,6,'LOAN_OFFICER',9 ,7,30 ,3 ,1 ,1,GETDATE(),'ADMIN',0);
INSERT INTO [ACM_INCENTIVE_REGESTRATION]  ([PRODUCT_ID] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE] ,[CUSTOMER_TYPE_ID] ,[INCENTIVE_TYPE_ID] ,[INCENTIVE_VALUE] ,[BASED_ON_ID] ,[ORDRE] ,[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES (1,6,'LOAN_OFFICER',10 ,7,35 ,3 ,2 ,1,GETDATE(),'ADMIN',0);

-----------------
-- ACM_INCENTIVE_OPERATION  
-----------------
INSERT INTO [ACM_INCENTIVE_OPERATION]  ([PRODUCT_ID] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE]  ,[INCENTIVE_TYPE_ID] ,[INCENTIVE_VALUE] ,[BASED_ON_ID] ,[ORDRE] ,[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES (1,6,'BRANCH_AUDITOR' ,7,5 ,4 ,1 ,1,GETDATE(),'ADMIN',0);
INSERT INTO [ACM_INCENTIVE_OPERATION]  ([PRODUCT_ID] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE]  ,[INCENTIVE_TYPE_ID] ,[INCENTIVE_VALUE] ,[BASED_ON_ID] ,[ORDRE] ,[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES (1,6,'BRANCH_AUDITOR' ,7,10 ,4 ,1 ,1,GETDATE(),'ADMIN',0);
INSERT INTO [ACM_INCENTIVE_OPERATION]  ([PRODUCT_ID] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE]  ,[INCENTIVE_TYPE_ID] ,[INCENTIVE_VALUE] ,[BASED_ON_ID] ,[ORDRE] ,[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES (1,6,'BRANCH_AUDITOR' ,8,0.001 ,5 ,1 ,1,GETDATE(),'ADMIN',0); 

-----------------
-- ACM_INCENTIVE_LEGAL 
-----------------


-----------------
-- ACM_INCENTIVE_SETTING_RUN 
----------------- 

--ACM_INCENTIVE_REPAYMENT 
INSERT INTO [ACM_INCENTIVE_SETTING_RUN] ([CODE] ,[DESCRIPTION] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE] ,[APPLAY_DISCOUNT_RULE],[APPLAY_BRANCH_PROD_LEVEL],[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES ('ACM_INCENTIVE_REPAYMENT' ,'Issuance and Repayment Incentive' ,6,'IT_ADMINISTRATOR',1,1 ,1,GETDATE(),'ADMIN',0); 
--ACM_INCENTIVE_REGESTRATION 
INSERT INTO [ACM_INCENTIVE_SETTING_RUN] ([CODE] ,[DESCRIPTION] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE] ,[APPLAY_DISCOUNT_RULE],[APPLAY_BRANCH_PROD_LEVEL],[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES ('ACM_INCENTIVE_REGESTRATION' ,'Registration Incentives' ,11,'IT_ADMINISTRATOR, TELLER',1,1 ,1,GETDATE(),'ADMIN',0); 
-- ACM_INCENTIVE_OPERATION
INSERT INTO [ACM_INCENTIVE_SETTING_RUN] ([CODE] ,[DESCRIPTION] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE] ,[APPLAY_DISCOUNT_RULE],[APPLAY_BRANCH_PROD_LEVEL],[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES ('ACM_INCENTIVE_OPERATION' ,'Operations Incentives' ,6,'IT_ADMINISTRATOR',1,1 ,1,GETDATE(),'ADMIN',0); 
--ACM_INCENTIVE_LEGAL 
INSERT INTO [ACM_INCENTIVE_SETTING_RUN] ([CODE] ,[DESCRIPTION] ,[FREQUENCY_ID] ,[INCENTIVE_ROLE] ,[APPLAY_DISCOUNT_RULE],[APPLAY_BRANCH_PROD_LEVEL],[ACM_ENABLED] ,[DATE_INSERTION] ,[INSERT_BY] ,[ACM_VERSION])
VALUES ('ACM_INCENTIVE_LEGAL' ,'Legal Incentives' ,11,'IT_ADMINISTRATOR, TELLER',1,1 ,1,GETDATE(),'ADMIN',0); 