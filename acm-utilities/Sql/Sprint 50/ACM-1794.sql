-- delete old SOAP config
DELETE FROM [ACM_ENVIRONNEMENT] WHERE [ACM_ENVIRONNEMENT_KEY]='TAMKEEN_SOAP_REQUEST_USER'
DELETE FROM [ACM_ENVIRONNEMENT] WHERE [ACM_ENVIRONNEMENT_KEY]='TAMKEEN_SOAP_REQUEST_PASS'
DELETE FROM [ACM_ENVIRONNEMENT] WHERE [ACM_ENVIRONNEMENT_KEY]='TAMKEEN_SOAP_REQUEST_PATH'

-- add new API config
INSERT INTO [ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION],CATEGORY) 
VALUES ('TAMKEEN_API_REQUEST_USER', 'mf001800010001heq', 1, '2021-11-23', 0, 'FUNCTIONAL');
INSERT INTO [ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION],CATEGORY) 
VALUES ('TAMKEEN_API_REQUEST_PASS', 'Pass@123', 1, '2021-11-23', 0, 'FUNCTIONAL');
INSERT INTO [ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION],CATEGORY) 
VALUES ('TAMKEEN_API_REQUEST_PATH', 'https://www.i-score.com.eg/SbxLiveRequest/api/LiveRequest/', 1, '2021-11-23', 0, 'FUNCTIONAL');
INSERT INTO [ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION],CATEGORY) 
VALUES ('TAMKEEN_API_REQUEST_API_KEY', 'sxpUhKRCJc6t06h0GMznol1SSfJELnjffKgFJtMy_1', 1, '2021-11-23', 0, 'FUNCTIONAL');
INSERT INTO [ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION],CATEGORY) 
VALUES ('TAMKEEN_API_REQUEST_PRODUCT_ID', '140001', 1, '2021-11-23', 0, 'FUNCTIONAL');


-- ALTER TABLE ACM_3RD_PARTY_HISTORIQUE
ALTER TABLE ACM_3RD_PARTY_HISTORIQUE
ADD 
TOTAL_APPROVAL_AMT VARCHAR (512) NULL,
TOTAL_MONTHLY_INSTALLMENT_AMT VARCHAR (512) NULL,
TOTAL_BALANCE_AMOUNT VARCHAR (512) NULL;