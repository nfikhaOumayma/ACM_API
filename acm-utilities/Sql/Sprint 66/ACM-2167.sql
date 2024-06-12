-- TABLE COLLETION
CREATE TABLE ACM_COLLECTION (
ID_ACM_COLLECTION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
CUSTOMER_TYPE VARCHAR (10) NOT NULL,
CUSTOMER_ID_EXTERN BIGINT NOT NULL,
ID_LOAN_EXTERN BIGINT NOT NULL,
ACCOUNT_NUMBER VARCHAR (256) NOT NULL,
PRODUCT_DESCRIPTION VARCHAR (512) NOT NULL,
PRODUCT_ID INT NOT NULL,
CUSTOMER_NAME varchar(512) NOT NULL,
BRANCH_DESCRIPTION  VARCHAR (256) NOT NULL,
BRANCH_ID INT NOT NULL,
CURRENCY_DECIMALPLACES INT NULL,
CURRENCY_SYMBOL VARCHAR(10) NULL,
AMOUNT DECIMAL(16,4) NOT NULL,
LOAN_OFFICER VARCHAR(50) NOT NULL,
FIRST_UNPAID_INSTALLMENT DATETIME NOT NULL,
UNPAID_AMOUNT DECIMAL(16,4) NOT NULL,
LATE_DAYS INT NOT NULL,
NUMBER_OF_UNPAID_INSTALLMENT INT NOT NULL,
STATUS VARCHAR(50)  NOT NULL,
ID_ACM_COLLECTION_STEP VARCHAR(50) NULL,
AVAILABLE_DATE [date] NULL,
COLLECTION_OWNER VARCHAR(256) NULL,
COLLECTION_OWNER_NAME VARCHAR(256) NULL,
GROUP_OWNER VARCHAR(256) NULL,
GROUP_OWNER_NAME VARCHAR(256) NULL,

ACM_ENABLED BIT NOT NULL,
DATE_INSERTION DATETIME NULL,
INSERT_BY  VARCHAR (256) NULL,
DATE_LAST_UPDATE DATETIME NULL,
UPDATED_BY  VARCHAR (256) NULL,
ACM_VERSION INT NULL);

-- TABLE COLLECTION INSTANCE
 CREATE TABLE ACM_COLLECTION_INSTANCE(
	ID_ACM_COLLECTION_INSTANCE BIGINT IDENTITY(1,1) NOT NULL PRIMARY KEY,
	ID_ACM_COLLECTION BIGINT NOT NULL,
	ID_ACM_COLLECTION_STEP INT NOT NULL,
	LIBELLE VARCHAR(512) NULL,
	DESCRIPTION VARCHAR(512) NULL,
	CODE_STATUT_COLLECTION BIGINT NOT NULL,
	STATUT_COLLECTION VARCHAR(512) NULL,
	IHM_WEB_ROOT VARCHAR(512) NULL,
	CLIENT VARCHAR(256) NULL,
	ORDER_ETAPE_PROCESS INT NULL,
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY VARCHAR(256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY VARCHAR(256) NULL,
	ACM_VERSION INT NULL,
	START_DATE INT NULL,
	AFTER_DATE VARCHAR(256) NULL,
	STEP_NAME VARCHAR(256) NULL,
	ACTION_USER VARCHAR(50) NULL);


-- cron schedule every night at midnight
INSERT INTO [DBO].[ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION]) 
VALUES ('CRON_EXPRESSION_COLLECTION', '0 0/0 0 * * *', 1, '2022-10-27', 0);