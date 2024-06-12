
-----------------
-- ACM_PRODUCT_CATEGORY
-----------------
IF OBJECT_ID('dbo.ACM_PRODUCT_CATEGORY', 'U') IS NOT NULL
DROP TABLE dbo.ACM_PRODUCT_CATEGORY;

CREATE TABLE ACM_PRODUCT_CATEGORY(
    ID_ACM_PRODUCT_CATEGORY BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CODE VARCHAR (512) NOT NULL,
	DESCRIPTION VARCHAR (512) NOT NULL,
	PRODUCT_ID_LIST VARCHAR (512) NOT NULL,
	INCENTIVE_REPAYMENT BIT,
	INCENTIVE_REGESTRATION BIT,
	INCENTIVE_OPERATION BIT,
	INCENTIVE_LEGAL BIT,
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
);
-----------------
-- ACM_INCENTIVE_SETTING_CONSTANT 
-----------------
IF OBJECT_ID('dbo.ACM_INCENTIVE_SETTING', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_SETTING;

IF OBJECT_ID('dbo.ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL;

IF OBJECT_ID('dbo.ACM_INCENTIVE_SETTING_RUN', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_SETTING_RUN;

IF OBJECT_ID('dbo.ACM_INCENTIVE_REPAYMENT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_REPAYMENT;

IF OBJECT_ID('dbo.ACM_INCENTIVE_REGESTRATION', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_REGESTRATION;

IF OBJECT_ID('dbo.ACM_INCENTIVE_OPERATION', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_OPERATION;

IF OBJECT_ID('dbo.ACM_INCENTIVE_LEGAL', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_LEGAL;

IF OBJECT_ID('dbo.ACM_INCENTIVE_SETTING_CONSTANT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_SETTING_CONSTANT;

CREATE TABLE ACM_INCENTIVE_SETTING_CONSTANT(
    ID_ACM_INCENTIVE_SETTING_CONSTANT BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CATEGORY VARCHAR (512) NOT NULL, -- ACM_INCENTIVE_REPAYMENT / ACM_INCENTIVE_REGESTRATION / ACM_INCENTIVE_OPERATION / "FREQUENCY", "INCENTIVE_SETTING_TYPE", "INCENTIVE_REGESTRATION_CUSTOMER_TYPE
	CODE VARCHAR (512) NOT NULL,
	DESCRIPTION VARCHAR (512) NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
);
-----------------
-- ACM_INCENTIVE_SETTING : ACTIVE_CUSTOMER / PRODUCTIVITY / RISK_LEVEL / DISCOUNT_FROM_TOTAL
-----------------
CREATE TABLE ACM_INCENTIVE_SETTING(
    ID_ACM_INCENTIVE_SETTING BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	PRODUCT_ID BIGINT NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	CATEGORY VARCHAR (512) NOT NULL, --ACTIVE_CUSTOMER / PRODUCTIVITY / RISK_LEVEL / DISCOUNT_FROM_TOTAL
	INCENTIVE_SETTING_FROM DECIMAL(16,2) NOT NULL,
	INCENTIVE_SETTING_TO DECIMAL(16,2) NOT NULL,
	DISCOUNT BIGINT NULL,
	ORDRE BIGINT NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);
-----------------
-- ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL  
-----------------
CREATE TABLE ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL(
    ID_ACM_INCENTIVE_SETTING_BRANCH_PROD_LEVEL BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	PRODUCT_ID BIGINT NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	INCENTIVE_ROLE VARCHAR (512) NOT NULL, 
	MIN_AMOUNT DECIMAL(16,4) NOT NULL,
	MIN_NUMBER_CUSTOMER BIGINT NOT NULL,
	ORDRE BIGINT NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);
-----------------
-- ACM_INCENTIVE_SETTING_RUN 
-----------------
CREATE TABLE ACM_INCENTIVE_SETTING_RUN(
    ID_ACM_INCENTIVE_SETTING_RUN BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CODE VARCHAR (512) NOT NULL,
	DESCRIPTION VARCHAR (512) NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	INCENTIVE_ROLE VARCHAR (1000) NOT NULL,
	APPLAY_DISCOUNT_RULE BIT NULL,
	APPLAY_BRANCH_PROD_LEVEL BIT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,

	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);
-----------------
-- ACM_INCENTIVE_REPAYMENT  
-----------------
CREATE TABLE ACM_INCENTIVE_REPAYMENT(
    ID_ACM_INCENTIVE_REPAYMENT BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	PRODUCT_ID BIGINT NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	INCENTIVE_ROLE VARCHAR (512) NOT NULL,  
	ACTIVE_CUSTOMER_ID BIGINT NOT NULL,
	PRODUCTIVITY_ID BIGINT NOT NULL,
	RISK_LEVEL_ID BIGINT NOT NULL,
	INCENTIVE_TYPE_ID BIGINT NOT NULL, 
	INCENTIVE_VALUE VARCHAR (512) NOT NULL, 
	BASED_ON_ID BIGINT NOT NULL,
	ORDRE BIGINT NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (BASED_ON_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (INCENTIVE_TYPE_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);
-----------------
-- ACM_INCENTIVE_REGESTRATION  
-----------------
CREATE TABLE ACM_INCENTIVE_REGESTRATION(
    ID_ACM_INCENTIVE_REGESTRATION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	PRODUCT_ID BIGINT NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	INCENTIVE_ROLE VARCHAR (512) NOT NULL,
	CUSTOMER_TYPE_ID BIGINT NOT NULL,  -- NEW / RENEW
	INCENTIVE_TYPE_ID BIGINT NOT NULL, 
	INCENTIVE_VALUE VARCHAR (512) NOT NULL,
	BASED_ON_ID BIGINT NOT NULL,
	ORDRE BIGINT NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (BASED_ON_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (INCENTIVE_TYPE_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (CUSTOMER_TYPE_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);

-----------------
-- ACM_INCENTIVE_OPERATION  
-----------------
CREATE TABLE ACM_INCENTIVE_OPERATION(
    ID_ACM_INCENTIVE_OPERATION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	PRODUCT_ID BIGINT NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	INCENTIVE_ROLE VARCHAR (512) NOT NULL,  
	INCENTIVE_TYPE_ID BIGINT NOT NULL, 
	INCENTIVE_VALUE VARCHAR (512) NOT NULL,
	BASED_ON_ID BIGINT NOT NULL,
	ORDRE BIGINT NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (BASED_ON_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT),
	FOREIGN KEY (INCENTIVE_TYPE_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);

-----------------
-- ACM_INCENTIVE_LEGAL 
-----------------
CREATE TABLE ACM_INCENTIVE_LEGAL(
    ID_ACM_INCENTIVE_LEGAL BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	PRODUCT_ID BIGINT NOT NULL,
	FREQUENCY_ID BIGINT NOT NULL,
	ORDRE BIGINT NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (FREQUENCY_ID) REFERENCES ACM_INCENTIVE_SETTING_CONSTANT (ID_ACM_INCENTIVE_SETTING_CONSTANT)
);


IF OBJECT_ID('dbo.ACM_INCENTIVE_RUN_OPERATION', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_RUN_OPERATION;
CREATE TABLE [dbo].[ACM_INCENTIVE_RUN_OPERATION](
	[ID_ACM_INCENTIVE_RUN_OPERATION] BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	[REPORT_NAME] [NVARCHAR](256),
	[ROLE] [NVARCHAR](256),
	[LOAN_OFFICER_NAME] [NVARCHAR](256),
	[USERNAME] [NVARCHAR](256),
	[BRANCH] [NVARCHAR](256),
	[ISSUE_LOAN_MONTH_MEL] [bigint],
	[TOTALE_LOAN_AMOUNT_MEL] [bigint],
	[INCENTIVE_TYPE_MEL] [NVARCHAR](256),
	[INCENTIVE_VALUE_MEL] [bigint],
	[ISSUE_LOAN_MONTH_VSE] [bigint],
	[TOTALE_LOAN_AMOUNT_VSE] [bigint],
	[INCENTIVE_TYPE_VSE] [NVARCHAR](256),
	[INCENTIVE_VALUE_VSE] [bigint],
	[INCENTIVE_VALUE] [bigint],
	[MONTH][NVARCHAR](256),
	[RUN_DATE] datetime
)

IF OBJECT_ID('dbo.ACM_INCENTIVE_RUN_REGISTRATION', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_RUN_REGISTRATION;
CREATE TABLE [dbo].[ACM_INCENTIVE_RUN_REGISTRATION](
	[ID_ACM_INCENTIVE_RUN_REGISTRATION] [bigint] IDENTITY(1,1) NOT NULL PRIMARY KEY,
	[REPORT_NAME] [NVARCHAR](50),
	[LOAN_OFFICER_NAME] [NVARCHAR](50),
	[USERNAME] [NVARCHAR](50),
	[BRANCH] [NVARCHAR](50),
	
	[TOTALE_CUSTOMER_MEL] [bigint],
	[NEW_CUSTOMER_MEL] [bigint],
	[RENEWAL_CUSTOMER_MEL] [bigint],
	[SETTING_INCENTIVE_NEW_CUSTOMER_MEL] [bigint],
	[SETTING_INCENTIVE_RENEWAL_CUSTOMER_MEL] [bigint],
	[INCENTIVE_NEW_CUSTOMER_MEL] [bigint],
	[INCENTIVE_RENEWAL_CUSTOMER_MEL] [bigint],
	
	[TOTALE_CUSTOMER_VSE] [bigint],
	[NEW_CUSTOMER_VSE] [bigint],
	[RENEWAL_CUSTOMER_VSE] [bigint],
	[SETTING_INCENTIVE_NEW_CUSTOMER_VSE] [bigint],
	[SETTING_INCENTIVE_RENEWAL_CUSTOMER_VSE] [bigint],
	[INCENTIVE_NEW_CUSTOMER_VSE] [bigint],
	[INCENTIVE_RENEWAL_CUSTOMER_VSE] [bigint],

	[INCENTIVE_VALUE] [bigint],

	[MONTH] varchar(50),
	[RUN_DATE] date
)
DROP INDEX IF EXISTS INDEX_LOAN_PORTFOLIO_ID
ON ACM_LOAN;

CREATE INDEX INDEX_LOAN_PORTFOLIO_ID
ON ACM_LOAN(PORTFOLIO_ID); 

DROP INDEX IF EXISTS INDEX_LOAN_HISTORIQUE_ACTION_DATE_UPDATE
ON ACM_LOAN_HISTORIQUE;

CREATE INDEX INDEX_LOAN_HISTORIQUE_ACTION_DATE_UPDATE
ON ACM_LOAN_HISTORIQUE(ACTION, DATE_UPDATE);

IF OBJECT_ID('dbo.ACM_INCENTIVE_RUN_REPAYMENT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_INCENTIVE_RUN_REPAYMENT;
CREATE TABLE [dbo].[ACM_INCENTIVE_RUN_REPAYMENT](
	[ID_ACM_INCENTIVE_RUN_REPAYMENT] [bigint] IDENTITY(1,1) NOT NULL PRIMARY KEY,
	[REPORT_NAME] [NVARCHAR](256),
	[ROLE] [NVARCHAR](256),
	[LOAN_OFFICER_NAME] [NVARCHAR](256),
	[LOAN_SUPERVISOR] [NVARCHAR](256),
	[LOAN_BRANCH_MANAGER] [NVARCHAR](256),
	[USERNAME] [NVARCHAR](256),

	[BRANCH] [NVARCHAR](256),
	[PRODUCT_CATEGORY_ID] [bigint],
	[PRODUCT_LIST_ID] [NVARCHAR](256),
	[PRODUCT_CATEGORY_DESCRIPTION] [NVARCHAR] (256),
	
	[ACTIVE_CUSTOMER] [bigint],
	[TOTALE_LOAN_AMOUNT] [bigint],
	[ISSUE_LOAN_MONTH] [bigint],
	[MONTH][NVARCHAR](256),
	[BALANCE_PAID] [bigint],
	[BALANCE_NOT_PAID] [bigint],
	[RISK] [float],
	[REPAYMENT_PAID] [bigint],
	[REPAYMENT_NOT_PAID] [bigint],
	[REPAYMENT_RATE] [bigint],
	
	[INCENTIVE_VALUE_SETTING] [bigint],
	[BASED_ON] [NVARCHAR](256),
	[PRODUCTIVITY] [bit],
	[DISCOUNT] [bit],
	[INCENTIVE_VALUE_NO_DISCOUNT] [bigint],
	[INCENTIVE_VALUE] [bigint],
	[RUN_DATE] datetime
)
