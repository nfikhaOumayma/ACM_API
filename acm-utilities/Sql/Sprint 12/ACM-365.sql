IF OBJECT_ID('dbo.ACM_CUSTOMER', 'U') IS NOT NULL 
DROP TABLE dbo.ACM_CUSTOMER

-------------
----- TABLE : ACM_CUSTOMER 
CREATE TABLE ACM_CUSTOMER(
  ID_ACM_CUSTOMER BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,  
  CUSTOMER_ID_EXTERN BIGINT NOT NULL,
  CUSTOMER_NAME varchar(512),
  CUSTOMER_OPENDATE DATE NULL,
  CORRESPONDENCE_NAME varchar(512),
  CUSTOMER_NUMBER varchar(512),
  ALT_NAME varchar(512),
  DATE_OF_BIRTH DATE NULL,
  ACCOUNT_PORTFOLIO_ID BIGINT NOT NULL,
  PORTFOLIO_CODE varchar(512),
  PORTFOLIO_DESCRIPTION varchar(512),
  CUSTOMER_ADDRESS varchar(512),
  AGE BIGINT null,
  TELEPHONE_1 varchar(256) null,
  TELEPHONE_2 varchar(256) null,
  TELEPHONE_3 varchar(256) null,
  
  BRANCH_ID int not null default 0,
  BRANCHE_NAME varchar(512),
  BRANCHE_DESCRIPTION varchar(512),
	
  ACM_ENABLED BIT NOT NULL,
  DATE_INSERTION DATETIME NULL,
  INSERT_BY  VARCHAR (256) NULL,
  DATE_LAST_UPDATE DATETIME NULL,
  UPDATED_BY  VARCHAR (256) NULL,
  ACM_VERSION INT NULL
);

ALTER TABLE [dbo].[ACM_CUSTOMER]
ALTER COLUMN [ALT_NAME] VARCHAR (512) COLLATE Arabic_CI_AI_KS_WS;

----------------------------------
----- TABLE : ACM_LOAN
ALTER TABLE ACM_LOAN ADD ID_ACM_CUSTOMER BIGINT NULL;

ALTER TABLE ACM_LOAN ADD FOREIGN KEY (ID_ACM_CUSTOMER) REFERENCES ACM_CUSTOMER(ID_ACM_CUSTOMER);