IF OBJECT_ID('dbo.ACM_AML_DATA', 'U') IS NOT NULL
DROP TABLE dbo.ACM_AML_DATA;

CREATE TABLE ACM_AML_DATA(
    ID_ACM_AML_DATA BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	REFERENCE_CASE VARCHAR (1000) NOT NULL,
    REFERENCE_IN_FILE BIGINT NOT NULL,
    NAME VARCHAR (1000) NOT NULL,
    IDENTITY_NUMBER VARCHAR(256) NULL,
    DATE_OF_BIRTH VARCHAR(256) NULL,
    UPDATED_DATA VARCHAR(1000) NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);
 