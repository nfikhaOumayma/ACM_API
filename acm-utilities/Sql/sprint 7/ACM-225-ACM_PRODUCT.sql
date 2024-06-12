IF OBJECT_ID('dbo.ACM_PRODUCT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_PRODUCT;

CREATE TABLE ACM_PRODUCT(
    ID_ACM_PRODUCT BIGINT NOT NULL PRIMARY KEY,
	CODE VARCHAR(50) NOT NULL,
	DESCRIPTION VARCHAR(512) NOT NULL ,

	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);

ALTER TABLE [ACM].[dbo].[ACM_PRODUCT]
ALTER COLUMN DESCRIPTION VARCHAR(512) COLLATE Arabic_CI_AI_KS_WS;

