DROP TABLE [ACM_CUSTOMER_MEMBER]

	
ALTER TABLE dbo.ACM_CUSTOMER ADD 
	GENDER varchar(255),
	DATE_OF_BIRTH_HIJRI Date,
	ORGANIZATION_NAME varchar(255),
	ACCOUNT_YEAR_END Date,
	SOLIDARITY_NAME varchar(255);
	
-------------
----- TABLE : ACM_LINKS_RELATIONSHIPS
CREATE TABLE dbo.ACM_LINKS_RELATIONSHIPS (
    ID_ACM_LINKS_RELATIONSHIPS BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ACM_CUSTOMER BIGINT,
	ID_ACM_CUSTOMER_MEMBER BIGINT,
	LINK_RELATIONSHIPS_TYPE VARCHAR(255),
	CATEGORY VARCHAR(100),
	DATE_DEBUT DATETIME NULL,
	DATE_FIN DATETIME NULL,

	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	FOREIGN KEY (ID_ACM_CUSTOMER_MEMBER) REFERENCES ACM_CUSTOMER (ID_ACM_CUSTOMER)
);

