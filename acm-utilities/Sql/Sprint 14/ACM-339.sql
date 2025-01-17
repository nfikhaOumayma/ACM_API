-------------
----- TABLE : ACM_SETTING_REQUIRED_STEP
CREATE TABLE ACM_SETTING_REQUIRED_STEP(
  ID_ACM_SETTING_REQUIRED_STEP BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,  
  ID_PRODUCT BIGINT NOT NULL,
  CODE varchar(256) NOT NULL,
  DESCRIPTION varchar(512),
  MANDATORY BIT NOT NULL,
	
  ACM_ENABLED BIT NOT NULL,
  DATE_INSERTION DATETIME NULL,
  INSERT_BY  VARCHAR (256) NULL,
  DATE_LAST_UPDATE DATETIME NULL,
  UPDATED_BY  VARCHAR (256) NULL,
  ACM_VERSION INT NULL
  
 FOREIGN KEY (ID_PRODUCT) REFERENCES ACM_PRODUCT(ID_ACM_PRODUCT)

);