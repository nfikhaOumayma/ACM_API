-----------------------------------
----- TABLE : ACM_ADDRESS_SETTING
-----------------------------------
CREATE TABLE ACM_ADDRESS_SETTING (
  ID_ACM_ADDRESS_SETTING BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
  TABLE_ABACUS_NAME  VARCHAR (512) NULL, 
  ID_EXTERN VARCHAR (256) NOT NULL, 
  PARENT_ID BIGINT NULL,
  ID_ADDRESS_LIST BIGINT NOT NULL,
  VALUE_JSON VARCHAR (1000) NULL, 
  
  ACM_ENABLED BIT NOT NULL,
  DATE_INSERTION DATETIME NULL,
  INSERT_BY  VARCHAR (256) NULL,
  DATE_LAST_UPDATE DATETIME NULL,
  UPDATED_BY  VARCHAR (256) NULL,
  ACM_VERSION INT NULL
);