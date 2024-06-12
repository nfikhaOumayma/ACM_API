-----------------------------------
----- TABLE : ACM_UDF_GROUPE
-----------------------------------
CREATE TABLE ACM_UDF_GROUPE(
	ID_ACM_UDF_GROUPE BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ABACUS_UDF_GROUPE BIGINT NOT NULL, -- UDF Groupe ID d'Abacus
	CODE VARCHAR(512) NULL,
	DESCRIPTION VARCHAR(512) NULL,
	LOAN BIT NULL, -- - 1 : Loan  - 0 : Customer
	CUSTOMER BIT NULL, -- - 1 : Customer  - 0 : Loan
	CUSTOMER_TYPE INT NULL, -- - 1 : Individuel - 2 : Organisation - 4 : Groupe - 3 : Indivuel & organisation - 5 : Indiduel & groupe - 6 : organisation et groupe - 7 : Tous les types - 0 : groupe crédit
	PRODUCTID VARCHAR(MAX) NULL, -- Chaine contenant les id des produits séparé par une virgule ',' ou NULL en cas de groupe client
	
    ACM_ENABLED BIT NOT NULL,
    DATE_INSERTION DATETIME NULL,
    INSERT_BY  VARCHAR (256) NULL,
    DATE_LAST_UPDATE DATETIME NULL,
    UPDATED_BY  VARCHAR (256) NULL,
    ACM_VERSION INT NULL
);

-----------------------------------
----- TABLE : ACM_UDF_FIELD
-----------------------------------

 CREATE TABLE ACM_UDF_FIELD(
	ID_ACM_UDF_FIELD BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ABACUS_UDF_FIELD BIGINT NOT NULL, -- UDF Field ID d'Abacus
	ID_ACM_PARENT_UDF_FIELD BIGINT NULL, -- L'ID du champs parent en cas de liaison sinon NULL
	ACM_PARENT_UDF_FIELD_Value VARCHAR(MAX) NULL, -- La valeur de filtrage en cas d'existance de field parent sinon NULL
	ID_ACM_UDF_GROUPE BIGINT NULL, -- UDF Groupe ID d'ACM
	FIELD_MASC VARCHAR(MAX) NULL, -- Le masque du champs, chaine vide en cas d'inexistance de masque
	FIELD_TYPE INT NULL, -- - 1 : Texte - 2 : Numerique - 3 : Date - 4 : liste
	MANDATORY BIT NULL,
	FIELD_NAME VARCHAR(512) NULL,
	DESCRIPTION VARCHAR(512) NULL,
	ID_UDF_LIST_VALUE  BIGINT NULL,
	
    ACM_ENABLED BIT NOT NULL,
    DATE_INSERTION DATETIME NULL,
    INSERT_BY  VARCHAR (256) NULL,
    DATE_LAST_UPDATE DATETIME NULL,
    UPDATED_BY  VARCHAR (256) NULL,
    ACM_VERSION INT NULL
		
	FOREIGN KEY (ID_ACM_UDF_GROUPE) REFERENCES ACM_UDF_GROUPE(ID_ACM_UDF_GROUPE) 
 );

-----------------------------------
----- TABLE : ACM_UDF_LINK
-----------------------------------

CREATE TABLE ACM_UDF_LINK(
	ID_ACM_UDF_LINK BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ACM_UDF_FIELD BIGINT NOT NULL, -- UDF Field ID d'ACM
	ID_ABACUS_UDF_LINK BIGINT NULL, -- ID ABACUS UDF LINK
	ID_LOAN BIGINT NULL, -- Prend l'ID loan en cas de groupe crédit sinon NULL
	ID_CUSTOMER BIGINT NULL, -- Prend l'ID Client en cas de groupe client sinon NULL
	ID_ABACUS_FIELD_LIST_VALUE BIGINT NULL, -- UDF LIST Value ID d'Abacus en cas de liste sinon NULL
	FIELD_VALUE VARCHAR(MAX) NULL, -- Valeur de champs UDF
	
	ACM_ENABLED BIT NOT NULL,
    DATE_INSERTION DATETIME NULL,
    INSERT_BY  VARCHAR (256) NULL,
    DATE_LAST_UPDATE DATETIME NULL,
    UPDATED_BY  VARCHAR (256) NULL,
    ACM_VERSION INT NULL,
  
	FOREIGN KEY (ID_ACM_UDF_FIELD) REFERENCES ACM_UDF_FIELD(ID_ACM_UDF_FIELD)
 );


-----------------------------------
----- TABLE : ACM_UDF_LIST_VALUES
-----------------------------------

CREATE TABLE ACM_UDF_LIST_VALUES(
	ID_ACM_UDF_LIST_VALUES BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
    TABLE_ABACUS_NAME  VARCHAR (512) NULL,
    
    ID_UDF_LIST BIGINT NULL, -- table UserDefinedFieldLists
	
	ID_UDF_LIST_VALUE  BIGINT NULL, -- table UserDefinedFieldListValues
	ID_UDF_LIST_LINK  BIGINT NULL, -- table UserDefinedFieldListValues
    SCORE INT NULL, -- table UserDefinedFieldListValues
	
	NAME VARCHAR (256) NULL, -- table UserDefinedFieldLists / UserDefinedFieldListValues
    DESCRIPTION VARCHAR (512) NULL, -- table UserDefinedFieldLists / UserDefinedFieldListValues
    
	ACM_ENABLED BIT NOT NULL,
    DATE_INSERTION DATETIME NULL,
    INSERT_BY  VARCHAR (256) NULL,
    DATE_LAST_UPDATE DATETIME NULL,
    UPDATED_BY  VARCHAR (256) NULL,
    ACM_VERSION INT NULL
 );

alter table ACM_UDF_LIST_VALUES ALTER COLUMN NAME VARCHAR (256) COLLATE Arabic_CI_AI_KS_WS;
alter table ACM_UDF_LIST_VALUES ALTER COLUMN DESCRIPTION VARCHAR (512) COLLATE Arabic_CI_AI_KS_WS;
