-- ACM : DATA BASE

--- RESET DB : 

IF OBJECT_ID('dbo.ACM_DOCUMENTS', 'U') IS NOT NULL
DROP TABLE dbo.ACM_DOCUMENTS;

IF OBJECT_ID('dbo.ACM_CUSTOMER_DESICION', 'U') IS NOT NULL
DROP TABLE dbo.ACM_CUSTOMER_DESICION;

IF OBJECT_ID('dbo.ACM_REPORT_VISIT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_REPORT_VISIT;

IF OBJECT_ID('DBO.ACM_USERS_GROUPE', 'U') IS NOT NULL
DROP TABLE DBO.ACM_USERS_GROUPE;

IF OBJECT_ID('DBO.ACM_GROUPE', 'U') IS NOT NULL
DROP TABLE DBO.ACM_GROUPE;

IF OBJECT_ID('dbo.ACM_USERS', 'U') IS NOT NULL
DROP TABLE dbo.ACM_USERS;

IF OBJECT_ID('dbo.ACM_ENVIRONNEMENT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_ENVIRONNEMENT;

IF OBJECT_ID('DBO.ACM_SETTING_MOTIFS_REJET', 'U') IS NOT NULL
DROP TABLE DBO.ACM_SETTING_MOTIFS_REJET;

IF OBJECT_ID('dbo.ACM_SETTING_LEVEL_PROCESS', 'U') IS NOT NULL
DROP TABLE dbo.ACM_SETTING_LEVEL_PROCESS;

IF OBJECT_ID('dbo.ACM_SETTING_LEVEL', 'U') IS NOT NULL
DROP TABLE dbo.ACM_SETTING_LEVEL;

IF OBJECT_ID('DBO.ACM_HABILITATION', 'U') IS NOT NULL
DROP TABLE DBO.ACM_HABILITATION;

IF OBJECT_ID('DBO.OAUTH_CLIENT_DETAILS', 'U') IS NOT NULL
DROP TABLE DBO.OAUTH_CLIENT_DETAILS;

IF OBJECT_ID('DBO.OAUTH_ACCESS_TOKEN', 'U') IS NOT NULL
DROP TABLE DBO.OAUTH_ACCESS_TOKEN;

IF OBJECT_ID('DBO.OAUTH_REFRESH_TOKEN', 'U') IS NOT NULL
DROP TABLE DBO.OAUTH_REFRESH_TOKEN;

IF OBJECT_ID('DBO.ACM_LOAN_HISTORIQUE', 'U') IS NOT NULL
DROP TABLE DBO.ACM_LOAN_HISTORIQUE;

IF OBJECT_ID('dbo.ACM_LOAN_APPROVAL_HISTORIQUE', 'U') IS NOT NULL
DROP TABLE dbo.ACM_LOAN_APPROVAL_HISTORIQUE;

IF OBJECT_ID('DBO.ACM_SETTING_STATUT_WORKFLOW', 'U') IS NOT NULL
DROP TABLE DBO.ACM_SETTING_STATUT_WORKFLOW;

IF OBJECT_ID('dbo.ACM_SETTING_DOC_PRODUCT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_SETTING_DOC_PRODUCT;

IF OBJECT_ID('dbo.ACM_SETTING_DOC_TYPE', 'U') IS NOT NULL
DROP TABLE dbo.ACM_SETTING_DOC_TYPE;

IF OBJECT_ID('dbo.ACM_PRODUCT', 'U') IS NOT NULL
DROP TABLE dbo.ACM_PRODUCT;

IF OBJECT_ID('dbo.ACM_LOAN', 'U') IS NOT NULL 
DROP TABLE dbo.ACM_LOAN

-------------

----- TABLE : LOAN 
CREATE TABLE ACM_LOAN(

	ID_ACM_LOAN BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,   
	ID_LOAN_EXTERN BIGINT NOT NULL,
	ID_ACCOUNT_EXTERN BIGINT,
	ACCOUNT_NUMBER_EXTERN VARCHAR (256),
	PRODUCT_ID INT NULL,
	STATUT INT NOT NULL,
	PROCESS_INSTANCE_ID VARCHAR (256) NULL,
	PORTFOLIO_ID BIGINT NOT NULL, --- 0 : PAR DEFAULT PAS DE PROFIL COTÉ ABACUS
	LOANPRODUCT_DESCRIPTION VARCHAR (512) NULL ,
	CUSTOMER_NAME VARCHAR (256) NULL ,
	AMOUNT DECIMAL NULL,
    CURRENCY_SYMBOL VARCHAR (10) NULL,
	CURRENCY_DECIMALPLACES INT NULL,
	APPLYDATE DATE NULL,
    ACCOUNTPORTFOLIO_CODE VARCHAR (256) NULL,
	ACCOUNTPORTFOLIO_DESCRIPTION VARCHAR (256) NULL,
	LOAN_OWNER VARCHAR (512) NULL,
	STATUT_WORKFLOW INT NULL,
	CUSTOMER_ID BIGINT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL);

-------------------
----- TABLE : ACM_ENVIRONNEMENT 
CREATE TABLE ACM_ENVIRONNEMENT(   

	ID BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,   
	ACM_ENVIRONNEMENT_KEY VARCHAR (256) NOT NULL,
	ACM_ENVIRONNEMENT_VALUE VARCHAR (256) NOT NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL)
-------------------
----- TABLE : ACM_REPORT_VISIT 
CREATE TABLE ACM_REPORT_VISIT(
	
	ID_ACM_REPORT_VISIT BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,  
	ID_ACM_LOAN BIGINT NOT NULL,
	DESCRIPTION VARCHAR(5000) NOT NULL,
	VISIT_BY VARCHAR (256) NULL,
	PLANNED_VISIT DATE,
	COMMENT VARCHAR (256) NOT NULL,
	
	ACM_ENABLED BIT NOT NULL DEFAULT 1,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,

	FOREIGN KEY (ID_ACM_LOAN) REFERENCES ACM_LOAN (ID_ACM_LOAN)
);
-------------------
----- TABLE : ACM_SETTING_MOTIFS_REJET
CREATE TABLE ACM_SETTING_MOTIFS_REJET(

    ID_ACM_SETTING_MOTIFS_REJET BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CATEGORIE VARCHAR (100) NOT NULL, -- REJET / CORRECTIFS
	CODE  VARCHAR (256) NOT NULL,
	LIBELLE  VARCHAR (512) NULL,
    DESCRIPTION  VARCHAR (512) NULL,
	ID_PRODUIT INT NULL,
	 
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);
-------------------
----- TABLE : ACM_SETTING_LEVEL
CREATE TABLE ACM_SETTING_LEVEL(

    ID_ACM_SETTING_LEVEL BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CODE VARCHAR (256) NOT NULL,
	TITLE VARCHAR (256) NOT NULL,
	DESCRIPTION VARCHAR (256) NULL,
		
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
);
-------------------
----- TABLE : ACM_SETTING_LEVEL
CREATE TABLE ACM_SETTING_LEVEL_PROCESS(

    ID_ACM_SETTING_LEVEL_PROCESS BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_PRODUCT BIGINT NOT NULL,
	AMOUNT BIGINT NOT NULL,
	DESCRIPTION VARCHAR (256) NULL,
	ID_ACM_SETTING_LEVEL BIGINT NOT NULL,
		
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (ID_ACM_SETTING_LEVEL) REFERENCES ACM_SETTING_LEVEL (ID_ACM_SETTING_LEVEL)
);
-------------------
----- TABLE : ACM_USERS 
CREATE TABLE ACM_USERS(   
	USERNAME VARCHAR (256) NOT NULL PRIMARY KEY,   
	PASSWORD VARCHAR (256) NOT NULL,
	ACCOUNT_PORTFOLIO_ID BIGINT NOT NULL, --- 0 : PAR DEFAULT PAS DE PORTFOLIO COTÉ ABACUS
	USER_EXTERN_ID BIGINT NOT NULL, --- 0 : PAR DEFAULT PAS DE USER COTÉ ABACUS
	USER_PROFIL_ID BIGINT NOT NULL, --- 0 : PAR DEFAULT PAS DE PROFIL COTÉ ABACUS
	RESPONSABLE_ID VARCHAR (256) NULL , --- NULL : PAS DE RESPONSABLE 
	NAME VARCHAR (256) NULL,
	SUR_NAME VARCHAR (256) NULL,
	EMAIL VARCHAR (512) NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
	);
-------------------
----- TABLE : OAUTH_CLIENT_DETAILS 
CREATE TABLE OAUTH_CLIENT_DETAILS (
    CLIENT_ID VARCHAR(256) PRIMARY KEY,
    RESOURCE_IDS VARCHAR(256),
    CLIENT_SECRET VARCHAR(256),
    SCOPE VARCHAR(256),
    AUTHORIZED_GRANT_TYPES VARCHAR(256),
    WEB_SERVER_REDIRECT_URI VARCHAR(256),
    AUTHORITIES VARCHAR(256),
    ACCESS_TOKEN_VALIDITY INTEGER,
    REFRESH_TOKEN_VALIDITY INTEGER,
    ADDITIONAL_INFORMATION VARCHAR(4096),
    AUTOAPPROVE VARCHAR(256)
);
-------------------
----- TABLE : OAUTH_ACCESS_TOKEN  
CREATE TABLE OAUTH_ACCESS_TOKEN (
  TOKEN_ID VARCHAR(256),
  TOKEN VARBINARY(5000),
  AUTHENTICATION_ID VARCHAR(256) PRIMARY KEY,
  USER_NAME VARCHAR(256),
  CLIENT_ID VARCHAR(256),
  AUTHENTICATION VARBINARY(5000),
  REFRESH_TOKEN VARCHAR(256)
);
-------------------
----- TABLE : OAUTH_REFRESH_TOKEN  
CREATE TABLE OAUTH_REFRESH_TOKEN (
  TOKEN_ID VARCHAR(256) PRIMARY KEY,
  TOKEN VARBINARY(5000),
  AUTHENTICATION VARBINARY(5000)
);
-------------------
----- TABLE : ACM_GROUPE 
CREATE TABLE ACM_GROUPE(   
	ID_ACM_GROUPE BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CODE VARCHAR (256) NOT NULL,
	LIBELLE VARCHAR (100) NOT NULL,
	DESCRIPTION  VARCHAR (512) NULL,
			
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);
-------------------
----- TABLE : ACM_USERS_GROUPE 
CREATE TABLE ACM_USERS_GROUPE(   
	ID_ACM_GROUPE BIGINT NOT NULL,
	USERNAME VARCHAR (256) NOT NULL,
    PRIMARY KEY (ID_ACM_GROUPE, USERNAME),
    FOREIGN KEY (ID_ACM_GROUPE) REFERENCES ACM_GROUPE (ID_ACM_GROUPE),
    FOREIGN KEY (USERNAME) REFERENCES ACM_USERS (USERNAME)	
);
-------------------
----- TABLE : ACM_HABILITATION 
CREATE TABLE ACM_HABILITATION(   
	ID_ACM_HABILITATION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY, 
	ACTIONS  VARCHAR (512) NOT NULL,
	CLIENT VARCHAR (256) NOT NULL,
	GROUPE_ID BIGINT NOT NULL,
	ACM_HABILITATION VARCHAR (512) NOT NULL,
	ACM_WEB_ROUTE VARCHAR (512) NOT NULL,
	VALUE BIT NOT NULL,
			
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);
-------------
----- TABLE : ACM_LOAN_HISTORIQUE
CREATE TABLE ACM_LOAN_HISTORIQUE(
    ID_ACM_LOAN_HISTORIQUE BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ACM_LOAN BIGINT NOT NULL,
	ACTION VARCHAR (100) NOT NULL,
	DESCRIPTION  VARCHAR (512) NULL,
	DATE_UPDATE DATETIME NOT NULL,
	UPDATED_BY  VARCHAR (256) NOT NULL
	
	FOREIGN KEY (ID_ACM_LOAN) REFERENCES ACM_LOAN (ID_ACM_LOAN)
);
-------------
----- TABLE : ACM_SETTING_STATUT_WORKFLOW
CREATE TABLE ACM_SETTING_STATUT_WORKFLOW(
    ID_ACM_SETTING_STATUT_WORKFLOW BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CODE  INT NOT NULL,
	LIBELLE  VARCHAR (512) NULL,
    DESCRIPTION  VARCHAR (512) NULL,
	CODE_STATUT_LOAN BIGINT NOT NULL,
    STATUT_LOAN	VARCHAR (512) NULL,
    IHM_WEB_ROOT VARCHAR (512) NULL,
	CLIENT VARCHAR (256) NULL ;
    BPMN_PROCESS_NAME VARCHAR (512) NULL ;
    ORDER_ETAPE_PROCESS INT NULL ;

	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);

-------------
----- TABLE : ACM_CUSTOMER_DESICION
-------------
CREATE TABLE ACM_CUSTOMER_DESICION(

    ID_ACM_CUSTOMER_DESICION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ACM_LOAN BIGINT NOT NULL,
	CONTACT_DATE DATETIME NOT NULL,
	COMMENTS  NVARCHAR (MAX) NULL,
    STATUS_ID INT NULL,
	STATUS_LIBELLE VARCHAR (256) NULL,
	AMOUNT DECIMAL NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL

	FOREIGN KEY (ID_ACM_LOAN) REFERENCES ACM_LOAN (ID_ACM_LOAN)
);

----- TABLE : ACM_LOAN_APPROVAL_HISTORIQUE
CREATE TABLE ACM_LOAN_APPROVAL_HISTORIQUE(
    ID_ACM_LOAN_APPROVAL_HISTORIQUE BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ACM_LOAN BIGINT NOT NULL,
	
	APPROVAL_DATE DATETIME NOT NULL,
	APPROVAL_AMOUNT BIGINT NOT NULL,
	APPROVAL_DESICION INT NOT NULL,
	APPROVAL_DESICION_LABEL VARCHAR (256) NOT NULL,
	APPROVAL_NOTE  VARCHAR (512) NOT NULL,
    APPROVAL_LEVEL INT NOT NULL,
	APPROVAL_LEVEL_LABEL VARCHAR (256) NOT NULL,
	APPROVED_BY  VARCHAR (256) NULL,

	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL

	FOREIGN KEY (ID_ACM_LOAN) REFERENCES ACM_LOAN (ID_ACM_LOAN)
);

-------------
----- TABLE : ACM_SETTING_DOC_TYPE
CREATE TABLE [dbo].[ACM_SETTING_DOC_TYPE](
	[ID_ACM_SETTING_DOC_TYPE] [bigint] IDENTITY(1,1) NOT NULL PRIMARY KEY,
	[CODE] [varchar](256) NOT NULL,
	[LIBELLE] [varchar](512) NULL,
	[DESCRIPTION] [varchar](512) NULL,
	[CATEGORIE] [smallint] NOT NULL,
	[UNIQUENESS] [bit] NULL,
	[DATE_DEBUT] [date] NULL,
	[DATE_FIN] [date] NULL,
	
	[ACM_ENABLED] [bit] NOT NULL,
	[DATE_INSERTION] [date] NULL,
	[INSERT_BY] [varchar](256) NULL,
	[DATE_LAST_UPDATE] [date] NULL,
	[UPDATED_BY] [varchar](256) NULL,
	[ACM_VERSION] [int] NULL,
)	
-------------
----- TABLE : ACM_SETTING_DOC_PRODUCT
CREATE TABLE [dbo].[ACM_SETTING_DOC_PRODUCT](
	[ID_ACM_SETTING_DOC_PRODUCT] [bigint] IDENTITY(1,1) NOT NULL PRIMARY KEY,
	[PRODUCT_ID] [int] NOT NULL,
	[ID_ACM_SETTING_DOC_TYPE] [bigint] NOT NULL,
	[MANDATORY] [bit] NOT NULL,
	[DATE_DEBUT] [date] NULL,
	[DATE_FIN] [date] NULL,
	
	[ACM_ENABLED] [bit] NOT NULL,
	[DATE_INSERTION] [date] NULL,
	[INSERT_BY] [varchar](256) NULL,
	[DATE_LAST_UPDATE] [date] NULL,
	[UPDATED_BY] [varchar](256) NULL,
	[ACM_VERSION] [int] NULL,
	
	FOREIGN KEY (ID_ACM_SETTING_DOC_TYPE) REFERENCES ACM_SETTING_DOC_TYPE (ID_ACM_SETTING_DOC_TYPE)
)	
-------------
----- TABLE : ACM_PRODUCT
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

CREATE TABLE ACM_DOCUMENTS(
    ID_ACM_DOCUMENTS BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	ID_ACM_LOAN BIGINT,
	ID_CUSTOMER BIGINT,
	ID_ACM_SETTING_DOC_TYPE BIGINT,
	ID_DOCUMENT_GED VARCHAR (256) NULL,
    TITRE  VARCHAR (512) NOT NULL,
    DESCRIPTION  VARCHAR (1000) NULL,
    AUTEUR VARCHAR (256) NULL,
    DATE_CREATION DATETIME NULL,
		
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,
	
	FOREIGN KEY (ID_ACM_LOAN) REFERENCES ACM_LOAN (ID_ACM_LOAN),
	FOREIGN KEY (ID_ACM_SETTING_DOC_TYPE) REFERENCES ACM_SETTING_DOC_TYPE (ID_ACM_SETTING_DOC_TYPE) 
);

ALTER TABLE [dbo].[ACM_LOAN]
ALTER COLUMN [LOANPRODUCT_DESCRIPTION] VARCHAR (512) COLLATE Arabic_CI_AI_KS_WS;

ALTER TABLE [dbo].[ACM_LOAN]
ALTER COLUMN [CUSTOMER_NAME] VARCHAR (256) COLLATE Arabic_CI_AI_KS_WS;

ALTER TABLE [dbo].[ACM_LOAN]
ALTER COLUMN [ACCOUNTPORTFOLIO_DESCRIPTION] VARCHAR (256) COLLATE Arabic_CI_AI_KS_WS;

ALTER TABLE [dbo].[ACM_PRODUCT]
ALTER COLUMN DESCRIPTION VARCHAR (512) COLLATE Arabic_CI_AI_KS_WS;

