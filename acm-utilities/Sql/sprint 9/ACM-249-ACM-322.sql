IF OBJECT_ID('DBO.ACM_HABILITATION_IHM_ROUTE', 'U') IS NOT NULL
	DROP TABLE DBO.ACM_HABILITATION_IHM_ROUTE;

-------------------
----- TABLE : ACM_HABILITATION_IHM_ROUTE 
CREATE TABLE ACM_HABILITATION_IHM_ROUTE(   
	ID_ACM_HABILITATION_IHM_ROUTE BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CLIENT VARCHAR (256) NOT NULL,
	CODE_IHM_ROUTE  VARCHAR (512) NOT NULL,
	IHM_ROUTE VARCHAR (512) NOT NULL,
	DESCRIPTION VARCHAR (512) NULL,
	RACINE_ID BIGINT NULL,
			
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATE NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATE NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);