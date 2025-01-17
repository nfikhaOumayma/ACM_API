  ----- TABLE : ACM_COLLECTION_THIRD_PARTY
CREATE TABLE ACM_COLLECTION_THIRD_PARTY(   
    ID_ACM_THIRD_PARTY_LEGAL_COLLECTION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
    FIRST_NAME VARCHAR (256) NULL,
    LAST_NAME VARCHAR (256) NULL,
    ADDRESS_PARTY VARCHAR (512) NULL,
    EMAIL VARCHAR (512) NULL,
    PHONE_NUMBER VARCHAR (512) NULL,
    ACCESS_BRANCHES VARCHAR (512) NULL,
    TYPE_PARTY VARCHAR (512) NULL,
    BRANCHID int,
    BRANCHE_NAME varchar(512),
    BRANCHE_DESCRIPTION varchar(512),
    ACM_ENABLED BIT NOT NULL,
    DATE_INSERTION DATETIME NULL,
    INSERT_BY  VARCHAR (256) NULL,
    DATE_LAST_UPDATE DATETIME NULL,
    UPDATED_BY  VARCHAR (256) NULL,
    ACM_VERSION INT NULL
    );


----- TABLE ASSOCIATION : ACM_COLL_THIRD_PARTY_ASSOC
IF OBJECT_ID('dbo.ACM_COLL_THIRD_PARTY_ASSOC', 'U') IS NOT NULL
DROP TABLE dbo.ACM_COLL_THIRD_PARTY_ASSOC;

	CREATE TABLE ACM_COLL_THIRD_PARTY_ASSOC (
	ACM_COLLECTION_ID BIGINT NOT NULL,
	ACM_THIRD_PARTY_ID BIGINT NOT NULL
	PRIMARY KEY (ACM_COLLECTION_ID,ACM_THIRD_PARTY_ID)
	FOREIGN KEY (ACM_COLLECTION_ID) REFERENCES ACM_COLLECTION(ID_ACM_COLLECTION),
	FOREIGN KEY (ACM_THIRD_PARTY_ID) REFERENCES ACM_COLLECTION_THIRD_PARTY(ID_ACM_THIRD_PARTY_LEGAL_COLLECTION),
); 



-- ADD HABILITATION SETTING_THIRD_PARTY
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,ID_ACM_GROUPE ,'IHM_SETTING_THIRD_PARTY' ,'setting-collection-third-party' ,'IHM'  ,1,GETDATE(),'ADMIN',0 from ACM_GROUPE


--ADD ROUTE HABILITATION SETTING_THIRD_PARTY
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])          
VALUES('ACM','IHM_SETTING_THIRD_PARTY','setting-collection-third-party' ,'page setting legal collection third party' ,1,GETDATE(),'ADMIN',0);


--Rename in table ACM_COLLECTION_STEP Column TYPE_PARTICIPANTS TO TYPE_THIRD_PARTY 
sp_rename 'ACM_COLLECTION_STEP.TYPE_PARTICIPANTS', 'TYPE_THIRD_PARTY', 'COLUMN';



----DROP THE OLD TABLE ASSOCIATION :  ACM_COLL_THIRD_PARTY_ASSOC

drop table ACM_COLL_THIRD_PARTY_ASSOC;


----- TABLE ASSOCIATION : ACM_COLLECTION_INSTANCE_ACM_THIRD_PARTY

   CREATE TABLE ACM_COLLECTION_INSTANCE_ACM_THIRD_PARTY (
    ACM_COLLECTION_INSTANCE_ID BIGINT NOT NULL,
    ACM_THIRD_PARTY_ID BIGINT NOT NULL
    PRIMARY KEY (ACM_COLLECTION_INSTANCE_ID,ACM_THIRD_PARTY_ID)
    FOREIGN KEY (ACM_COLLECTION_INSTANCE_ID) REFERENCES ACM_COLLECTION_INSTANCE(ID_ACM_COLLECTION_INSTANCE),
    FOREIGN KEY (ACM_THIRD_PARTY_ID) REFERENCES ACM_COLLECTION_THIRD_PARTY(ID_ACM_THIRD_PARTY_LEGAL_COLLECTION),
); 