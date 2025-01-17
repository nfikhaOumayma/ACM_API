-- ('1') New 
-- ('2') Drafts 
-- ('3') Awaiting Approval 
-- ('4') Approved 
-- ('5') Rejected 
-- ('6') Cancelled 
-- ('7') Correctifs 
 
-- ACM : DATA BASE

--- RESET DB : 
IF OBJECT_ID('DBO.ACM_SETTING_STATUT_WORKFLOW', 'U') IS NOT NULL
DROP TABLE DBO.ACM_SETTING_STATUT_WORKFLOW;
-------------

----- TABLE : ACM_SETTING_STATUT_WORKFLOW
CREATE TABLE ACM_SETTING_STATUT_WORKFLOW(
    ID_ACM_SETTING_STATUT_WORKFLOW BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CODE  INT NOT NULL,
	LIBELLE  VARCHAR (512) NULL,
    DESCRIPTION  VARCHAR (512) NULL,
	
	CODE_STATUT_LOAN BIGINT NOT NULL,
    STATUT_LOAN	VARCHAR (512) NULL,
	
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATE NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATE NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);

----- TABLE : ACM_LOAN
ALTER TABLE ACM_LOAN ADD STATUT_WORKFLOW INT NULL;

ALTER TABLE ACM_LOAN ADD ETAPE_WORKFLOW INT NULL;
