CREATE TABLE ACM_COLLECTION_NOTE(
    ID BIGINT NOT NULL PRIMARY KEY,
	ACTION VARCHAR(150) NOT NULL,
	COMMENT VARCHAR(512) NOT NULL ,
    COLLECTION_ID BIGINT ,


	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL
);