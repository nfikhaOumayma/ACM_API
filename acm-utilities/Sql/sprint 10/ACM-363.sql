IF OBJECT_ID('dbo.ACM_NOTIFICATION', 'U') IS NOT NULL
DROP TABLE dbo.ACM_NOTIFICATION;

CREATE TABLE ACM_NOTIFICATION(
    ID_ACM_NOTIFICATION BIGINT NOT NULL IDENTITY(1,1) PRIMARY KEY,
	USERNAME  VARCHAR (256) NOT NULL,
	CREATION_DATE DATETIME NOT NULL,
	CATEGORY VARCHAR (256) NOT NULL, -- LOAN / TASK / OTHERS
	TYPE_NOTIF  VARCHAR (256) NOT NULL, -- ALERT / INFO / WARN
	REDIRECT BIT NOT NULL,
	STATUS_NOTIF VARCHAR (256) NOT NULL, -- NEW  / READ
	ACTION VARCHAR (256) NULL, -- ADD / UPDATE / APPROVE / REJECT / DECLINE / CANCEL / REVEIW 
	TITLE VARCHAR (256) NULL,
	DESCRIPTION VARCHAR (512) NOT NULL,
	ID_ACM_LOAN BIGINT NULL,
	ID_ACM_CALENDAR_EVENT BIGINT NULL,
    
	ACM_ENABLED BIT NOT NULL,
	DATE_INSERTION DATETIME NULL,
	INSERT_BY  VARCHAR (256) NULL,
	DATE_LAST_UPDATE DATETIME NULL,
	UPDATED_BY  VARCHAR (256) NULL,
	ACM_VERSION INT NULL,

	FOREIGN KEY (ID_ACM_LOAN) REFERENCES ACM_LOAN (ID_ACM_LOAN),
	FOREIGN KEY (ID_ACM_CALENDAR_EVENT) REFERENCES ACM_CALENDAR_EVENT (ID_ACM_CALENDAR_EVENT)
);