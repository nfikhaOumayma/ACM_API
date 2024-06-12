 --create claim note table--
CREATE TABLE [dbo].[ACM_CLAIM_NOTE](
    [ID] [bigint] IDENTITY(1,1) NOT NULL,
    [COMMENT] [varchar](512) NULL,
    [ID_CLAIM_NOTE] [bigint] NULL,
    [VISIBILITY] [varchar](256) NULL, 
    [ACM_ENABLED] [bit] NOT NULL,
    [DATE_INSERTION] [datetime] NULL,
    [INSERT_BY] [varchar](256) NULL,
    [DATE_LAST_UPDATE] [datetime] NULL,
    [UPDATED_BY] [varchar](256) NULL,
    [ACM_VERSION] [int] NULL,
    PRIMARY KEY CLUSTERED 
    (
        [ID] ASC
    )
) ON [PRIMARY]

 --add claim id column to calendar event table--
ALTER TABLE [dbo].[ACM_CALENDAR_EVENT]
ADD [ACM_ID_CLAIM] [bigint] NULL;

 --add rabbit mq user to acm_users--
INSERT INTO [dbo].[ACM_USERS] (
    [USERNAME],
    [PASSWORD],
    [ACCOUNT_PORTFOLIO_ID],
    [USER_EXTERN_ID],
    [USER_PROFIL_ID],
    [ACM_ENABLED],
    [DATE_INSERTION],
    [INSERT_BY],
    [DATE_LAST_UPDATE],
    [UPDATED_BY],
    [ACM_VERSION],
    [RESPONSABLE_ID],
    [NAME],
    [SUR_NAME],
    [EMAIL],
    [BranchID],
    [BRANCHE_NAME],
    [BRANCHE_DESCRIPTION],
    [CUSTOMER_ID],
    [ACCESS_BRANCHES],
    [TEMPORARY_PWD],
    [CATEGORY],
    [DEFAULT_LANG],
    [PORTFOLIO_NAME],
    [HIRING_DATE],
    [RESIGNING_DATE],
    [TEMPORARY_RESPONSABLE],
    [OLD_RESPONSABLE_NAME],
    [OLD_RESPONSABLE_ID],
    [EMPLOYEE_ID],
    [FAILED_ATTEMPTS],
    [PWD_EXPIRY_DATE]
) VALUES (
    'rabbitMq.user',
    '$2a$10$qJtN4anLjQTt2zSW4kPlKe/OxwWKMPanXp8ZXm6DfGX8w9P9usH7a',
    0,
    0,
    0,
    1,
    '2023-08-30',
    'Super.Admin',
    NULL,
    NULL,
    0,
    0,
    'user',
    'rabbitMq',
    'lamloummanel8@gmail.com',
    0,
    NULL,
    NULL,
    0,
    NULL,
    0,
    'OPERATION',
    'AR',
    '',
    '2023-08-04',
    '2030-11-15',
    NULL,
    NULL,
    NULL,
    'rabbitMq.user',
    0,
    '2023-11-28'
);