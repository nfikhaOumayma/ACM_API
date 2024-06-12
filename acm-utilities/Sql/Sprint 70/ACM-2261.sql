-----Create table Workflow step

CREATE TABLE [dbo].[ACM_WORKFLOW_STEP](
	[ID_ACM_WORKFLOW_STEP] [bigint] IDENTITY(1,1) NOT NULL,
	[STEP_NAME] [varchar](256) NULL,
	[STEP_ORDER] [bigint] NULL,
	[STEP_TYPE] [varchar](256) NULL,
	[PRODUCT_ID] [bigint] NULL,
	[USER_GROUP] [varchar](256) NULL,
	[PREVIOUS_STEP] [bigint] NULL,
	[PROCESS] [varchar](256) NULL,
	[SCREEN] [varchar](256) NULL,
	[CODE_STATUT_LOAN] [bigint] NULL,
	[PROCESS_VERSION] [bigint] NULL,
	[ACM_ENABLED] [bit] NOT NULL,
	[DATE_INSERTION] [datetime] NULL,
	[INSERT_BY] [varchar](256) NULL,
	[DATE_LAST_UPDATE] [datetime] NULL,
	[UPDATED_BY] [varchar](256) NULL,
	[ACM_VERSION] [int] NULL,
	[GROUP_CODE] [varchar](100) NULL,
	[READY_FOR_DISB] [bit] NULL,
	[APPROVAL_CONDITIONS] [bit] NULL,
	[SCREENING_COMPONENT] [varchar](256) NULL,
	[GENERATION_TASK] [bit] NULL,
	[MIN_AMOUNT] [bigint] NULL,
	[MAX_AMOUNT] [bigint] NULL,
	[CHECK_MEZA_CARD] [bit] NULL,
	[IB_SCREEN] [varchar](100) NULL,
	[CHECK_FEES] [bit] NULL,
	[acceptation_condition] [nvarchar](100) NULL,
	[AUTOMATIC_STEP] [bit] NULL,
	[MIN_SCORE_REJECTED] [bigint] NULL,
	[MAX_SCORE_REJECTED] [bigint] NULL,
	[MIN_SCORE_ACCEPTED] [bigint] NULL,
	[MAX_SCORE_ACCEPTED] [bigint] NULL,
	[REJECTION_CONDITION] [varchar](256) NULL,
PRIMARY KEY CLUSTERED 
(
	[ID_ACM_WORKFLOW_STEP] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[ACM_WORKFLOW_STEP] ADD  DEFAULT ((0)) FOR [MIN_AMOUNT]
GO
----- TABLE ASSOCIATION : ACM_WORKFLOW_STEP_ACM_DOCUMENT_TYPE_PRODUCT 

 CREATE TABLE ACM_WORKFLOW_STEP_ACM_DOCUMENT_TYPE_PRODUCT  (
 ACM_WORKFLOW_STEP_ID BIGINT NOT NULL,
 ACM_DOCU_TYPE_PROD_ID BIGINT NOT NULL

 PRIMARY KEY (ACM_WORKFLOW_STEP_ID,ACM_DOCU_TYPE_PROD_ID)

 FOREIGN KEY (ACM_WORKFLOW_STEP_ID) REFERENCES ACM_WORKFLOW_STEP(ID_ACM_WORKFLOW_STEP),
 FOREIGN KEY (ACM_DOCU_TYPE_PROD_ID) REFERENCES ACM_SETTING_DOC_PRODUCT(ID_ACM_SETTING_DOC_PRODUCT),
);