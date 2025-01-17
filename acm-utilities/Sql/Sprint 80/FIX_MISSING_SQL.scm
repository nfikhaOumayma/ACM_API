alter table acm_loan 
add ready_for_disb int 

alter table acm_loan_instance
add ACTION_USER varchar (50)

alter table acm_address
add ID_SUPPLIER bigint null


 

update ACM_HABILITATION_IHM_ROUTE set SETTINGS_WORKFLOW = 1 
where 
ID_ACM_HABILITATION_IHM_ROUTE in (
5,
9,
10,
14,
17,
18,
19,
20,
21,
24,
25,
27,
30,
36,
10046,
20060,
20062)

update ACM_HABILITATION_IHM_ROUTE set SETTINGS_WORKFLOW = 0 where SETTINGS_WORKFLOW is null

alter table ACM_HABILITATION
drop column id_acm_module

alter table acm_habilitation
drop FK_ACM_HABILITATION_MODULE

  update ACM_HABILITATION_IHM_ROUTE set IHM_ROUTE = '/'+IHM_ROUTE
  update ACM_HABILITATION_IHM_ROUTE set IHM_ROUTE = '' where IHM_ROUTE = '/'
  
  
  update ACM_HABILITATION set ID_ACM_HABILITATION_IHM_ROUTE = 17 where ID_ACM_HABILITATION_IHM_ROUTE is null

--- split repayment and interest frequency
update acm_loan set PERIOD_FREQ = 1 where STATUT not in (8,6,5)


update ACM_PRODUCT set SUPPLIER = 0


CREATE TABLE [dbo].[ACM_COLLECTION_STEP_ACM_GROUPE_PARTICIPANT](
	[ACM_COLLECTION_STEP_ID] [bigint] NOT NULL,
	[ACM_GROUPE_ID] [bigint] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[ACM_COLLECTION_STEP_ID] ASC,
	[ACM_GROUPE_ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[ACM_COLLECTION_STEP_ACM_GROUPE_PARTICIPANT]  WITH CHECK ADD FOREIGN KEY([ACM_COLLECTION_STEP_ID])
REFERENCES [dbo].[ACM_COLLECTION_STEP] ([ID_ACM_COLLECTION_STEP])
GO

ALTER TABLE [dbo].[ACM_COLLECTION_STEP_ACM_GROUPE_PARTICIPANT]  WITH CHECK ADD FOREIGN KEY([ACM_GROUPE_ID])
REFERENCES [dbo].[ACM_GROUPE] ([ID_ACM_GROUPE])
GO

INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[RACINE_ID],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], SETTINGS_WORKFLOW)            
VALUES('ACM','IHM_LOAN_LEGAL_DETAILS','/loan-legal-details' ,'PAGE LEGAL COLLECTION details' ,2,1,GETDATE(),'ADMIN',0,1);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[DESCRIPTION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_LOAN_LEGAL_DETAILS' ,'loan-legal-details' ,'IHM'  ,0,GETDATE(),'ADMIN',0, 'ADD SUPPLIER SCREEN' from ACM_GROUPE

ALTER DATABASE ABACUS_TAMKEEN  
SET COMPATIBILITY_LEVEL = 130;  
GO

