-- Update loans where statut is Issued
update ACM_LOAN set STATUT = 8 where STATUT = 4 and STATUT_WORKFLOW = 22
-- Update  ACM_SETTING_STATUT_WORKFLOW where libelle is Issued
update ACM_SETTING_STATUT_WORKFLOW set CODE_STATUT_LOAN = 8 where LIBELLE = 'Issued'

-- ADD HABILITATION ISSUED_LOAN 
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[DESCRIPTION])
select 'READ','ACM' ,ID_ACM_GROUPE ,'IHM_ISSUED_LOANS' ,'issued-loans' ,'IHM'  ,1,GETDATE(),'ADMIN',0,'ISSUED LOANS' from ACM_GROUPE
--ADD ROUTE HABILITATION ISSUED_LOAN
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
	VALUES('ACM','IHM_ISSUED_LOANS','issued-loans' ,'page issued loans' ,1,GETDATE(),'ADMIN',0);

