-- ADD HABILITATION UNASSIGNED COLLECTIONS
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,ID_ACM_GROUPE ,'IHM_UNASSIGNED_COLLECTION' ,'unassigned-collections' ,'IHM'  ,1,GETDATE(),'ADMIN',0 from ACM_GROUPE
--ADD ROUTE HABILITATION UNASSIGNED_COLLECTIONS
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
	VALUES('ACM','IHM_UNASSIGNED_COLLECTION','unassigned-collections' ,'page unassigned collections' ,1,GETDATE(),'ADMIN',0);