-- add aml setting route in [ACM_HABILITATION_IHM_ROUTE]
 INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
	VALUES('ACM','IHM_SETTING_AML','setting-aml' ,'page setting aml' ,1,GETDATE(),'ADMIN',0);
-- add habilitaion for aml setting IHM
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,ID_ACM_GROUPE ,'IHM_SETTING_AML' ,'setting-aml' ,'IHM'  ,1,GETDATE(),'ADMIN',0 from ACM_GROUPE