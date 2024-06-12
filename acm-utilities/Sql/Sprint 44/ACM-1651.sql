 ---ACM HABILITATION : 'SUBMIT' button in screening step when there is no field-visit step-----
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'SUBMIT'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE