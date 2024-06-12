INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'RUN_AML_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE
 
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'ACCEPT_AML_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'REJECT_AML_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'RUN_ISCORE_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'ACCPET_ISCORE_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'REJECT_ISCORE_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'RUN_AML_GUARANTOR'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'ACCEPT_AML_GUARANTOR'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'REJECT_AML_GUARANTOR'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'RUN_ISCORE_GUARANTOR'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'screening' ,'ACCEPT_ISCORE_GUARANTOR'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SCREENING' ,'' ,'REJECT_ISCORE_GUARANTOR'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE