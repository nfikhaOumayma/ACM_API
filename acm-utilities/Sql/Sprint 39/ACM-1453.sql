-- add ihm habilitation actions --
  INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_ACTIONS','' ,'Actions button' ,1,GETDATE(),'ADMIN',0);

-- add button habilitation for actions
 
  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 select 'ACM','ACTIVITY_STREAM','IHM_ACTIONS','ACTIVITY STREAM',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_ACTIONS';

  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','ASSIGN_APPLICATION','IHM_ACTIONS','ASSIGN APPLICATION',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_ACTIONS';

  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','CONTACT_CUSTOMER','IHM_ACTIONS','CONTACT CUSTOMER',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_ACTIONS';

  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','ASSIGN_TO_CUSTOMER','IHM_ACTIONS','ASSIGN TO CUSTOMER',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_ACTIONS';

  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select'ACM','CANCEL_LOAN','IHM_ACTIONS','CANCEL LOAN',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_ACTIONS';



  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select'ACM','CANCEL_ASSIGN_TO_CUSTOMER','IHM_ACTIONS','CANCEL ASSIGN TO CUSTOMER ',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_ACTIONS';

--- add habilitaion for action ihm and buttons for all groups

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select '','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'IHM'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE


 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'ACTIVITY_STREAM'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE


 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'ASSIGN_APPLICATION'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE


 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'CONTACT_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'ASSIGN_TO_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'CANCEL_LOAN'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_ACTIONS' ,'' ,'CANCEL_ASSIGN_TO_CUSTOMER'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE