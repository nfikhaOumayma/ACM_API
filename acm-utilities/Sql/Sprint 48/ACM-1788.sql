--- ACM_HABILITATION_IHM_BUTTON

 INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','LOAN_STATUT','IHM_REPPORTS_LIST','LOAN_STATUT',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_REPPORTS_LIST';

  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','COLLECTION_FOLLOW_UP','IHM_REPPORTS_LIST','COLLECTION_FOLLOW_UP',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_REPPORTS_LIST';

 INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','MEZA_CARD','IHM_REPPORTS_LIST','MEZA_CARD',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_REPPORTS_LIST';

  INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','I_SCORE','IHM_REPPORTS_LIST','I_SCORE',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_REPPORTS_LIST';



--- ACM_HABILITATION
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_REPPORTS_LIST' ,'reports-list' ,'LOAN_STATUT'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE


--- ACM_HABILITATION
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_REPPORTS_LIST' ,'reports-list' ,'COLLECTION_FOLLOW_UP'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

--- ACM_HABILITATION
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_REPPORTS_LIST' ,'reports-list' ,'MEZA_CARD'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE

--- ACM_HABILITATION
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_REPPORTS_LIST' ,'reports-list' ,'I_SCORE'  ,0,GETDATE(),'ADMIN',0 from ACM_GROUPE