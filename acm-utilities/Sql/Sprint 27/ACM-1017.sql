-- update name from RECOMMEND to RECOMMEND_AUDIT where IHM_LOAN_REVIEW --
update ACM_HABILITATION_IHM_BUTTON set CODE_IHM_BUTTON = 'RECOMMEND_AUDIT', DESCRIPTION = 'RECOMMEND_AUDIT' where ID_ACM_HABILITATION_IHM_BUTTON = 47
-- update name from RECOMMEND to RECOMMEND_AUDIT where IHM_CUSTOMER_GROUPE --
update ACM_HABILITATION_IHM_BUTTON set CODE_IHM_BUTTON = 'RECOMMEND_AUDIT', DESCRIPTION = 'RECOMMEND_AUDIT' where ID_ACM_HABILITATION_IHM_BUTTON = 53

-- update name from REVIEW_RISK_AUDIT to REVIEW_AUDIT where IHM_CUSTOMER_GROUPE --
update ACM_HABILITATION_IHM_BUTTON set CODE_IHM_BUTTON = 'REVIEW_AUDIT', DESCRIPTION = 'REVIEW_AUDIT' where ID_ACM_HABILITATION_IHM_BUTTON = 66

 -- insert new habilitation button REVIEW_RISK for  IHM_CUSTOMER_GROUPE --
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM','REVIEW_RISK','IHM_CUSTOMER_GROUPE','REVIEW_RISK',34,1,GETDATE(),'ADMIN',0);


 -- insert new habilitation button RECOMMEND_RISK for  IHM_CUSTOMER_GROUPE
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM','RECOMMEND_RISK','IHM_CUSTOMER_GROUPE','RECOMMEND_RISK',2,1,GETDATE(),'ADMIN',0);

  
 -- insert new habilitation button REVIEW_AUDIT for  IHM_LOAN_REVIEW
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM','REVIEW_AUDIT','IHM_LOAN_REVIEW','REVIEW_AUDIT',34,1,GETDATE(),'ADMIN',0);


 -- insert new habilitation button REVIEW_RISK for  IHM_LOAN_REVIEW
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM','REVIEW_RISK','IHM_LOAN_REVIEW','REVIEW_RISK',2,1,GETDATE(),'ADMIN',0);
-- insert new habilitation button RECOMMEND_RISK for  IHM_LOAN_REVIEW
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM','RECOMMEND_RISK','IHM_LOAN_REVIEW','RECOMMEND_RISK',2,1,GETDATE(),'ADMIN',0);

-- insert habilitaions for RECOMMEND_RISK
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS]
      ,[CLIENT]
      ,[GROUPE_ID]
      ,[ACM_HABILITATION]
      ,[ACM_WEB_ROUTE]
      ,[ACM_ENABLED]
      ,[DATE_INSERTION]
      ,[INSERT_BY]
      ,[DATE_LAST_UPDATE]
      ,[UPDATED_BY]
      ,[ACM_VERSION],[VALUE])
select [ACTIONS]
      ,[CLIENT]
      ,[GROUPE_ID]
      ,[ACM_HABILITATION]
      ,[ACM_WEB_ROUTE]
      ,[ACM_ENABLED]
      ,[DATE_INSERTION]
      ,[INSERT_BY]
      ,[DATE_LAST_UPDATE]
      ,[UPDATED_BY]
      ,[ACM_VERSION]
      ,'RECOMMEND_RISK'from ACM_HABILITATION where VALUE ='RECOMMEND' 
-- update RECOMMEND to RECOMMEND_AUDIT --
update ACM_HABILITATION set value= 'RECOMMEND_AUDIT' where value = 'RECOMMEND' 


-- insert habilitaions for REVIEW_RISK --
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS]
      ,[CLIENT]
      ,[GROUPE_ID]
      ,[ACM_HABILITATION]
      ,[ACM_WEB_ROUTE]
      ,[ACM_ENABLED]
      ,[DATE_INSERTION]
      ,[INSERT_BY]
      ,[DATE_LAST_UPDATE]
      ,[UPDATED_BY]
      ,[ACM_VERSION],[VALUE])
select [ACTIONS]
      ,[CLIENT]
      ,[GROUPE_ID]
      ,[ACM_HABILITATION]
      ,[ACM_WEB_ROUTE]
      ,[ACM_ENABLED]
      ,[DATE_INSERTION]
      ,[INSERT_BY]
      ,[DATE_LAST_UPDATE]
      ,[UPDATED_BY]
      ,[ACM_VERSION]
      ,'REVIEW_RISK'from ACM_HABILITATION where VALUE ='REVIEW_RISK_AUDIT'

-- update REVIEW_RISK_AUDIT to REVIEW_AUDIT --
update ACM_HABILITATION set value= 'REVIEW_AUDIT' where value = 'REVIEW_RISK_AUDIT'

