insert into ACM_MODULE(MODULE)  values('COMPLIANCE') ,('EXPENSES') , ('INCENTIVE')  , ('OFFLINE');


update ACM_HABILITATION_IHM_ROUTE set RACINE_ID  =(select ID_ACM_MODULE  from  ACM_MODULE where MODULE ='INCENTIVE') where CODE_IHM_ROUTE  like '%INCENTIVE%'
update ACM_HABILITATION_IHM_ROUTE set RACINE_ID  =(select ID_ACM_MODULE  from  ACM_MODULE where MODULE ='COMPLIANCE') where CODE_IHM_ROUTE  like '%aml%'
update ACM_HABILITATION_IHM_ROUTE set RACINE_ID  =(select ID_ACM_MODULE  from  ACM_MODULE where MODULE ='EXPENSES') where CODE_IHM_ROUTE  like '%EXPENSES%'


INSERT INTO [dbo].[ACM_ENVIRONNEMENT]
           ([ACM_ENVIRONNEMENT_KEY],[ACM_ENVIRONNEMENT_VALUE]
           ,[ACM_ENABLED]
           ,[DATE_INSERTION]
           ,[INSERT_BY]
           ,[DATE_LAST_UPDATE]
           ,[UPDATED_BY]
           ,[ACM_VERSION]
           ,[DESCRIPTION]
           ,[CATEGORY]
           ,[TYPE_VALUE])
     VALUES
           ('CRON_EXPRESSION_SENDING_MAIL_LICENCE'
           ,'* * * 9 * ?'
           ,1
           ,'2024-08-18'
           ,null
           ,'2024-08-18'
           ,null
           ,1
           ,'cron seding mail for licence'
           ,'TECHNICAL'
           ,null) ; 



INSERT INTO [dbo].[ACM_ENVIRONNEMENT]
           ([ACM_ENVIRONNEMENT_KEY],[ACM_ENVIRONNEMENT_VALUE]
           ,[ACM_ENABLED]
           ,[DATE_INSERTION]
           ,[INSERT_BY]
           ,[DATE_LAST_UPDATE]
           ,[UPDATED_BY]
           ,[ACM_VERSION]
           ,[DESCRIPTION]
           ,[CATEGORY]
           ,[TYPE_VALUE])
     VALUES
           ('MAIL_PERIODE_LICENCE'
           ,30
           ,1
           ,'2024-08-18'
           ,null
           ,'2024-08-18'
           ,null
           ,1
           ,'period before sending mail reminder'
           ,'TECHNICAL'
           ,null) ; 


update  ACM_ENVIRONNEMENT  set ACM_ENVIRONNEMENT_KEY  = 'KEY_LICENCE' where ACM_ENVIRONNEMENT_KEY  = 'KEY_LICENSE'
update  ACM_ENVIRONNEMENT  set ACM_ENVIRONNEMENT_KEY  = 'LICENCE_AUTOMATIC_STEP' where ACM_ENVIRONNEMENT_KEY  = 'LICENSE_AUTOMATIC_STEP'


		  


