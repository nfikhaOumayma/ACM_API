USE [ACM_DEV]
GO

INSERT INTO [dbo].[ACM_ENVIRONNEMENT]
           ([ACM_ENVIRONNEMENT_KEY]
           ,[ACM_ENVIRONNEMENT_VALUE]
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
           ('GUARANTOR_BRANCH_FILTER'
           ,'BRANCH_ID'
           ,1
           ,'2023-08-25'
           ,NULL
           ,NULL
           ,NULL
           ,NULL
           ,'Filter the list of guarantors by branch identifier'
           ,'FUNCTIONAL'
           ,NULL)
GO


