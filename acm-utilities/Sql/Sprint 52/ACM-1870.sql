INSERT INTO [dbo].[ACM_IHM_VALIDATOR] VALUES ('MezaCard Internal' ,'^([0-9]{19})$' ,1 ,GETDATE() ,'super.admin' ,null ,null ,0 ,'MEZA CARD MASK');
INSERT INTO [dbo].[ACM_IHM_VALIDATOR] VALUES ('Bank Account' ,'' ,1 ,GETDATE() ,'super.admin' ,null ,null ,0 ,'MEZA CARD MASK');
INSERT INTO [dbo].[ACM_IHM_VALIDATOR] VALUES ('Wallet' ,'^([0-9]{11})$' ,1 ,GETDATE() ,'super.admin' ,null ,null ,0 ,'MEZA CARD MASK');
INSERT INTO [dbo].[ACM_IHM_VALIDATOR] VALUES ('CardMeza External' ,'^([0-9]{16})$' ,1 ,GETDATE() ,'super.admin' ,null ,null ,0 ,'MEZA CARD MASK');
INSERT INTO [dbo].[ACM_IHM_VALIDATOR] VALUES ('No Card' ,'' ,1 ,GETDATE() ,'super.admin' ,null ,null ,0 ,'MEZA CARD MASK');