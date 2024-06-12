-----------------------
-----------------------
-- DATA [ACM_USERS]
-----------------------
TRUNCATE TABLE ACM_USERS;

INSERT INTO ACM_USERS(username,password,ACM_ENABLED, account_portfolio_id, user_extern_id,user_profil_id,responsable_id,DATE_INSERTION,INSERT_BY) 
    VALUES ('john', '$2a$10$7iSnwpCdFHFJCqhdcV.cMOlMPkoZvjhu0VMgQUnfNn2nhMplGz8bq', 1,0,0,0,0,GETDATE(),'ADMIN'); --john@123
INSERT INTO ACM_USERS (username,password,ACM_ENABLED, account_portfolio_id, user_extern_id,user_profil_id,responsable_id,DATE_INSERTION,INSERT_BY)
    VALUES ('kelly','$2a$10$dzbLJaAWzZaCS7uGFL9DeO4vsS/0/XvlgfyGeCeP1hrOUoGMFMVdy', 1,0,0,0,'john',GETDATE(),'ADMIN'); --kelly@123	

INSERT INTO ACM_USERS(username,password,ACM_ENABLED, account_portfolio_id, user_extern_id,user_profil_id,responsable_id,DATE_INSERTION,INSERT_BY) 
    VALUES ('acmbatch', '$2a$10$msJbB.ncaJRmnLSeGNfVr.FU3kCHIFNf3LV3tmic9lFqh5k2Bieq2', 1,0,0,0,0,GETDATE(),'ADMIN'); --acmbatch@123
 

TRUNCATE TABLE oauth_access_token;

TRUNCATE TABLE oauth_refresh_token;

-----------------------
-----------------------
-- DATA [ACM_SETTING_STATUT_WORKFLOW]
-----------------------
 
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (1,'Initial Check','Initial Check => New'
           ,1,'New'
           ,1,'2020-03-30','ADMIN',0);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (2,'Field visit','Field visit => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (3,'Guarantor','Guarantor => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (4,'Collateral','Collateral => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (5,'Add Documents','Add Documents => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (6,'Financial Analysis','Financial Analysis => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (7,'Approval L1','Approval L1 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (8,'Approval L2','Approval L2 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (9,'Approval L3','Approval L3 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (10,'Approval L4','Approval L4 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (11,'Customer Decision','Customer Decision => Approved'
           ,4,'Approved'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (12,'Upload Signed Agreement','Upload Signed Agreement => Approved'
           ,4,'Approved'
           ,1,'2020-03-30','ADMIN',0);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (13,'Disbursement & Case Closure','Disbursement & Case Closure => Approved'
           ,4,'Approved'
           ,1,'2020-03-30','ADMIN',0);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (14,'Rejected','Rejected'
           ,5,'Rejected'
           ,0,'2020-03-30','ADMIN',0);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (15,'Cancelled','Cancelled'
           ,6,'Cancelled'
           ,0,'2020-03-30','ADMIN',0);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (16,'Review','Review'
           ,7,'Review'
           ,0,'2020-03-30','ADMIN',0);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES (17,'Declined','Declined'
           ,6,'Cancelled'
           ,0,'2020-03-30','ADMIN',0);                                
