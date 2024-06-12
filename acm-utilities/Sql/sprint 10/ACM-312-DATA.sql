-----------------------
-----------------------
-- DATA [ACM_SETTING_STATUT_WORKFLOW]
-----------------------
 
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (1,'Initial Check','Initial Check => New'
           ,1,'New'
           ,1,'2020-03-30','ADMIN',0,'loan-details');

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (2,'Field visit','Field visit => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0,'field-visit');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (3,'Guarantor','Guarantor => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0,'check-guarantor');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (4,'Collateral','Collateral => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0,'check-collateral');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (5,'Add Documents','Add Documents => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0,'upload-document');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (6,'Financial Analysis','Financial Analysis => Drafts'
           ,2,'Drafts'
           ,1,'2020-03-30','ADMIN',0,'financial-analysis');

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (7,'Approval L1','Approval L1 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0,'loan-approval');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (8,'Approval L2','Approval L2 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0,'loan-approval');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (9,'Approval L3','Approval L3 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0,'loan-approval');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (10,'Approval L4','Approval L4 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,'2020-03-30','ADMIN',0,'loan-approval');

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (11,'Customer Decision','Customer Decision => Approved'
           ,4,'Approved'
           ,1,'2020-03-30','ADMIN',0,'customer-decision');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (12,'Upload Signed Agreement','Upload Signed Agreement => Approved'
           ,4,'Approved'
           ,1,'2020-03-30','ADMIN',0,'upload-signed-agreement');
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (13,'Disbursement & Case Closure','Disbursement & Case Closure => Approved'
           ,4,'Approved'
           ,1,'2020-03-30','ADMIN',0,'loan-approval');
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (14,'Rejected','Rejected'
           ,5,'Rejected'
           ,0,'2020-03-30','ADMIN',0,'loan-approval');
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (15,'Cancelled','Cancelled'
           ,6,'Cancelled'
           ,0,'2020-03-30','ADMIN',0,'loan-approval');

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (16,'Review','Review'
           ,7,'Review'
           ,0,'2020-03-30','ADMIN',0,'loan-details');
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT)
     VALUES (17,'Declined','Declined'
           ,6,'Cancelled'
           ,0,'2020-03-30','ADMIN',0,'loan-approval');