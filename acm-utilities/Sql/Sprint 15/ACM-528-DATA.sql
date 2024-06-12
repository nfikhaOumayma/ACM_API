-----------------------
-----------------------
-- DATA [ACM_SETTING_STATUT_WORKFLOW] for 'ACM' & 'process_loan_final'
-----------------------
 
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (1,'Initial Check','Initial Check => New'
           ,1,'New'
           ,1,GETDATE(),'ADMIN',0,'loan-details', 'ACM','process_loan_final',1);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (2,'Field visit','Field visit => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'field-visit', 'ACM','process_loan_final',2);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (3,'Guarantor','Guarantor => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'check-guarantor', 'ACM','process_loan_final',3);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (4,'Collateral','Collateral => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'check-collateral', 'ACM','process_loan_final',4);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (5,'Add Documents','Add Documents => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'upload-document', 'ACM','process_loan_final',5);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (6,'Financial Analysis','Financial Analysis => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'financial-analysis', 'ACM','process_loan_final',6);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (7,'Approval L1','Approval L1 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',7);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (8,'Approval L2','Approval L2 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',8);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (9,'Approval L3','Approval L3 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',9);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (10,'Approval L4','Approval L4 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',10);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (11,'Customer Decision','Customer Decision => Approved'
           ,4,'Approved'
           ,1,GETDATE(),'ADMIN',0,'customer-decision', 'ACM','process_loan_final',11);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (12,'Upload Signed Agreement','Upload Signed Agreement => Approved'
           ,4,'Approved'
           ,1,GETDATE(),'ADMIN',0,'upload-signed-agreement', 'ACM','process_loan_final',12);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (13,'Disbursement & Case Closure','Disbursement & Case Closure => Approved'
           ,4,'Approved'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',13);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (14,'Rejected','Rejected'
           ,5,'Rejected'
           ,0,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',14);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (15,'Cancelled','Cancelled'
           ,6,'Cancelled'
           ,0,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',15);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (16,'Review','Review'
           ,7,'Review'
           ,0,GETDATE(),'ADMIN',0,'loan-details', 'ACM','process_loan_final',16);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (17,'Declined','Declined'
           ,6,'Cancelled'
           ,0,GETDATE(),'ADMIN',0,'loan-approval', 'ACM','process_loan_final',17);
           
           
-----------------------
-----------------------
-- DATA [ACM_SETTING_STATUT_WORKFLOW] for 'BRJMF' & 'process_loan_BRJMF'
-----------------------
 
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (21,'Update Loan Data','Update Loan Data => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'loan-details', 'BRJMF','process_loan_BRJMF',1);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (3,'Guarantor','Guarantor => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'check-guarantor', 'BRJMF','process_loan_BRJMF',2);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (4,'Collateral','Collateral => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'check-collateral', 'BRJMF','process_loan_BRJMF',3);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (5,'Add Documents','Add Documents => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'upload-document', 'BRJMF','process_loan_BRJMF',4);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (18,'Screening','Screening => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'screening', 'BRJMF','process_loan_BRJMF',5);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (2,'Field visit','Field visit => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'field-visit', 'BRJMF','process_loan_BRJMF',6);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (19,'Audit','Audit => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'loan-review', 'BRJMF','process_loan_BRJMF',7);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (20,'Risk','Risk => Drafts'
           ,2,'Drafts'
           ,1,GETDATE(),'ADMIN',0,'loan-review', 'BRJMF','process_loan_BRJMF',8);

---------------------------------
---------------------------------
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (7,'Approval L1','Approval L1 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',9);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (8,'Approval L2','Approval L2 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',10);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (9,'Approval L3','Approval L3 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',11);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (10,'Approval L4','Approval L4 => Awaiting Approval'
           ,3,'Awaiting Approval'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',12);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (11,'Customer Decision','Customer Decision => Approved'
           ,4,'Approved'
           ,1,GETDATE(),'ADMIN',0,'customer-decision', 'BRJMF','process_loan_BRJMF',13);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (12,'Upload Signed Agreement','Upload Signed Agreement => Approved'
           ,4,'Approved'
           ,1,GETDATE(),'ADMIN',0,'upload-signed-agreement', 'BRJMF','process_loan_BRJMF',14);
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (13,'Disbursement & Case Closure','Disbursement & Case Closure => Approved'
           ,4,'Approved'
           ,1,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',15);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (14,'Rejected','Rejected'
           ,5,'Rejected'
           ,0,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',16);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (15,'Cancelled','Cancelled'
           ,6,'Cancelled'
           ,0,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',17);

INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (16,'Review','Review'
           ,7,'Review'
           ,0,GETDATE(),'ADMIN',0,'loan-details', 'BRJMF','process_loan_BRJMF',18);
           
INSERT INTO [dbo].[ACM_SETTING_STATUT_WORKFLOW] ([CODE],[LIBELLE],[DESCRIPTION],[CODE_STATUT_LOAN],[STATUT_LOAN],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION], IHM_WEB_ROOT, [CLIENT], [BPMN_PROCESS_NAME], [ORDER_ETAPE_PROCESS])
     VALUES (17,'Declined','Declined'
           ,6,'Cancelled'
           ,0,GETDATE(),'ADMIN',0,'loan-approval', 'BRJMF','process_loan_BRJMF',19);
                   