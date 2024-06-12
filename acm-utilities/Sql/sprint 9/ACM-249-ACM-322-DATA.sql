-- ACM_HABILITATION_IHM_ROUTE
-----------------------------
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
		VALUES('ACM','HOME','','page home ACM' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_LOAN_DETAILS','loan-details' ,'page LOAN DETAILS' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_FIELD_VISIT','field-visit' ,'page FIELD VISIT' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_CHECK_GUARANTOR','check-guarantor' ,'page CHECK GUARANTOR' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_CHECK_COLLATERAL','check-collateral' ,'page CHECK COLLATERAL' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_FINANCIAL_ANALYSIS','financial-analysis' ,'page FINANCIAL ANALYSIS' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_CALENDAR','calendar' ,'page CALENDAR' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_CUSTOMER_DECISION','customer-decision' ,'page CUSTOMER DECISION' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_UPLOAD_DOCUMENT','upload-document' ,'page UPLOAD DOCUMENT' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_TASK','task' ,'page TASK' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_LOAN_APPROVAL','loan-approval' ,'page LOAN APPROVAL' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_ADD_DOCUMENT','add-document' ,'page ADD DOCUMENT' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_CUSTOMER_NOTES','customer-notes' ,'page CUSTOMER NOTES' ,1,GETDATE(),'ADMIN',0);
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])		    
		VALUES('ACM','IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement' ,'page UPLOAD SIGNED AGREEMENT' ,1,GETDATE(),'ADMIN',0);

-- ACM_HABILITATION
-----------------------------
-- groupe ID = 1
----------------------------
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'HOME' ,'' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 , 'IHM_LOAN_DETAILS','loan-details' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_FIELD_VISIT','field-visit' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_CHECK_GUARANTOR','check-guarantor' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_CHECK_COLLATERAL','check-collateral'  ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_FINANCIAL_ANALYSIS','financial-analysis',1  ,1,GETDATE(),'ADMIN',0);  

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_CALENDAR','calendar' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_CUSTOMER_DECISION','customer-decision',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_UPLOAD_DOCUMENT','upload-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_TASK','task',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_LOAN_APPROVAL','loan-approval',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_ADD_DOCUMENT','add-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_CUSTOMER_NOTES','customer-notes',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,1 ,'IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement',1  ,1,GETDATE(),'ADMIN',0);		  

-- ACM_HABILITATION
-----------------------------
-- groupe ID = 19
----------------------------
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'HOME' ,'' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 , 'IHM_LOAN_DETAILS','loan-details' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_FIELD_VISIT','field-visit' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_CHECK_GUARANTOR','check-guarantor' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_CHECK_COLLATERAL','check-collateral'  ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_FINANCIAL_ANALYSIS','financial-analysis',1  ,1,GETDATE(),'ADMIN',0);  

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_CALENDAR','calendar' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_CUSTOMER_DECISION','customer-decision',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_UPLOAD_DOCUMENT','upload-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_TASK','task',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_LOAN_APPROVAL','loan-approval',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_ADD_DOCUMENT','add-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_CUSTOMER_NOTES','customer-notes',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,19 ,'IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement',1  ,1,GETDATE(),'ADMIN',0);
 
-- ACM_HABILITATION
-----------------------------
-- groupe ID = 27
----------------------------
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'HOME' ,'' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 , 'IHM_LOAN_DETAILS','loan-details' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_FIELD_VISIT','field-visit' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_CHECK_GUARANTOR','check-guarantor' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_CHECK_COLLATERAL','check-collateral'  ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_FINANCIAL_ANALYSIS','financial-analysis',1  ,1,GETDATE(),'ADMIN',0);  

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_CALENDAR','calendar' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_CUSTOMER_DECISION','customer-decision',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_UPLOAD_DOCUMENT','upload-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_TASK','task',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_LOAN_APPROVAL','loan-approval',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_ADD_DOCUMENT','add-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_CUSTOMER_NOTES','customer-notes',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,27 ,'IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement',1  ,1,GETDATE(),'ADMIN',0);
 
-- ACM_HABILITATION
-----------------------------
-- groupe ID = 22
----------------------------
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'HOME' ,'' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 , 'IHM_LOAN_DETAILS','loan-details' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_FIELD_VISIT','field-visit' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_CHECK_GUARANTOR','check-guarantor' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_CHECK_COLLATERAL','check-collateral'  ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_FINANCIAL_ANALYSIS','financial-analysis',1  ,1,GETDATE(),'ADMIN',0);  

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_CALENDAR','calendar' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_CUSTOMER_DECISION','customer-decision',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_UPLOAD_DOCUMENT','upload-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_TASK','task',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_LOAN_APPROVAL','loan-approval',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_ADD_DOCUMENT','add-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_CUSTOMER_NOTES','customer-notes',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,22 ,'IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement',1  ,1,GETDATE(),'ADMIN',0);
 

-- ACM_HABILITATION
-----------------------------
-- groupe ID = 21
----------------------------
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'HOME' ,'' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 , 'IHM_LOAN_DETAILS','loan-details' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_FIELD_VISIT','field-visit' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_CHECK_GUARANTOR','check-guarantor' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_CHECK_COLLATERAL','check-collateral'  ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_FINANCIAL_ANALYSIS','financial-analysis',1  ,1,GETDATE(),'ADMIN',0);  

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_CALENDAR','calendar' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_CUSTOMER_DECISION','customer-decision',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_UPLOAD_DOCUMENT','upload-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_TASK','task',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_LOAN_APPROVAL','loan-approval',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_ADD_DOCUMENT','add-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_CUSTOMER_NOTES','customer-notes',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,21 ,'IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement',1  ,1,GETDATE(),'ADMIN',0);   


-- ACM_HABILITATION
-----------------------------
-- groupe ID = 23 (DG)
----------------------------
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'HOME' ,'' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 , 'IHM_LOAN_DETAILS','loan-details' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_FIELD_VISIT','field-visit' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_CHECK_GUARANTOR','check-guarantor' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_CHECK_COLLATERAL','check-collateral'  ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_FINANCIAL_ANALYSIS','financial-analysis',1  ,1,GETDATE(),'ADMIN',0);  

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_CALENDAR','calendar' ,1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_CUSTOMER_DECISION','customer-decision',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_UPLOAD_DOCUMENT','upload-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_TASK','task',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_LOAN_APPROVAL','loan-approval',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_ADD_DOCUMENT','add-document',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_CUSTOMER_NOTES','customer-notes',1  ,1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_UPLOAD_SIGNED_AGREEMENT','upload-signed-agreement',1  ,1,GETDATE(),'ADMIN',0); 

INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_NOTIFICATIONS' ,'notification' ,1  ,1,GETDATE(),'ADMIN',0);
 
INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('fullControl','ACM' ,23 ,'IHM_FIND_DOCUMENT' ,'find-document' ,1  ,1,GETDATE(),'ADMIN',0);  
		