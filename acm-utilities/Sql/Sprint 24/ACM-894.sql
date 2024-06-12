--------------ACM_HABILITATION_ROUTE
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_CUSTOMER_EDIT','customer-edit-menu' ,'page EDIT CUSTOMER MENU' ,1,GETDATE(),'ADMIN',0);
		
INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_SEARCH','search' ,'page SEARCH' ,1,GETDATE(),'ADMIN',0);
		INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_CUSTOMER_LIST','customer-list' ,'page CUSTOMER LIST' ,1,GETDATE(),'ADMIN',0);
		INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_CUSTOMER_MESSAGE','customer-message' ,'page CUSTOMER MESSAGE' ,1,GETDATE(),'ADMIN',0);
		
		INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_REPPORTS_LIST','reports-list' ,'page REPPORT LIST' ,1,GETDATE(),'ADMIN',0);
		
		INSERT INTO [dbo].[ACM_HABILITATION_IHM_ROUTE]([CLIENT],[CODE_IHM_ROUTE],[IHM_ROUTE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])	        
		VALUES('ACM','IHM_ANALYTICS','analytics' ,'page ANALYTICS' ,1,GETDATE(),'ADMIN',0);
		
----------------------------
-- ACM_HABILITATION_IHM_BUTTON
-----------------------------
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('ACM','SAVE','IHM_LOAN_DETAILS','SAVE',18,1,GETDATE(),'ADMIN',0);	


-----------------------------
-- ACM_HABILITATION
-----------------------------
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,1 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,0,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,1 ,'IHM_SEARCH' ,'search' ,'IHM'  ,0,GETDATE(),'ADMIN',0);
 
  
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,1 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,0,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,1 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,0,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,1 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,0,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('READ','ACM' ,1 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 

 -----------------------------------------------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,2 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,2 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,2 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,2 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,2 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,2 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 
 ------------------------------------------------------------------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,4 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,4 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,4 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,4 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,4 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

   INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,4 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 -----------------------------------------------------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,5 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,5 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,5 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,5 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,5 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,5 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 
 -----------------------------------------------------------------------------------
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,6 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,6 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,6 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,6 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,6 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

   INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,6 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 ------------------------------------------------------------
 
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,7 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,7 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,7 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,7 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,7 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

   INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,7 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 -------------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,8 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,8 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,8 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,8 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,8 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,8 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 
 ----------------------------------------
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,9 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,9 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,9 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,9 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,9 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('READ','ACM' ,9 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 ------------------------------------------
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,10 ,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,10 ,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,10 ,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,10 ,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,10 ,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,10 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 ---------------------------------------
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,11,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,11,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,11,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,11,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,11,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

   INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,11 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 -------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,12,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,12,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,12,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,12,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,12,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,12 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 ----------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,13,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,13,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,13,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,13,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,13,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,13 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 ------------------------------------------
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,14,'IHM_CUSTOMER_EDIT' ,'customer-edit-menu' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,14,'IHM_SEARCH' ,'search' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,14,'IHM_CUSTOMER_LIST' ,'customer-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);
 
 INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,14,'IHM_CUSTOMER_MESSAGE' ,'customer-message' ,'IHM'  ,1,GETDATE(),'ADMIN',0);	
 
  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
 VALUES('READ','ACM' ,14,'IHM_REPPORTS_LIST' ,'reports-list' ,'IHM'  ,1,GETDATE(),'ADMIN',0);

  INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
VALUES('EXECUTE','ACM' ,14 ,'IHM_LOAN_DETAILS' ,'loan-details' ,'SAVE'  ,0,GETDATE(),'ADMIN',0);	
 -----------------------------------------
 