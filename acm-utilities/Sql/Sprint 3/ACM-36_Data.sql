-----------------------
-----------------------
-- DATA [ACM_USERS]
-----------------------

INSERT INTO ACM_USERS(username,password,ACM_ENABLED, account_portfolio_id, user_extern_id,user_profil_id,responsable_id,DATE_INSERTION,INSERT_BY) 
    VALUES ('john', '$2a$04$Ts1ry6sOr1BXXie5Eez.j.bsvqC0u3x7xAwOInn2qrItwsUUIC9li', 1,0,0,0,0,GETDATE(),'ADMIN'); --john@123
INSERT INTO ACM_USERS (username,password,ACM_ENABLED, account_portfolio_id, user_extern_id,user_profil_id,responsable_id,DATE_INSERTION,INSERT_BY)
    VALUES ('kelly','$2a$04$qkCGgz.e5dkTiZogvzxla.KXbIvWXrQzyf8wTPJOOJBKjtHAQhoBa', 1,0,0,0,0,GETDATE(),'ADMIN'); --kelly@123

-----------------------
-----------------------
-- DATA [ACM_GROUPE]
-----------------------

INSERT INTO [dbo].[ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('ADMINISTRATOR','Administrator','Groupe pour Administrator',1,GETDATE(),'ADMIN',0);
	 
INSERT INTO [dbo].[ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('TELLER','Teller','Groupe pour Teller',1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('LOAN_MONITOR','Loan Monitor','Groupe pour Loan Monitor',1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('BRANCH_MANAGER','Branch Manager','Groupe pour Branch Manager',1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('LOAN_OFFICER','Loan Officer','Groupe pour Loan Officer',1,GETDATE(),'ADMIN',0);

INSERT INTO [dbo].[ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('EXPRESS','Express','Groupe pour Express',1,GETDATE(),'ADMIN',0);	 


-----------------------
-----------------------
-- DATA [ACM_USERS_GROUPE]
-----------------------

INSERT INTO [dbo].[ACM_USERS_GROUPE]([ID_ACM_GROUPE],[USERNAME]) VALUES(1,'john');
INSERT INTO [dbo].[ACM_USERS_GROUPE]([ID_ACM_GROUPE],[USERNAME]) VALUES(2,'kelly'); 

-----------------------
-----------------------
-- DATA [OAUTH_CLIENT_DETAILS]
-----------------------
INSERT INTO  oauth_client_details ( client_id,client_secret,resource_ids,scope,authorized_grant_types,authorities,access_token_validity,refresh_token_validity)
VALUES  ( 'talysclient', '$2a$08$ePUWmsLTqNezRk7MCUfg6.HU3RUO3N2M6H.Xj0gMvKiUsGgvg/Fve', --appclient@123
    'talysapp', 'read,write', 'authorization_code,check_token,refresh_token,password', 'ROLE_CLIENT',  7000,  250000 );

-----------------------
-----------------------
-- DATA [ACM_SETTING_MOTIFS_REJET]
-- Table : select * from CULoanCancelReasons;
-- CATEGORIE : REJET / CORRECTIFS
-----------------------    

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('REJECT','INCORRECT_APPLICATION','Incorrect Application','Incorrect Application',1,GETDATE(),'ADMIN',0,1);

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('REJECT','INSUFFICIENT_HISTORY','Insufficient History','Insufficient History',1,GETDATE(),'ADMIN',0,2);

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('REJECT','LOAN_IN_ARREARS','Loan in Arrears','Loan in Arrears',1,GETDATE(),'ADMIN',0,5);

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('REJECT','PREVIOUS_BAD_DEBT','Previous Bad Debt','Previous Bad Debt',1,GETDATE(),'ADMIN',0,5);

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('REJECT','DECEASED','Deceased','Deceased',1,GETDATE(),'ADMIN',0,5);
        
INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('REVIEW','INSUFFICIENT_DATA','Insufficient Data','Insufficient Data',1,GETDATE(),'ADMIN',0,5);   
      
INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('DECLINE','CLIENT_ABANDONED','Client Has Abandoned','Client Has Abandoned',1,GETDATE(),'ADMIN',0,5); 

INSERT INTO [dbo].[ACM_SETTING_MOTIFS_REJET]  ([CATEGORIE],[CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[CODE_EXTERNAL])
     VALUES ('DECLINE','ALLOCATED_AMOUNT','The Allocated Amount Does Not Meet The Client''s Needs','The Allocated Amount Does Not Meet The Client''s Needs',1,GETDATE(),'ADMIN',0,5);                     
      