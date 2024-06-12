-----------------------
-----------------------
-- DATA [OAUTH_CLIENT_DETAILS]
-----------------------
INSERT INTO  oauth_client_details ( client_id,client_secret,resource_ids,scope,authorized_grant_types,authorities,access_token_validity,refresh_token_validity)
VALUES  ( 'ibclient', '$2a$08$ePUWmsLTqNezRk7MCUfg6.HU3RUO3N2M6H.Xj0gMvKiUsGgvg/Fve', --appclient@123
    'talysapp', 'read,write', 'authorization_code,check_token,refresh_token,password', 'ROLE_CLIENT',  7000,  250000 );
    

-----------------------
-----------------------
-- DATA [ACM_GROUPE]
-----------------------
INSERT INTO [ACM_GROUPE] ([CODE],[LIBELLE],[DESCRIPTION],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
     VALUES('IB_GROUP','INTERNET BANKING GROUP','Groupe For Internet Banking User',1,GETDATE(),'ADMIN',0); 