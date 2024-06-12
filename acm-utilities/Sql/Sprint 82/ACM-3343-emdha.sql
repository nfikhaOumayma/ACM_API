 
 --delete old configuration of Emdha api in uat environement then execute the new config 
 delete from ACM_ENVIRONNEMENT where ACM_ENVIRONNEMENT_KEY like '%EMDHA_API_REQUEST%'
 
 --EMDHA
 insert into ACM_ENVIRONNEMENT (ACM_ENVIRONNEMENT_KEY,ACM_ENVIRONNEMENT_VALUE,ACM_ENABLED)
 values('EMDHA_API_REQUEST_TEMPDIR','/root/emdha_files/',1),
	   ('EMDHA_API_REQUEST_LICENCEFILE','/root/emdha_files/UAT-BFSI-BTSHL01.lic',1),
	   ('EMDHA_API_REQUEST_PFXFILEPATH','/root/emdha_files/ClientSDKDEV.pfx',1),
	   ('EMDHA_API_REQUEST_PFXPASSWORD','emdha',1),
	   ('EMDHA_API_REQUEST_PFXALIAS','fin',1),
	   ('EMDHA_API_REQUEST_SIPID','UAT-BFSI-BTSHL01',1),
	   ('EMDHA_API_REQUEST_ESIGNURL','https://esign-dev.emdha.sa/eSign/SignDoc',1),
	   ('EMDHA_API_REQUEST_ORGANIZATION','Sanad Finance',1) ;
