 insert into ACM_ENVIRONNEMENT (ACM_ENVIRONNEMENT_KEY,ACM_ENVIRONNEMENT_VALUE,ACM_ENABLED)
 values('VNEURON_API_REQUEST_AUTH_URL','http://172.16.4.33:8081/kyc-web-restful/xauth/authenticate/',1),
	   ('VNEURON_API_REQUEST_SEARCH_CUSTOMER_URL','http://172.16.4.33:8081/kyc-web-restful/search/searchPersonCustomer',1),
	   ('VNEURON_API_REQUEST_DECISION_URL','http://172.16.4.33:8081/kyc-web-restful/search/getSearchDecisions/',1),
	   ('VNEURON_API_REQUEST_ONBOARD_CUSTOMER_URL','http://172.16.4.33:8081/kyc-web-restful/onboarding/v1/onboardCustomer',1),
	   ('VNEURON_API_REQUEST_RISK_CALCULATION_URL','http://172.16.4.33:8081/kyc-web-restful/risk-calculation/risk-calculation-result/',1),
	   ('VNEURON_API_AUTH_USERNAME','superadmin',1),
		('VNEURON_API_AUTH_PASSWORD','15e7cd13-fa56-4765-b9b4-0d2f92c0e574',1) ; 

CREATE TABLE [dbo].[ACM_3RD_PARTY_MAPPING_DATA](
	[ID_ACM_3RD_PARTY_MAPPING_DATA] [bigint] IDENTITY(1,1) NOT NULL,
	[CATEGORY] [varchar](256) NULL,
	[ORIGINAL_DATA_TABLE_NAME] [varchar](256) NULL,
	[ORIGINAL_DATA_ID] [bigint]  NULL,
	[ORIGINAL_DATA] [varchar](256) NULL,
	[MAPPED_DATA] [varchar](256) NULL,
	[DESCRIPTION] [varchar](512) NULL,
	[ACM_ENABLED] [bit] NOT NULL,
	[DATE_INSERTION] [datetime] NULL,
	[INSERT_BY] [varchar](256) NULL,
	[DATE_LAST_UPDATE] [datetime] NULL,
	[UPDATED_BY] [varchar](256) NULL,
	[ACM_VERSION] [int] NULL,
);


 




insert into ACM_3RD_PARTY_MAPPING_DATA(CATEGORY,ORIGINAL_DATA,MAPPED_DATA,ACM_ENABLED ) values ('NATIONALITY','TUNISIE','TN',1) ;

	  
