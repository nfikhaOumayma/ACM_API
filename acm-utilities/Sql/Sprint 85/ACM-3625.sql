
--- ACM_HABILITATION_IHM_BUTTON PAYMENT FROM ABACUS
 
INSERT INTO [dbo].[ACM_HABILITATION_IHM_BUTTON]([CLIENT],[CODE_IHM_BUTTON],[IHM_BUTTON],[DESCRIPTION],[ID_ACM_HABILITATION_IHM_ROUTE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION])
select 'ACM','PAYMENT_FROM_ACM','IHM_CUSTOMER_360_DETAILS','payment from acm',ID_ACM_HABILITATION_IHM_ROUTE,1,GETDATE(),'ADMIN',0  from ACM_HABILITATION_IHM_ROUTE where CODE_IHM_ROUTE = 'IHM_CUSTOMER_360_DETAILS';


INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[DESCRIPTION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_CUSTOMER_360_DETAILS' ,'customer-360-details' ,'PAYMENT_FROM_ACM'  ,1,GETDATE(),'ADMIN',0, 'PAYMENT FROM ACM' from ACM_GROUPE
 

UPDATE ACM_HABILITATION
SET ID_ACM_HABILITATION_IHM_ROUTE = IHM_ROUTE.ID_ACM_HABILITATION_IHM_ROUTE
FROM ACM_HABILITATION
INNER JOIN ACM_HABILITATION_IHM_ROUTE AS IHM_ROUTE
ON ACM_HABILITATION.ACM_HABILITATION = 'IHM_CUSTOMER_360_DETAILS'

