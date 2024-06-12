INSERT INTO ACM_HABILITATION_IHM_ROUTE (CLIENT, CODE_IHM_ROUTE, IHM_ROUTE,DESCRIPTION,RACINE_ID,ACM_ENABLED,DATE_INSERTION,INSERT_BY,DATE_LAST_UPDATE,UPDATED_BY,ACM_VERSION,SETTINGS_WORKFLOW,MODULE_ROUTE)
VALUES ('ACM', 'IHM_SIMULATION_LOAN','/acm/simulation-loan','page simulation loan',3,1,'2020-09-12','ADMIN','','',0,0,'acm')


INSERT INTO [dbo].[ACM_HABILITATION]([ACTIONS],[CLIENT],[GROUPE_ID],[ACM_HABILITATION],[ACM_WEB_ROUTE],[VALUE],[ACM_ENABLED],[DATE_INSERTION],[INSERT_BY],[ACM_VERSION],[DESCRIPTION])
select 'READ','ACM' ,id_acm_groupe ,'IHM_SIMULATION_LOAN' ,'simulation-loan' ,'IHM'  ,1,GETDATE(),'ADMIN',0, 'SIMULATION LOAN SCREEN' from ACM_GROUPE

 

UPDATE ACM_HABILITATION
SET ID_ACM_HABILITATION_IHM_ROUTE = IHM_ROUTE.ID_ACM_HABILITATION_IHM_ROUTE
FROM ACM_HABILITATION
INNER JOIN ACM_HABILITATION_IHM_ROUTE AS IHM_ROUTE
ON ACM_HABILITATION.ACM_HABILITATION = 'IHM_SIMULATION_LOAN'