Alter table ACM_HABILITATION_IHM_ROUTE
add SETTINGS_WORKFLOW BIGINT;
update ACM_HABILITATION_IHM_ROUTE set SETTINGS_WORKFLOW = 1 where IHM_ROUTE  = '/check-collateral'