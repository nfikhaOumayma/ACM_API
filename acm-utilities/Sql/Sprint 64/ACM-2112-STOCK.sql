update ACM_CUSTOMER set ENABLE_CRITICAL_DATA = 1, UPDATED_BY = 'TALYS_SCRIPT,V9', DATE_LAST_UPDATE = GETDATE()  where ID_ACM_CUSTOMER in (
select ID_ACM_CUSTOMER from acm_loan where STATUT in (3,4,7,8) or ETAPE_WORKFLOW = 20)

