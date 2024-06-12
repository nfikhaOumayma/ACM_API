--update all customer which have a loan at CR1 Step
update ACM_CUSTOMER set ENABLE_CRITICAL_DATA = 0 where ID_ACM_CUSTOMER in (
select ID_ACM_CUSTOMER from acm_loan where ETAPE_WORKFLOW = 19)

--update all customers which have a loan come back to review from CR1 step
update ACM_CUSTOMER set ENABLE_CRITICAL_DATA = 0 where ID_ACM_CUSTOMER in (
select ID_ACM_CUSTOMER from acm_loan where STATUT = 7 and UPDATED_BY like '%(1 مراجعة مركزية)%')  