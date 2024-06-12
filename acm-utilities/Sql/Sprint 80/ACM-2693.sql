alter table ACM_LOAN_INSTANCE
add IB_IHM_ROOT nvarchar(100);

alter table ACM_WORKFLOW_STEP
add IB_SCREEN varchar(100) ;

alter table ACM_LOAN
add ID_IB_LOAN bigint;

alter table ACM_CUSTOMER
add IB_CUSTOMER_ID bigint;