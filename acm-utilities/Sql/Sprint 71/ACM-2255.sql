
alter table ACM_LOAN add STATUT_LIBELLE varchar(100);


update  ll set ll.STATUT_LIBELLE = (select top 1 Libelle from ACM_LOAN_INSTANCE i where i.ID_ACM_LOAN = ll.ID_ACM_LOAN and i.code =ll.STATUT_WORKFLOW) 
 FROM ACM_LOAN ll ;


-- select distinct STATUT_WORKFLOW from ACM_LOAN where STATUT_LIBELLE is null

update ACM_LOAN set STATUT_LIBELLE = 'Initial Check'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 1;
update ACM_LOAN set STATUT_LIBELLE = 'Field visit'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 2;
update ACM_LOAN set STATUT_LIBELLE = 'Guarantor'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 3;
update ACM_LOAN set STATUT_LIBELLE = 'Collateral'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 4;
update ACM_LOAN set STATUT_LIBELLE = 'Add Documents'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 5;
update ACM_LOAN set STATUT_LIBELLE = 'Financial Analysis'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 6;
update ACM_LOAN set STATUT_LIBELLE = 'Approval L1'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 7;
update ACM_LOAN set STATUT_LIBELLE = 'Approval L2'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 8;
update ACM_LOAN set STATUT_LIBELLE = 'Approval L3'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 9;
update ACM_LOAN set STATUT_LIBELLE = 'Approval L4'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 10;
update ACM_LOAN set STATUT_LIBELLE = 'Customer Decision'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 11;
update ACM_LOAN set STATUT_LIBELLE = 'Docs Signing'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 12;
update ACM_LOAN set STATUT_LIBELLE = 'Disbursement & Case Closure'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 13;
update ACM_LOAN set STATUT_LIBELLE = 'Rejected'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 14;
update ACM_LOAN set STATUT_LIBELLE = 'Cancelled'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 15;
update ACM_LOAN set STATUT_LIBELLE = 'Review'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 16;
update ACM_LOAN set STATUT_LIBELLE = 'Declined'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 17;
update ACM_LOAN set STATUT_LIBELLE = 'Screening'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 18;
update ACM_LOAN set STATUT_LIBELLE = 'Audit'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 19;
update ACM_LOAN set STATUT_LIBELLE = 'Risk'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 20;
update ACM_LOAN set STATUT_LIBELLE = 'Complete Loan Data'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 21;
update ACM_LOAN set STATUT_LIBELLE = 'Issued'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 22;
update ACM_LOAN set STATUT_LIBELLE = 'Central Revision'  where STATUT_LIBELLE is null and STATUT_WORKFLOW = 23;


------  Add Status column for Collection
alter table ACM_COLLECTION add STATUT_LIBELLE varchar(100);
alter table ACM_COLLECTION add STATUT_LIBELLE_DONE varchar(100);
