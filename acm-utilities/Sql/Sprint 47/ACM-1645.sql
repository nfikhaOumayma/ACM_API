-- Update loans where statut is Issued
update ACM_LOAN set STATUT = 8 where STATUT = 4 and STATUT_WORKFLOW = 22
-- Update  ACM_SETTING_STATUT_WORKFLOW where libelle is Issued
update ACM_SETTING_STATUT_WORKFLOW set CODE_STATUT_LOAN = 8 where LIBELLE = 'Issued'
