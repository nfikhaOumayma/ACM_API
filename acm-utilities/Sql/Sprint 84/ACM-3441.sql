-- Customer new field
ALTER TABLE ACM_CUSTOMER ADD BENEFICIAL_EFFECTIVE varchar(100) NULL;
 
-- Prospect new field
ALTER TABLE ACM_CUSTOMER ADD PROSPECTION_SOURCE varchar(256) NULL;
ALTER TABLE ACM_CUSTOMER ADD PROSPECTION_COMMENT text NULL;
ALTER TABLE ACM_CUSTOMER ADD SUPPLIER_RECOMMANDATION bigint NULL;
 
-- Customer IHM field
INSERT INTO ACM_IHM_FIELD
(CODE_FIELD, TYPE_FIELD, DEFAULT_VALUE, TITLE, DESCRIPTION, PLACEHOLDER, MIN, MAX, STEP, SINGLE_SELECT, TYPE_OPTION_VALUE, ORDRE, ID_ACM_ACM_IHM_FORM, ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, SUB_CODE_FIELD, FORM_CONTROL_NAME)
VALUES(N'beneficialEffective', N'TEXT', NULL, N'beneficialEffective', N'beneficialEffective', NULL, NULL, NULL, NULL, NULL, NULL, (
SELECT MAX(acm_ihm_field.ordre) + 1
FROM acm_ihm_field
JOIN ACM_IHM_FORM ON acm_ihm_field.ID_ACM_ACM_IHM_FORM = ACM_IHM_FORM.id
WHERE ACM_IHM_FORM.code_page = 'ADD_CUSTOMER_INDIV'
), (select id from ACM_IHM_FORM  where code_page = 'ADD_CUSTOMER_INDIV'), 1, GETDATE(), N'ADMI', NULL, N'Admin Super (ACM SUPER ADMIN GROUPE)', 1, N'beneficial Effective', N'beneficialEffective');
 
INSERT INTO ACM_IHM_FIELD
(CODE_FIELD, TYPE_FIELD, DEFAULT_VALUE, TITLE, DESCRIPTION, PLACEHOLDER, MIN, MAX, STEP, SINGLE_SELECT, TYPE_OPTION_VALUE, ORDRE, ID_ACM_ACM_IHM_FORM, ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, SUB_CODE_FIELD, FORM_CONTROL_NAME)
VALUES(N'prospectionSource', N'TEXT', NULL, N'prospectionSource', N'prospectionSource', NULL, NULL, NULL, NULL, NULL, NULL, (
SELECT MAX(acm_ihm_field.ordre) + 1
FROM acm_ihm_field
JOIN ACM_IHM_FORM ON acm_ihm_field.ID_ACM_ACM_IHM_FORM = ACM_IHM_FORM.id
WHERE ACM_IHM_FORM.code_page = 'ADD_CUSTOMER_INDIV'
), (select id from ACM_IHM_FORM  where code_page = 'ADD_CUSTOMER_INDIV'), 1, GETDATE(), N'ADMI', NULL, N'Admin Super (ACM SUPER ADMIN GROUPE)', 1, N'prospection Source', N'prospectionSource');
 
INSERT INTO ACM_IHM_FIELD
(CODE_FIELD, TYPE_FIELD, DEFAULT_VALUE, TITLE, DESCRIPTION, PLACEHOLDER, MIN, MAX, STEP, SINGLE_SELECT, TYPE_OPTION_VALUE, ORDRE, ID_ACM_ACM_IHM_FORM, ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, SUB_CODE_FIELD, FORM_CONTROL_NAME)
VALUES(N'prospectionComment', N'TEXT', NULL, N'prospectionComment', N'prospectionComment', NULL, NULL, NULL, NULL, NULL, NULL, (
SELECT MAX(acm_ihm_field.ordre) + 1
FROM acm_ihm_field
JOIN ACM_IHM_FORM ON acm_ihm_field.ID_ACM_ACM_IHM_FORM = ACM_IHM_FORM.id
WHERE ACM_IHM_FORM.code_page = 'ADD_CUSTOMER_INDIV'
), (select id from ACM_IHM_FORM  where code_page = 'ADD_CUSTOMER_INDIV'), 1, GETDATE(), N'ADMI', NULL, N'Admin Super (ACM SUPER ADMIN GROUPE)', 1, N'prospection Comment', N'prospectionComment');
 
INSERT INTO ACM_IHM_FIELD
(CODE_FIELD, TYPE_FIELD, DEFAULT_VALUE, TITLE, DESCRIPTION, PLACEHOLDER, MIN, MAX, STEP, SINGLE_SELECT, TYPE_OPTION_VALUE, ORDRE, ID_ACM_ACM_IHM_FORM, ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, SUB_CODE_FIELD, FORM_CONTROL_NAME)
VALUES(N'supplierRecommandation', N'TEXT', NULL, N'supplierRecommandation', N'supplierRecommandation', NULL, NULL, NULL, NULL, NULL, NULL, (
SELECT MAX(acm_ihm_field.ordre) + 1
FROM acm_ihm_field
JOIN ACM_IHM_FORM ON acm_ihm_field.ID_ACM_ACM_IHM_FORM = ACM_IHM_FORM.id
WHERE ACM_IHM_FORM.code_page = 'ADD_CUSTOMER_INDIV'
), (select id from ACM_IHM_FORM  where code_page = 'ADD_CUSTOMER_INDIV'), 1, GETDATE(), N'ADMI', NULL, N'Admin Super (ACM SUPER ADMIN GROUPE)', 1, N'supplier Recommandation', N'supplierRecommandation');
 
-- Customer Ihm Validators 
INSERT INTO ACM_IHM_VALIDATOR
(CODE_VALIDATOR, [PARAMETER], ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, TYPE_VALIDATOR)
VALUES(N'telephoneMask_TUNISIE', N'([0-9]{8})$', 1, NULL, NULL, NULL, NULL, NULL, N'patternRegExp');
 
INSERT INTO ACM_IHM_VALIDATOR
(CODE_VALIDATOR, [PARAMETER], ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, TYPE_VALIDATOR)
VALUES(N'CIN_Tunisie', N'([0-9]{8})$', 1, NULL, NULL, NULL, NULL, NULL, N'patternRegExp');
 
INSERT INTO ACM_IHM_VALIDATOR
(CODE_VALIDATOR, [PARAMETER], ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, TYPE_VALIDATOR)
VALUES(N'residentIdMask_TUNISIE', N'([0-9]{9})$', 1, NULL, NULL, NULL, NULL, NULL, N'patternRegExp');
 
-- ( You can just add them from field Setting)
UPDATE ACM_IHM_FIELD
SET PLACEHOLDER=N'customer_management.mask_tunisia_national_id'
WHERE title = 'national_ID';
 

UPDATE ACM_IHM_FIELD
SET PLACEHOLDER=N'customer_management.mask_tunisia_phone_number'
WHERE title = 'mobile2';
 
UPDATE ACM_IHM_FIELD
SET PLACEHOLDER=N'customer_management.mask_tunisia_phone_number'
WHERE title = 'mobile';
 
UPDATE ACM_IHM_FIELD
SET PLACEHOLDER=N'customer_management.mask_tunisia_resident_id'
WHERE title = 'resident_ID';
 
 
-- Add generate date of birth and gender setting
INSERT INTO ACM_ENVIRONNEMENT
(ACM_ENVIRONNEMENT_KEY, ACM_ENVIRONNEMENT_VALUE, ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, DESCRIPTION, CATEGORY, TYPE_VALUE)
VALUES(N'GENERATE_DATE_OF_BIRTH_GENDER', N'0', 1, '2023-12-13', NULL, '2023-12-13', N'', 2, N'customer mgt', N'FUNCTIONAL', NULL);