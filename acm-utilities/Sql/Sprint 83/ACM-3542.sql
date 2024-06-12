-- Add identity number and isCustomer columns to Supplier
ALTER TABLE ACM_SUPPLIER ADD IDENTITY_NUMBER varchar(256) NULL;
ALTER TABLE ACM_SUPPLIER ADD IS_CUSTOMER bit NULL;

-- Rename Patent Number to Register number in Supplier
EXEC sys.sp_rename N'ACM_SUPPLIER.PATENT_NUMBER' , N'REGISTER_NUMBER', 'COLUMN';

-- Add isSupplier columns to Customer 
ALTER TABLE ACM_CUSTOMER ADD IS_SUPPLIER bit NULL;

-- Add patent number field to add customer form 
INSERT INTO ACM_IHM_FIELD
(CODE_FIELD, TYPE_FIELD, DEFAULT_VALUE, TITLE, DESCRIPTION, PLACEHOLDER, MIN, MAX, STEP, SINGLE_SELECT, TYPE_OPTION_VALUE, ORDRE, ID_ACM_ACM_IHM_FORM, ACM_ENABLED, DATE_INSERTION, INSERT_BY, DATE_LAST_UPDATE, UPDATED_BY, ACM_VERSION, SUB_CODE_FIELD, FORM_CONTROL_NAME)
VALUES(N'patentNumber', N'TEXT', NULL, N'patentNumber', N'patentNumber', NULL, NULL, NULL, NULL, NULL, NULL, (
SELECT MAX(acm_ihm_field.ordre) + 1
FROM acm_ihm_field
JOIN ACM_IHM_FORM ON acm_ihm_field.ID_ACM_ACM_IHM_FORM = ACM_IHM_FORM.id
WHERE ACM_IHM_FORM.code_page = 'ADD_CUSTOMER_INDIV'
), (select id from ACM_IHM_FORM  where code_page = 'ADD_CUSTOMER_INDIV'), 1, GETDATE(), N'ADMI', NULL, N'Admin Super (ACM SUPER ADMIN GROUPE)', 1, N'patent Number', N'patentNumber');

-- REGISTER_NUMBER unique
--ALTER TABLE ACM_SUPPLIER
--ADD CONSTRAINT unique_REGISTER_NUMBER_constraint UNIQUE (REGISTER_NUMBER);
-- IDENTITY_NUMBER unique
--ALTER TABLE ACM_SUPPLIER
--ADD CONSTRAINT unique_IDENTITY_NUMBER_constraint UNIQUE (IDENTITY_NUMBER);
-- we should delete those two constraints
ALTER TABLE ACM_SUPPLIER
DROP CONSTRAINT unique_REGISTER_NUMBER_constraint, unique_IDENTITY_NUMBER_constraint;

-- And add an index unique and nullable constraint
CREATE UNIQUE INDEX UNIQUE_NULLABLE_SUPPLIER_REGISTER_NUMBER_CONSTRAINT 
    ON ACM_SUPPLIER (REGISTER_NUMBER)
    WHERE REGISTER_NUMBER IS NOT NULL;

CREATE UNIQUE INDEX UNIQUE_NULLABLE_SUPPLIER_IDENTITY_NUMBER_CONSTRAINT 
    ON ACM_SUPPLIER (IDENTITY_NUMBER)
    WHERE IDENTITY_NUMBER IS NOT NULL;

CREATE UNIQUE INDEX UNIQUE_NULLABLE_CUSTOMER_REGISTER_NUMBER_CONSTRAINT 
    ON ACM_CUSTOMER (REGISTER_NUMBER)
    WHERE REGISTER_NUMBER IS NOT NULL;
