-- Create Unicity index for EMPLOYEE_ID
CREATE UNIQUE NONCLUSTERED INDEX idx_EMPLOYEE_ID
ON acm_users(EMPLOYEE_ID)
WHERE EMPLOYEE_ID IS NOT NULL;