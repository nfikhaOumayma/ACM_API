CREATE or ALTER PROCEDURE [dbo].[ACM_PROC_INCENTIVE_OPERATION] (@total INT OUTPUT) as 
declare @name  nvarchar(100)
declare @branch nvarchar(100)
declare @username varchar(100)
declare @account_id int
declare @customer_id int
declare @loan_issued int
declare @date datetime
declare @setting_incetive_operation_MEL int
declare @incentive_type_MEL varchar(256)
declare @setting_incetive_operation_VSE int
declare @incentive_type_VSE varchar(256)
declare @total_issued_loan_MEL int
declare @total_amount_issued_loan_MEL money
declare @incetive_value_MEL int
declare @total_issued_loan_VSE int
declare @total_amount_issued_loan_VSE money
declare @incetive_value_VSE int
declare @incetive_value int


set @total = 0
set @date = '2021-10-31T23:59:00'
DELETE FROM ACM_INCENTIVE_RUN_OPERATION where MONTH = DATENAME(MONTH,@date) and DATENAME(YEAR,RUN_DATE) = DATENAME(YEAR,@date)

declare @firstDate datetime
SET @firstDate = (SELECT DATEADD(month, DATEDIFF(month, 0, @date), 0))

	-- FIND SETTING OPERATION INCENTIVE FOR MEL PRODUCTS
	SELECT 
		@setting_incetive_operation_MEL = ISNULL(INCENTIVE_VALUE, 0),
		@incentive_type_MEL = ISNULL(INCENTIVE_TYPE.CODE, '')
	FROM ACM_INCENTIVE_OPERATION, 
	ACM_INCENTIVE_SETTING_CONSTANT INCENTIVE_TYPE,
	ACM_PRODUCT_CATEGORY
	WHERE ACM_INCENTIVE_OPERATION.INCENTIVE_ROLE = 'BRANCH_OPERATION'
		  AND INCENTIVE_TYPE.ID_ACM_INCENTIVE_SETTING_CONSTANT = ACM_INCENTIVE_OPERATION.INCENTIVE_TYPE_ID
		  AND ACM_INCENTIVE_OPERATION.PRODUCT_ID = ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY
		  AND ACM_PRODUCT_CATEGORY.CODE = 'MEL'


	-- FIND SETTING OPERATION INCENTIVE FOR VSE PRODUCTS
	SELECT 
		@setting_incetive_operation_VSE = ISNULL(INCENTIVE_VALUE, 0),
		@incentive_type_VSE = ISNULL(INCENTIVE_TYPE.CODE, '')
	FROM ACM_INCENTIVE_OPERATION, 
	ACM_INCENTIVE_SETTING_CONSTANT INCENTIVE_TYPE,
	ACM_PRODUCT_CATEGORY
	WHERE ACM_INCENTIVE_OPERATION.INCENTIVE_ROLE = 'BRANCH_OPERATION'
		  AND INCENTIVE_TYPE.ID_ACM_INCENTIVE_SETTING_CONSTANT = ACM_INCENTIVE_OPERATION.INCENTIVE_TYPE_ID
		  AND ACM_INCENTIVE_OPERATION.PRODUCT_ID = ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY
		  AND ACM_PRODUCT_CATEGORY.CODE = 'VSE'

	DECLARE curseur_incentive_operation CURSOR FOR
		SELECT
			Name + ' ' + SUR_NAME,
			ACM_USERS.USERNAME,
			ACM_USERS.BRANCHE_NAME,
			ACCOUNT_PORTFOLIO_ID
		FROM ACM_USERS, ACM_USERS_GROUPE, ACM_GROUPE

		WHERE ACM_USERS.USERNAME = ACM_USERS_GROUPE.USERNAME
			  AND ACM_GROUPE.ID_ACM_GROUPE = ACM_USERS_GROUPE.ID_ACM_GROUPE
			  AND ACM_GROUPE.CODE = 'BRANCH_OPERATION'
			  and ACM_USERS.ACM_ENABLED = 1
		ORDER BY Name + SUR_NAME

	OPEN curseur_incentive_operation
	FETCH curseur_incentive_operation INTO @name, @username, @branch, @account_id
	WHILE @@FETCH_STATUS = 0
	BEGIN		
			set @total = @total + 1
			-- Calcule INCETIVE FOR MEL PRODUCT
			SELECT 
			   @total_issued_loan_MEL = ISNULL(COUNT(*), 0), 
			   @total_amount_issued_loan_MEL = ISNULL(SUM(Tamkeen_Support.dbo.CULoanPart.IssueAmount), 0)		
			FROM Tamkeen_Support.dbo.CUAccount, 
				 Tamkeen_Support.dbo.CULoan, 
				 Tamkeen_Support.dbo.CULoanpart, 
				 ACM_LOAN_PARTICIPANTS, 
				 ACM_LOAN
			WHERE Tamkeen_Support.dbo.CUAccount.CUAccountID = Tamkeen_Support.dbo.CULoan.CUAccountID
			AND Tamkeen_Support.dbo.CULoan.CULoanID = Tamkeen_Support.dbo.CULoanpart.CULoanID
			AND Tamkeen_Support.dbo.CULoan.Status = 4
			AND Tamkeen_Support.dbo.CULoanpart.issueDate between @firstDate and @date
			AND ACM_LOAN.ID_LOAN_EXTERN = Tamkeen_Support.dbo.CULoan.CULoanID
			AND ACM_LOAN_PARTICIPANTS.ID_ACM_LOAN =  ACM_LOAN.ID_ACM_LOAN
			AND ACM_LOAN_PARTICIPANTS.USERNAME = @username
			AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(
				(SELECT PRODUCT_ID_LIST FROM ACM_PRODUCT_CATEGORY WHERE CODE = 'MEL'), ','))

				IF @incentive_type_MEL = 'FIXED'
					BEGIN
						SET @incetive_value_MEL = @total_issued_loan_MEL * @setting_incetive_operation_MEL
					END
				ELSE IF @incentive_type_MEL = 'PERCENTAGE'
					BEGIN
						SET @incetive_value_MEL = (@total_amount_issued_loan_MEL/100) * @setting_incetive_operation_MEL

					END
				ELSE
					BEGIN
						SET @incetive_value_MEL = 0
					END

			-- Calcule INCETIVE FOR VSE PRODUCT
			SELECT 
			   @total_issued_loan_VSE = ISNULL(COUNT(*), 0), 
			   @total_amount_issued_loan_VSE = ISNULL(SUM(Tamkeen_Support.dbo.CULoanPart.IssueAmount), 0)		
			FROM Tamkeen_Support.dbo.CUAccount, 
				 Tamkeen_Support.dbo.CULoan, 
				 Tamkeen_Support.dbo.CULoanpart, 
				 ACM_LOAN_PARTICIPANTS, 
				 ACM_LOAN
			WHERE Tamkeen_Support.dbo.CUAccount.CUAccountID = Tamkeen_Support.dbo.CULoan.CUAccountID
			AND Tamkeen_Support.dbo.CULoan.CULoanID = Tamkeen_Support.dbo.CULoanpart.CULoanID
			AND Tamkeen_Support.dbo.CULoan.Status = 4
			AND Tamkeen_Support.dbo.CULoanpart.issueDate between @firstDate and @date
			AND ACM_LOAN.ID_LOAN_EXTERN = Tamkeen_Support.dbo.CULoan.CULoanID
			AND ACM_LOAN_PARTICIPANTS.ID_ACM_LOAN =  ACM_LOAN.ID_ACM_LOAN
			AND ACM_LOAN_PARTICIPANTS.USERNAME = @username
			AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(
				(SELECT PRODUCT_ID_LIST FROM ACM_PRODUCT_CATEGORY WHERE CODE = 'VSE'), ','))
				IF @incentive_type_VSE = 'FIXED'
					BEGIN
						SET @incetive_value_VSE = @total_issued_loan_VSE * @setting_incetive_operation_VSE
					END
				ELSE IF @incentive_type_VSE = 'PERCENTAGE'
					BEGIN
						SET @incetive_value_VSE = (@total_amount_issued_loan_VSE/100) * @setting_incetive_operation_VSE
					END
				ELSE
					BEGIN
						SET @incetive_value_VSE = 0
					END

			 INSERT INTO ACM_INCENTIVE_RUN_OPERATION VALUES ('Operations Incentives', 'BRANCH_OPERATION', @name, @username, @branch, @total_issued_loan_MEL, @total_amount_issued_loan_MEL, @incentive_type_MEL, @incetive_value_MEL,
			 @total_issued_loan_VSE, @total_amount_issued_loan_VSE, @incentive_type_VSE, @incetive_value_VSE,
			 @incetive_value_MEL + @incetive_value_VSE, DATENAME(month,@date), @date)


	FETCH curseur_incentive_operation INTO  @name, @username, @branch, @account_id
	END
	CLOSE curseur_incentive_operation
	DEALLOCATE curseur_incentive_operation



	