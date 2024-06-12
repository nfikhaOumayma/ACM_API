CREATE or ALTER PROCEDURE [dbo].[ACM_PROC_INCENTIVE_REGISTRATION] (@total INT OUTPUT) as 

declare @name  nvarchar(100)
declare @branch nvarchar(100)
declare @username varchar(100)
declare @account_id int
declare @customer_id int
declare @total_customer_MEL int
declare @new_customer_MEL int
declare @renewal_customer_MEL int
declare @total_customer_VSE int
declare @new_customer_VSE int
declare @renewal_customer_VSE int
declare @date datetime
declare @setting_incetive_new_customer_MEL int
declare @setting_incetive_renewal_customer_MEL int
declare @incetive_new_customer_MEL int
declare @incetive_renewal_customer_MEL int
declare @setting_incetive_new_customer_VSE int
declare @setting_incetive_renewal_customer_VSE int
declare @incetive_new_customer_VSE int
declare @incetive_renewal_customer_VSE int


set @date = '2021-10-31T23:59:00'
declare @firstDate datetime
SET @firstDate = (SELECT DATEADD(month, DATEDIFF(month, 0, @date), 0))

set @total = 0
DELETE FROM ACM_INCENTIVE_RUN_REGISTRATION where MONTH = DATENAME(MONTH,@date) and DATENAME(YEAR,RUN_DATE) = DATENAME(YEAR,@date)

	SET @setting_incetive_new_customer_MEL = (SELECT INCENTIVE_VALUE
	FROM ACM_INCENTIVE_REGESTRATION, ACM_INCENTIVE_SETTING_CONSTANT, ACM_PRODUCT_CATEGORY
	WHERE ACM_INCENTIVE_REGESTRATION.CUSTOMER_TYPE_ID = ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CATEGORY = 'INCENTIVE_REGESTRATION_CUSTOMER_TYPE_ID'
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CODE = 'NEW' AND ACM_INCENTIVE_REGESTRATION.PRODUCT_ID = ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY 
		  AND ACM_PRODUCT_CATEGORY.CODE = 'MEL')

	SET @setting_incetive_renewal_customer_MEL = (SELECT INCENTIVE_VALUE
	FROM ACM_INCENTIVE_REGESTRATION, ACM_INCENTIVE_SETTING_CONSTANT, ACM_PRODUCT_CATEGORY
	WHERE ACM_INCENTIVE_REGESTRATION.CUSTOMER_TYPE_ID = ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CATEGORY = 'INCENTIVE_REGESTRATION_CUSTOMER_TYPE_ID'
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CODE = 'RENEWAL' AND ACM_INCENTIVE_REGESTRATION.PRODUCT_ID = ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY 
		  AND ACM_PRODUCT_CATEGORY.CODE = 'MEL')

	SET @setting_incetive_new_customer_VSE = (SELECT INCENTIVE_VALUE
	FROM ACM_INCENTIVE_REGESTRATION, ACM_INCENTIVE_SETTING_CONSTANT, ACM_PRODUCT_CATEGORY
	WHERE ACM_INCENTIVE_REGESTRATION.CUSTOMER_TYPE_ID = ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CATEGORY = 'INCENTIVE_REGESTRATION_CUSTOMER_TYPE_ID'
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CODE = 'NEW' AND ACM_INCENTIVE_REGESTRATION.PRODUCT_ID = ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY 
		  AND ACM_PRODUCT_CATEGORY.CODE = 'VSE')

	SET @setting_incetive_renewal_customer_VSE = (SELECT INCENTIVE_VALUE
	FROM ACM_INCENTIVE_REGESTRATION, ACM_INCENTIVE_SETTING_CONSTANT, ACM_PRODUCT_CATEGORY
	WHERE ACM_INCENTIVE_REGESTRATION.CUSTOMER_TYPE_ID = ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CATEGORY = 'INCENTIVE_REGESTRATION_CUSTOMER_TYPE_ID'
		  AND ACM_INCENTIVE_SETTING_CONSTANT.CODE = 'RENEWAL' AND ACM_INCENTIVE_REGESTRATION.PRODUCT_ID = ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY 
		  AND ACM_PRODUCT_CATEGORY.CODE = 'VSE')

	DECLARE curseur_incentive_repayment CURSOR FOR

		SELECT
			Name + ' ' + SUR_NAME,
			ACM_USERS.USERNAME,
			ACM_USERS.BRANCHE_NAME,
			ACCOUNT_PORTFOLIO_ID
		FROM ACM_USERS, ACM_USERS_GROUPE, ACM_GROUPE

		WHERE ACM_USERS.USERNAME = ACM_USERS_GROUPE.USERNAME
			  AND ACM_GROUPE.ID_ACM_GROUPE = ACM_USERS_GROUPE.ID_ACM_GROUPE
			  AND ACM_GROUPE.CODE = 'LOAN_OFFICER'
		ORDER BY Name + SUR_NAME
	OPEN curseur_incentive_repayment
	FETCH curseur_incentive_repayment INTO @name, @username, @branch, @account_id
	WHILE @@FETCH_STATUS = 0
	BEGIN
		SET @total = @total + 1
		SET @new_customer_MEL = 0
		SET @renewal_customer_MEL = 0
		SET @total_customer_MEL = 0
		DECLARE curseur_customer_MEL CURSOR FOR
			SELECT DISTINCT(ID_ACM_CUSTOMER)
			FROM ACM_LOAN, ACM_LOAN_HISTORIQUE
			WHERE ACM_LOAN.PORTFOLIO_ID = @account_id
				AND ACM_LOAN.ID_ACM_LOAN = ACM_LOAN_HISTORIQUE.ID_ACM_LOAN
				AND ACM_LOAN_HISTORIQUE.ACTION = 'ACTION_UPLOAD_DOCUMENTS'
				AND ACM_LOAN_HISTORIQUE.DATE_UPDATE between @firstDate and @date
				AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(
				(SELECT PRODUCT_ID_LIST FROM ACM_PRODUCT_CATEGORY WHERE CODE = 'MEL'), ','))
		OPEN curseur_customer_MEL
		FETCH curseur_customer_MEL INTO @customer_id
		WHILE @@FETCH_STATUS = 0
		BEGIN
			SET @total_customer_MEL = @total_customer_MEL + 1
			IF (select count(*) from acm_loan where CUSTOMER_ID = @customer_id) = 1
				BEGIN
					SET @new_customer_MEL = @new_customer_MEL + 1
				END
			ELSE 
				BEGIN
					SET @renewal_customer_MEL = @renewal_customer_MEL + 1
				END

		FETCH curseur_customer_MEL INTO  @customer_id
		END
		CLOSE curseur_customer_MEL
		DEALLOCATE curseur_customer_MEL

	SET @incetive_new_customer_MEL = @setting_incetive_new_customer_MEL * @new_customer_MEL
	SET @incetive_renewal_customer_MEL = @setting_incetive_renewal_customer_MEL * @renewal_customer_MEL

	SET @new_customer_VSE = 0
	SET @renewal_customer_VSE = 0
	SET @total_customer_VSE = 0
	DECLARE curseur_customer_VSE CURSOR FOR
			SELECT DISTINCT(ID_ACM_CUSTOMER)
			FROM ACM_LOAN, ACM_LOAN_HISTORIQUE
			WHERE ACM_LOAN.PORTFOLIO_ID = @account_id
				AND ACM_LOAN.ID_ACM_LOAN = ACM_LOAN_HISTORIQUE.ID_ACM_LOAN
				AND ACM_LOAN_HISTORIQUE.ACTION = 'ACTION_UPLOAD_DOCUMENTS'
				AND ACM_LOAN_HISTORIQUE.DATE_UPDATE between @firstDate and @date
				AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(
				(SELECT PRODUCT_ID_LIST FROM ACM_PRODUCT_CATEGORY WHERE CODE = 'VSE'), ','))
		OPEN curseur_customer_VSE
		FETCH curseur_customer_VSE INTO @customer_id
		WHILE @@FETCH_STATUS = 0
		BEGIN
			SET @total_customer_VSE = @total_customer_VSE + 1
			IF (select count(*) from acm_loan where CUSTOMER_ID = @customer_id) = 1
				BEGIN
					SET @new_customer_VSE = @new_customer_VSE + 1
				END
			ELSE 
				BEGIN
					SET @renewal_customer_VSE = @renewal_customer_VSE + 1
				END

		FETCH curseur_customer_VSE INTO  @customer_id
		END
		CLOSE curseur_customer_VSE
		DEALLOCATE curseur_customer_VSE

	SET @incetive_new_customer_VSE = @setting_incetive_new_customer_VSE * @new_customer_VSE
	SET @incetive_renewal_customer_VSE = @setting_incetive_renewal_customer_VSE * @renewal_customer_VSE

	INSERT INTO ACM_INCENTIVE_RUN_REGISTRATION values ('Registration Incentives', @name, @username, @branch, 
	@total_customer_MEL, @new_customer_MEL, @renewal_customer_MEL, @setting_incetive_new_customer_MEL, @setting_incetive_renewal_customer_MEL, @incetive_new_customer_MEL, @incetive_renewal_customer_MEL, 
	@total_customer_VSE, @new_customer_VSE, @renewal_customer_VSE, @setting_incetive_new_customer_VSE, @setting_incetive_renewal_customer_VSE, @incetive_new_customer_VSE, @incetive_renewal_customer_VSE, 
	ISNULL(@incetive_new_customer_MEL, 0) + ISNULL(@incetive_renewal_customer_MEL, 0) 
	+ ISNULL(@incetive_new_customer_VSE, 0) + ISNULL(@incetive_renewal_customer_VSE, 0),
	DATENAME(month,@date) ,GETDATE())

	FETCH curseur_incentive_repayment INTO  @name, @username, @branch, @account_id
	END
	CLOSE curseur_incentive_repayment
	DEALLOCATE curseur_incentive_repayment