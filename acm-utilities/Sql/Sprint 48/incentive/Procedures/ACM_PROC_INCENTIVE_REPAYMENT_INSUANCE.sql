
CREATE or ALTER PROCEDURE [dbo].[ACM_PROC_INCENTIVE_REPAYMENT_INSUANCE]  (@toltalLoan_issued INT OUTPUT) as 

ALTER DATABASE ACM_PROD_TAMKEEN SET COMPATIBILITY_LEVEL = 130

declare @name  nvarchar(100)
declare @branch nvarchar(100)
declare @productIDList varchar(100)
declare @productDescription varchar(100)
declare @account_id int
declare @active_cutomer int
declare @loan_issued int
declare @balance money
declare @balance_not_paid money
declare @risk money
declare @count_result_setting_incentive money
declare @incetive_value varchar(512)
declare @applay_discount_rule int
declare @discount int
declare @applay_branch_prod_lvl int
declare @branch_prod_lvl int
declare @check_prod_lvl bit
declare @based_on varchar(100)
declare @productCatergoryID int
declare @totale_loan_amount int
declare @username varchar(100)
declare @responsableID varchar(100)
declare @incentive_value_no_discount varchar(100)

----------------------------------------------------------------
SET @toltalLoan_issued = 0

declare @user_type nvarchar(100)
SET @user_type = 'LOAN_OFFICER'
declare @date datetime
set @date = GETDATE()
declare @firstDate datetime

SET @firstDate = (SELECT DATEADD(month, DATEDIFF(month, 0, @date), 0))


DELETE FROM [ACM_INCENTIVE_RUN_REPAYMENT] where MONTH = DATENAME(MONTH,@date) and DATENAME(YEAR,RUN_DATE) = DATENAME(YEAR,@date)

SELECT 
 @applay_discount_rule =  [APPLAY_DISCOUNT_RULE],
 @applay_branch_prod_lvl = [APPLAY_BRANCH_PROD_LEVEL] 
FROM 
 ACM_INCENTIVE_SETTING_RUN WHERE CODE = 'ACM_INCENTIVE_REPAYMENT'

 --- GET ACTIVE CUSTOMER, LOAN ISSUE, RISK FOR EVERY @user_type BY PRODUCT
DECLARE curseur_loan CURSOR FOR
	SELECT
	Name + ' ' + SUR_NAME,
	ACM_USERS.USERNAME,
	ACM_USERS.BRANCHE_NAME,
	ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY,
	ACM_PRODUCT_CATEGORY.PRODUCT_ID_LIST,
	ACM_PRODUCT_CATEGORY.CODE,
	ACCOUNT_PORTFOLIO_ID,
	ACM_USERS.RESPONSABLE_ID,
	(SELECT COUNT(*) FROM ACM_CUSTOMER, ACM_LOAN, Tamkeen_Support.dbo.CULoan
			WHERE ACM_LOAN.ID_ACM_CUSTOMER = ACM_CUSTOMER.ID_ACM_CUSTOMER 
			AND ACCOUNT_PORTFOLIO_ID = ACM_USERS.ACCOUNT_PORTFOLIO_ID
			AND ACM_LOAN.ID_LOAN_EXTERN = Tamkeen_Support.dbo.CULoan.CULoanID
			AND Tamkeen_Support.dbo.CULoan.Status = 4
			AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(ACM_PRODUCT_CATEGORY.PRODUCT_ID_LIST, ','))
			AND Tamkeen_Support.dbo.CULoan.Paid = 0
	),

	(SELECT COUNT(*) FROM Tamkeen_Support.dbo.CUAccount, Tamkeen_Support.dbo.CULoan ,  Tamkeen_Support.dbo.CULoanpart
	 WHERE Tamkeen_Support.dbo.CUAccount.CUAccountPortfolioID = ACM_USERS.ACCOUNT_PORTFOLIO_ID 
			AND Tamkeen_Support.dbo.CUAccount.CUAccountID = Tamkeen_Support.dbo.CULoan.CUAccountID
			AND Tamkeen_Support.dbo.CULoan.CULoanID = Tamkeen_Support.dbo.CULoanpart.CULoanID
			AND Tamkeen_Support.dbo.CULoan.Status = 4
			AND Tamkeen_Support.dbo.CULoanpart.issueDate between @firstDate and @date
			AND Tamkeen_Support.dbo.CULoan.ProductID in (SELECT Value FROM STRING_SPLIT(ACM_PRODUCT_CATEGORY.PRODUCT_ID_LIST, ','))
	),
		
	(SELECT ISNULL(SUM(Tamkeen_Support.dbo.CUAccount.DRAmount - Tamkeen_Support.dbo.CUAccount.CRAmount), 0)
	 FROM  ACM_LOAN, Tamkeen_Support.dbo.CUAccount
	 WHERE Tamkeen_Support.dbo.CUAccount.CUAccountID = ACM_LOAN.ID_ACCOUNT_EXTERN
		AND ACM_LOAN.PORTFOLIO_ID = ACM_USERS.ACCOUNT_PORTFOLIO_ID
		AND Tamkeen_Support.dbo.CUAccount.ParentAccountID is null
		AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(ACM_PRODUCT_CATEGORY.PRODUCT_ID_LIST, ','))
		AND ACM_LOAN.DATE_LAST_UPDATE between @firstDate and @date
	),
	(SELECT ISNULL(SUM(Tamkeen_Support.dbo.CUAccount.DRAmount - Tamkeen_Support.dbo.CUAccount.CRAmount), 0)
	 FROM  Tamkeen_Support.dbo.CUAccount
	 WHERE Tamkeen_Support.dbo.CUAccount.ParentAccountID IS NULL 
	 AND Tamkeen_Support.dbo.CUAccount.CUAccountID IN 
		(SELECT distinct(Tamkeen_Support.dbo.CUAccount.CUAccountID)
		 FROM ACM_CUSTOMER, ACM_LOAN, Tamkeen_Support.dbo.CUAccount, Tamkeen_Support.dbo.CULoanPart, Tamkeen_Support.dbo.CULoanSchedule
		 WHERE ACM_CUSTOMER.ID_ACM_CUSTOMER = ACM_LOAN.ID_ACM_CUSTOMER
				AND Tamkeen_Support.dbo.CUAccount.CUAccountID = ACM_LOAN.ID_ACCOUNT_EXTERN
				AND ACM_LOAN.PORTFOLIO_ID = ACM_USERS.ACCOUNT_PORTFOLIO_ID
				AND Tamkeen_Support.dbo.CULoanPart.CULoanID = ACM_LOAN.ID_LOAN_EXTERN
				AND Tamkeen_Support.dbo.CULoanSchedule.CULoanPartID = Tamkeen_Support.dbo.CULoanPart.CULoanPartID
				AND Tamkeen_Support.dbo.CULoanSchedule.RepaymentDate <  DATEADD(month, -1, @date)
				AND Tamkeen_Support.dbo.CULoanSchedule.AllPaid = 0
				AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(ACM_PRODUCT_CATEGORY.PRODUCT_ID_LIST, ','))
				AND ACM_LOAN.DATE_LAST_UPDATE between @firstDate AND @date
		 )
	),
	(
		SELECT ISNULL(SUM(ACM_LOAN.APPROVEL_AMOUNT), 0) FROM ACM_LOAN, Tamkeen_Support.dbo.CULoan 
		WHERE ACM_LOAN.PORTFOLIO_ID = ACM_USERS.ACCOUNT_PORTFOLIO_ID 
			AND ACM_LOAN.ID_LOAN_EXTERN = Tamkeen_Support.dbo.CULoan .CULoanID
			AND Tamkeen_Support.dbo.CULoan.Status = 4
			AND ACM_LOAN.DATE_LAST_UPDATE between @firstDate and @date
			AND ACM_LOAN.PRODUCT_ID in (SELECT Value FROM STRING_SPLIT(ACM_PRODUCT_CATEGORY.PRODUCT_ID_LIST, ','))
	)




	FROM ACM_USERS, ACM_PRODUCT_CATEGORY, ACM_USERS_GROUPE, ACM_GROUPE

	WHERE ACM_USERS.USERNAME = ACM_USERS_GROUPE.USERNAME
		  AND ACM_GROUPE.ID_ACM_GROUPE = ACM_USERS_GROUPE.ID_ACM_GROUPE
		  AND ACM_GROUPE.CODE = @user_type
		  AND ACM_PRODUCT_CATEGORY.ID_ACM_PRODUCT_CATEGORY in (select DISTINCT(PRODUCT_ID) from ACM_INCENTIVE_REPAYMENT where INCENTIVE_ROLE = @user_type)
		  AND ACM_PRODUCT_CATEGORY.INCENTIVE_REPAYMENT = 1
	ORDER BY Name + SUR_NAME
OPEN curseur_loan
FETCH curseur_loan INTO @name, @username, @branch, @productCatergoryID, @productIDList, @productDescription, @account_id, @responsableID, @active_cutomer, 
						@loan_issued, @balance, @balance_not_paid, @totale_loan_amount
-- TODO RESPONSABLEID
WHILE @@FETCH_STATUS = 0
	BEGIN
		-- calculate risk
		IF @balance_not_paid = 0 or  @balance_not_paid is null
			BEGIN
				SET @balance_not_paid = 0
				SET @risk = 0
			END
		
		IF @balance = 0 or  @balance is null
			BEGIN
				SET @balance = 0
				SET @risk = 0
			END
		
		IF  (@balance_not_paid != 0 and  @balance_not_paid is not null) and (@balance != 0 and  @balance is not null)
			SET @risk = CAST(@balance_not_paid as Float)/CAST(@balance as Float)

		--set @risk = 4
	 
	SET @toltalLoan_issued = @toltalLoan_issued + @loan_issued

	SET @count_result_setting_incentive = (
	SELECT 
			COUNT(*)
		FROM ACM_INCENTIVE_REPAYMENT, 
			 ACM_INCENTIVE_SETTING ACTIVE_CUSTOMER, 
			 ACM_INCENTIVE_SETTING PRODUCTIVITY, 
			 ACM_INCENTIVE_SETTING RISK_LEVEL,
			 ACM_INCENTIVE_SETTING_CONSTANT
		WHERE 
			 ACM_INCENTIVE_REPAYMENT.ACTIVE_CUSTOMER_ID = ACTIVE_CUSTOMER.ID_ACM_INCENTIVE_SETTING
			 AND ACM_INCENTIVE_REPAYMENT.PRODUCTIVITY_ID = PRODUCTIVITY.ID_ACM_INCENTIVE_SETTING
			 AND ACM_INCENTIVE_REPAYMENT.RISK_LEVEL_ID = RISK_LEVEL.ID_ACM_INCENTIVE_SETTING
			 AND ACM_INCENTIVE_REPAYMENT.PRODUCT_ID = @productCatergoryID
			 AND INCENTIVE_ROLE = @user_type 
			 AND ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT = ACM_INCENTIVE_REPAYMENT.BASED_ON_ID
			 
			 AND @active_cutomer >= ACTIVE_CUSTOMER.INCENTIVE_SETTING_FROM and @active_cutomer <= ACTIVE_CUSTOMER.INCENTIVE_SETTING_TO
			 AND @loan_issued >= PRODUCTIVITY.INCENTIVE_SETTING_FROM and @loan_issued <= PRODUCTIVITY.INCENTIVE_SETTING_TO
			 AND @risk >= RISK_LEVEL.INCENTIVE_SETTING_FROM and @risk < RISK_LEVEL.INCENTIVE_SETTING_TO
			 AND ACM_INCENTIVE_REPAYMENT.INCENTIVE_ROLE = 'LOAN_OFFICER'
			 )

	IF @count_result_setting_incentive = 0
	 BEGIN
		INSERT INTO [ACM_INCENTIVE_RUN_REPAYMENT] values ('Issuance_And_Repayment_Incentive',@user_type, @name, '', '', @username, @branch, @productCatergoryID, @productIDList, @productDescription, @active_cutomer, @totale_loan_amount, 
		@loan_issued, DATENAME(month,@date), @balance, @balance_not_paid, @risk, 0, 0, 0, 0, '', 0, 0, 0, 0, @date)
	 END

	ELSE 
	BEGIN
		SELECT 
			@incetive_value = ACM_INCENTIVE_REPAYMENT.INCENTIVE_VALUE,
			@based_on = ACM_INCENTIVE_SETTING_CONSTANT.CODE
				
		FROM ACM_INCENTIVE_REPAYMENT, 
				 ACM_INCENTIVE_SETTING ACTIVE_CUSTOMER, 
				 ACM_INCENTIVE_SETTING PRODUCTIVITY, 
				 ACM_INCENTIVE_SETTING RISK_LEVEL,
				 ACM_INCENTIVE_SETTING_CONSTANT
			
		WHERE 
			ACM_INCENTIVE_REPAYMENT.ACTIVE_CUSTOMER_ID = ACTIVE_CUSTOMER.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.PRODUCTIVITY_ID = PRODUCTIVITY.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.RISK_LEVEL_ID = RISK_LEVEL.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.PRODUCT_ID = @productCatergoryID
			AND ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT = ACM_INCENTIVE_REPAYMENT.BASED_ON_ID
			 
			AND @active_cutomer >= ACTIVE_CUSTOMER.INCENTIVE_SETTING_FROM and @active_cutomer <= ACTIVE_CUSTOMER.INCENTIVE_SETTING_TO
			AND @loan_issued >= PRODUCTIVITY.INCENTIVE_SETTING_FROM and @loan_issued <= PRODUCTIVITY.INCENTIVE_SETTING_TO
			AND @risk >= RISK_LEVEL.INCENTIVE_SETTING_FROM and @risk < RISK_LEVEL.INCENTIVE_SETTING_TO
			AND ACM_INCENTIVE_REPAYMENT.INCENTIVE_ROLE = 'LOAN_OFFICER'
	
		
		Declare @incetive_value_setting int;
		SET @incetive_value_setting = @incetive_value

		IF @based_on = 'ISSUED_LOANS'
			BEGIN
				SET @incetive_value = @incetive_value * @loan_issued
			END
		 ELSE IF @based_on = 'TOTAL_COLLECTED'
			BEGIN
				SET @incentive_value_no_discount = ISNULL(@incetive_value, 0)
				SET @incetive_value = ( 
					SELECT (SUM(Amount)/1000)*@incetive_value FROM Tamkeen_Support.dbo.CUTransaction, Tamkeen_Support.dbo.CUAccount where TransactionTypeID IN (4, 32) 
								AND ReversalReceiptNo = 0
								AND ValueDate BETWEEN DATEADD(month, -1, @date) AND @date
								AND Tamkeen_Support.dbo.CUTransaction.CUAccountID = Tamkeen_Support.dbo.CUAccount.CUAccountID 
								AND Tamkeen_Support.dbo.CUAccount.CUAccountPortfolioID	= @account_id
							)
			END

		Declare @RepaymentPaid int;
		Declare @RepaymentNotPaid int;
		Declare @RepaymentRate int;

		SET @RepaymentPaid= (
			SELECT 
				ISNULL(SUM(Amount), 0) 
			FROM 
				Tamkeen_Support.dbo.CUTransaction, Tamkeen_Support.dbo.CUAccount where TransactionTypeID IN (4, 32) 
				AND ReversalReceiptNo = 0
				AND ValueDate BETWEEN DATEADD(month, -1, @date) AND @date
				AND Tamkeen_Support.dbo.CUTransaction.CUAccountID = Tamkeen_Support.dbo.CUAccount.CUAccountID 
				AND Tamkeen_Support.dbo.CUAccount.CUAccountPortfolioID	= @account_id
		)

		SET @RepaymentNotPaid= (
			SELECT 
					ISNULL(SUM(TotalRepayment), 0) 
			FROM Tamkeen_Support.dbo.CULoanSchedule, Tamkeen_Support.dbo.CULoan, Tamkeen_Support.dbo.CULoanPart, Tamkeen_Support.dbo.CUAccount
			WHERE RepaymentDate between @firstDate and @date
			AND Tamkeen_Support.dbo.CULoanSchedule.CULoanPartID = Tamkeen_Support.dbo.CULoanPart.CULoanPartID
			AND Tamkeen_Support.dbo.CULoan.CULoanID = Tamkeen_Support.dbo.CULoanPart.CULoanID
			AND Tamkeen_Support.dbo.CULoan.CUAccountID = Tamkeen_Support.dbo.CUAccount.CUAccountID
			AND Tamkeen_Support.dbo.CUAccount.CUAccountPortfolioID = @account_id
			AND Tamkeen_Support.dbo.CULoanSchedule.AllPaid = 0
		)

		IF @RepaymentPaid = 0
			BEGIN
				SET @RepaymentRate = 100
			END
		
		IF @RepaymentNotPaid = 0 
			BEGIN
				SET @RepaymentRate = 100
			END
		
		IF  @RepaymentPaid != 0 and @RepaymentNotPaid != 0
			SET @RepaymentRate = 100 - CAST(@RepaymentNotPaid as Float)/CAST(@RepaymentPaid as Float)
		
		SET @incentive_value_no_discount = ISNULL(@incetive_value,0)

		-- CHECK DISCOUNT
		IF @applay_discount_rule != 0 and @applay_discount_rule is not null and @check_prod_lvl=1
			BEGIN
				SET @discount =  (Select DISCOUNT FROM ACM_INCENTIVE_SETTING where CATEGORY = 'DISCOUNT_FROM_TOTAL' and
				CAST(INCENTIVE_SETTING_FROM as INT) >=  @RepaymentPaid and CAST(INCENTIVE_SETTING_TO as INT) < @RepaymentPaid)
				 
				SET @incetive_value = (@incetive_value_setting* @discount)/100

			END
		
		
		BEGIN
		INSERT INTO [ACM_INCENTIVE_RUN_REPAYMENT] values ('Issuance_And_Repayment_Incentive',@user_type, @name, '', '', @username, @branch, @productCatergoryID, @productIDList, @productDescription, @active_cutomer, 0, 
		@loan_issued, DATENAME(month,@date), @balance, @balance_not_paid, @risk, @RepaymentPaid, @RepaymentNotPaid, @RepaymentRate, @incetive_value_setting, @based_on, 
		0, @applay_discount_rule, @incentive_value_no_discount, @incetive_value, @date)
		END
	END

	--------------------------------- START SUPERVISOR
		SET @incetive_value = 0
		SET @based_on = ''
		SELECT 
			@incetive_value = ACM_INCENTIVE_REPAYMENT.INCENTIVE_VALUE,
			@based_on = ACM_INCENTIVE_SETTING_CONSTANT.CODE
				
		FROM ACM_INCENTIVE_REPAYMENT, 
				 ACM_INCENTIVE_SETTING ACTIVE_CUSTOMER, 
				 ACM_INCENTIVE_SETTING PRODUCTIVITY, 
				 ACM_INCENTIVE_SETTING RISK_LEVEL,
				 ACM_INCENTIVE_SETTING_CONSTANT
			
		WHERE 
			ACM_INCENTIVE_REPAYMENT.ACTIVE_CUSTOMER_ID = ACTIVE_CUSTOMER.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.PRODUCTIVITY_ID = PRODUCTIVITY.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.RISK_LEVEL_ID = RISK_LEVEL.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.PRODUCT_ID = @productCatergoryID
			AND ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT = ACM_INCENTIVE_REPAYMENT.BASED_ON_ID
			 
			AND @active_cutomer >= ACTIVE_CUSTOMER.INCENTIVE_SETTING_FROM and @active_cutomer <= ACTIVE_CUSTOMER.INCENTIVE_SETTING_TO
			AND @loan_issued >= PRODUCTIVITY.INCENTIVE_SETTING_FROM and @loan_issued <= PRODUCTIVITY.INCENTIVE_SETTING_TO
			AND @risk >= RISK_LEVEL.INCENTIVE_SETTING_FROM and @risk < RISK_LEVEL.INCENTIVE_SETTING_TO
			AND ACM_INCENTIVE_REPAYMENT.INCENTIVE_ROLE = 'SUPERVISOR'


		Declare @supervisorName varchar(100)
		Declare @supervisorResposableID varchar(100)

		select 
			 @supervisorName =  Name + ' ' + SUR_NAME,
			 @supervisorResposableID = RESPONSABLE_ID
		from ACM_USERS where USERNAME = @responsableID

		SET @incetive_value_setting = @incetive_value

		IF @based_on = 'ISSUED_LOANS'
			BEGIN
				SET @incetive_value = @incetive_value * @loan_issued
			END
		 ELSE IF @based_on = 'TOTAL_COLLECTED'
			-- Collection paid
			BEGIN
				SET @incetive_value = ( 
					SELECT (SUM(Amount)/1000)*@incetive_value FROM Tamkeen_Support.dbo.CUTransaction, Tamkeen_Support.dbo.CUAccount where TransactionTypeID IN (4, 32) 
								AND ReversalReceiptNo = 0
								AND ValueDate BETWEEN DATEADD(month, -1, @date) AND @date
								AND Tamkeen_Support.dbo.CUTransaction.CUAccountID = Tamkeen_Support.dbo.CUAccount.CUAccountID 
								AND Tamkeen_Support.dbo.CUAccount.CUAccountPortfolioID	= @account_id
							)
			END
		

		INSERT INTO [ACM_INCENTIVE_RUN_REPAYMENT] values ('Issuance_And_Repayment_Incentive','SUPERVISOR', @name, @supervisorName, '', @username, @branch, @productCatergoryID, @productIDList, @productDescription, @active_cutomer, 0, 
		@loan_issued, DATENAME(month,@date), @balance, @balance_not_paid, @risk, @RepaymentPaid, @RepaymentNotPaid, @RepaymentRate, @incetive_value_setting, @based_on, 
		0, @applay_discount_rule, null, @incetive_value, @date)

		--------------------------------- END SUPERVISOR
		--------------------------------- START BRANCH_MANAGER
		SET @incetive_value = 0
		SET @based_on = ''
		SELECT 
			@incetive_value = ACM_INCENTIVE_REPAYMENT.INCENTIVE_VALUE,
			@based_on = ACM_INCENTIVE_SETTING_CONSTANT.CODE
				
		FROM ACM_INCENTIVE_REPAYMENT, 
				 ACM_INCENTIVE_SETTING ACTIVE_CUSTOMER, 
				 ACM_INCENTIVE_SETTING PRODUCTIVITY, 
				 ACM_INCENTIVE_SETTING RISK_LEVEL,
				 ACM_INCENTIVE_SETTING_CONSTANT
			
		WHERE 
			ACM_INCENTIVE_REPAYMENT.ACTIVE_CUSTOMER_ID = ACTIVE_CUSTOMER.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.PRODUCTIVITY_ID = PRODUCTIVITY.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.RISK_LEVEL_ID = RISK_LEVEL.ID_ACM_INCENTIVE_SETTING
			AND ACM_INCENTIVE_REPAYMENT.PRODUCT_ID = @productCatergoryID
			AND ACM_INCENTIVE_SETTING_CONSTANT.ID_ACM_INCENTIVE_SETTING_CONSTANT = ACM_INCENTIVE_REPAYMENT.BASED_ON_ID
			 
			AND @active_cutomer >= ACTIVE_CUSTOMER.INCENTIVE_SETTING_FROM and @active_cutomer <= ACTIVE_CUSTOMER.INCENTIVE_SETTING_TO
			AND @loan_issued >= PRODUCTIVITY.INCENTIVE_SETTING_FROM and @loan_issued <= PRODUCTIVITY.INCENTIVE_SETTING_TO
			AND @risk >= RISK_LEVEL.INCENTIVE_SETTING_FROM and @risk < RISK_LEVEL.INCENTIVE_SETTING_TO
			AND ACM_INCENTIVE_REPAYMENT.INCENTIVE_ROLE = 'BRANCH_MANAGER'

		Declare @branchManagerName varchar(100)
		Declare @branchManagerResposableID varchar(100)

		select 
			 @branchManagerName =  Name + ' ' + SUR_NAME,
			 @branchManagerResposableID = RESPONSABLE_ID
		from ACM_USERS where USERNAME = @supervisorResposableID


		SET @incetive_value_setting = @incetive_value

		IF @based_on = 'ISSUED_LOANS'
			BEGIN
				SET @incetive_value = @incetive_value * @loan_issued
			END
		 ELSE IF @based_on = 'TOTAL_COLLECTED'
			-- Collection paid
			BEGIN
				SET @incetive_value = ( 
					SELECT (SUM(Amount)/1000)*@incetive_value FROM Tamkeen_Support.dbo.CUTransaction, Tamkeen_Support.dbo.CUAccount where TransactionTypeID IN (4, 32) 
								AND ReversalReceiptNo = 0
								AND ValueDate BETWEEN DATEADD(month, -1, @date) AND @date
								AND Tamkeen_Support.dbo.CUTransaction.CUAccountID = Tamkeen_Support.dbo.CUAccount.CUAccountID 
								AND Tamkeen_Support.dbo.CUAccount.CUAccountPortfolioID	= @account_id
							)
			END
		

		INSERT INTO [ACM_INCENTIVE_RUN_REPAYMENT] values ('Issuance_And_Repayment_Incentive','BRANCH_MANAGER', @name, @supervisorName, @branchManagerName, @username, @branch, @productCatergoryID, @productIDList, @productDescription, @active_cutomer, 0, 
		@loan_issued, DATENAME(month,@date), @balance, @balance_not_paid, @risk, @RepaymentPaid, @RepaymentNotPaid, @RepaymentRate, @incetive_value_setting, @based_on, 
		0, @applay_discount_rule, null, @incetive_value, @date)

FETCH curseur_loan INTO @name, @username, @branch, @productCatergoryID, @productIDList, @productDescription, @account_id, @responsableID, @active_cutomer, 
						@loan_issued, @balance, @balance_not_paid, @totale_loan_amount
END
CLOSE curseur_loan
DEALLOCATE curseur_loan
