	------------------ FUNCTION
 ALTER FUNCTION [dbo].[ACM_GETRealRepaymentDate](
	@CUAccountID INT,
	@CULoanPartID INT,
	@Period INT,
	@RepaymentDate DATE )
RETURNS DATE
AS
BEGIN

	DECLARE @Principal_TH FLOAT; 
		SET @Principal_TH = (SELECT SUM(LoanRepayment) FROM CULoanSchedule WHERE CULoanPartID=@CULoanPartID and Period <= @Period);
	DECLARE @Interest_TH FLOAT ;
		SET @Interest_TH = (SELECT SUM(InterestRepayment) FROM CULoanSchedule WHERE CULoanPartID=@CULoanPartID and Period <= @Period);
	DECLARE @Fees_TH FLOAT ;
		SET @Fees_TH = (SELECT SUM(FeeRepayment) FROM CULoanSchedule WHERE CULoanPartID=@CULoanPartID and Period <= @Period);

	DECLARE @Principal_date DATE;
		SET @Principal_date = (SELECT ISNULL(MIN(valuedate),'01/01/1900') FROM ACM_PrincipalPaid_Schedule WHERE TotalPrincipalPaid>=@Principal_TH and CUAccountID=@CUAccountID);
	DECLARE @Interest_date DATE ;
		SET @Interest_date = (SELECT ISNULL(MIN(valuedate),'01/01/1900') FROM ACM_InterestPaid_Schedule WHERE TotalInterestPaid>=@Interest_TH and CUAccountID=@CUAccountID+1);
	DECLARE @Fees_date DATE;
		SET @Fees_date = (SELECT ISNULL(MIN(valuedate),'01/01/1900') FROM ACM_FeePaid_Schedule WHERE TotalFeePaid>=@Fees_TH and CUAccountID=@CUAccountID)

	DECLARE @PaymentDate DATE;


	IF ((DATEDIFF(DAY,@Interest_date,@Principal_date) > 0 AND DATEDIFF(DAY,@Fees_date,@Principal_date) > 0)) SET @PaymentDate = @Principal_date
	ELSE IF (DATEDIFF(DAY,@Fees_date,@Interest_date) > 0) SET @PaymentDate =  @Interest_date
	ELSE IF @Fees_TH=0 SET @PaymentDate =  @Principal_date
	ELSE SET @PaymentDate =  @Fees_date
	

	RETURN @PaymentDate;
	

END