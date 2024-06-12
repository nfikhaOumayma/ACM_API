-------------
----- FUNCTION
CREATE FUNCTION ACM_GETRealRepaymentDate
(
	@CUAccountID INT,
	@CULoanPartID INT,
	@Period INT,
	@RepaymentDate DATE
)
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
		SET @Principal_date = (SELECT ISNULL(MIN(valuedate),@RepaymentDate) FROM ACM_PrincipalPaid_Schedule WHERE TotalPrincipalPaid>=@Principal_TH and CUAccountID=@CUAccountID);
	DECLARE @Interest_date DATE ;
		SET @Interest_date = (SELECT ISNULL(MIN(valuedate),@RepaymentDate) FROM ACM_InterestPaid_Schedule WHERE TotalInterestPaid>=@Interest_TH and CUAccountID=@CUAccountID+1);
	DECLARE @Fees_date DATE;
		SET @Fees_date = (SELECT ISNULL(MIN(valuedate),@RepaymentDate) FROM ACM_FeePaid_Schedule WHERE TotalFeePaid>=@Fees_TH and CUAccountID=@CUAccountID)

	DECLARE @PaymentDate DATE;


	IF ((DATEDIFF(DAY,@Interest_date,@Principal_date) > 0 AND DATEDIFF(DAY,@Fees_date,@Principal_date) > 0)) SET @PaymentDate = @Principal_date
	ELSE IF (DATEDIFF(DAY,@Fees_date,@Interest_date) > 0) SET @PaymentDate =  @Interest_date
	ELSE SET @PaymentDate =  @Fees_date
	

	RETURN @PaymentDate;
	

END
-------------
----- VUE
CREATE VIEW ACM_PrincipalPaid_Schedule AS
SELECT CUAccountID,ValueDate,Amount,
(SELECT sum(t2.Amount) from CUTransaction t2 where t2.TransactionTypeID in (4,4194304) and t1.ReversalReceiptNo = 0 
	and t1.CUAccountID=t2.CUAccountID and t2.CUTransactionID<=t1.CUTransactionID) TotalPrincipalPaid
FROM CUTransaction t1
WHERE t1.TransactionTypeID in (4,4194304) and t1.ReversalReceiptNo = 0;

CREATE VIEW ACM_InterestPaid_Schedule AS
SELECT CUAccountID,ValueDate,Amount,
(SELECT sum(t2.Amount) from CUTransaction t2 where t2.TransactionTypeID=32 and t1.ReversalReceiptNo = 0 
	and t1.CUAccountID=t2.CUAccountID and t2.CUTransactionID<=t1.CUTransactionID) TotalInterestPaid
FROM CUTransaction t1
WHERE t1.TransactionTypeID=32 and t1.ReversalReceiptNo = 0; 


CREATE VIEW ACM_FeePaid_Schedule AS
SELECT CUAccountID,ValueDate,Amount,
(SELECT sum(t2.Amount) from CUTransaction t2 where t2.TransactionTypeID=128 and t1.ReversalReceiptNo = 0 
	and t1.CUAccountID=t2.CUAccountID and t2.CUTransactionID<=t1.CUTransactionID and t2.Balance=0) TotalFeePaid
FROM CUTransaction t1
WHERE t1.TransactionTypeID=128 and t1.ReversalReceiptNo = 0 and t1.Balance=0;


CREATE VIEW ACM_CUSTOMER_KPI_Arrears AS 
SELECT 
	CUAccount.CustomerID,
	AVG(CAST(DATEDIFF(DAY,CULoanSchedule.RepaymentDate,dbo.ACM_GETRealRepaymentDate(CUAccount.CUAccountID,CULoanPart.CULoanPartID,CULoanSchedule.Period,CULoanSchedule.RepaymentDate)) AS DECIMAL(12,2))) ArrearDay,
	SUM(CASE WHEN DATEDIFF(DAY,CULoanSchedule.RepaymentDate,dbo.ACM_GETRealRepaymentDate(CUAccount.CUAccountID,CULoanPart.CULoanPartID,CULoanSchedule.Period,CULoanSchedule.RepaymentDate)) > 0 THEN 1 ELSE 0 END) ArrearSchedule
FROM CUAccount
INNER JOIN CULoan ON CUAccount.CUAccountID=CULoan.CUAccountID
INNER JOIN CULoanPart ON CULoanPart.CULoanID=CULoan.CULoanID
INNER JOIN CULoanSchedule ON CULoanSchedule.CULoanPartID=CULoanPart.CULoanPartID
WHERE CULoan.Status in (4,8,16,32) and CULoanSchedule.RepaymentDate <= GETDATE()
and CUAccount.CUAccountID in (select CUAccountID from CUAccount where ParentAccountID is null )
Group by CUAccount.CustomerID;