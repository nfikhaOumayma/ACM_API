CREATE VIEW [dbo].[ACM_Customer_Stat] AS
SELECT
CUAccount.CustomerID,
ISNULL((SELECT Datediff(DAY,MIN(RepaymentDate),Getdate()) FROM CULoanSchedule WHERE AllPaid=0 AND RepaymentDate<Getdate() AND CULoanSchedule.CULoanPartID=CULoanPart.CULoanPartID),0) ArriearsDay,
ISNULL((SELECT COUNT (*)
FROM CULoanSchedule WHERE AllPaid=0 AND RepaymentDate<Getdate() AND CULoanSchedule.CULoanPartID=CULoanPart.CULoanPartID),0) ArrearsInstallments
FROM CUAccount
INNER JOIN CULoan ON CUAccount.CUAccountID=CULoan.CUAccountID
INNER JOIN CULoanPart ON CULoanPart.CULoanID=CULoan.CULoanID
WHERE ParentAccountID IS NULL