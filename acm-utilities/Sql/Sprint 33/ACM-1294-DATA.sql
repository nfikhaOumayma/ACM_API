UPDATE
    [ACM_TAMKEEN].[dbo].[ACM_USERS]
SET
    [ACM_TAMKEEN].[dbo].[ACM_USERS].PORTFOLIO_NAME = [Tamkeen_Integration].[dbo].[CUAccountPortfolio].Code
FROM
    [ACM_TAMKEEN].[dbo].[ACM_USERS] 
INNER JOIN
    [Tamkeen_Integration].[dbo].[CUAccountPortfolio]
ON 
    [ACM_TAMKEEN].[dbo].[ACM_USERS].ACCOUNT_PORTFOLIO_ID = [Tamkeen_Integration].[dbo].[CUAccountPortfolio].[CUAccountPortfolioID]