--  "0 0 0/6 * * ?" = run job Every 6 hours of every day.
INSERT INTO [DBO].[ACM_ENVIRONNEMENT]([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION]) 
VALUES ('CRON_EXPRESSION_CANCEL_LOAN', '0 0 0/6 * * ?', 1, '2020-10-28', 0);   