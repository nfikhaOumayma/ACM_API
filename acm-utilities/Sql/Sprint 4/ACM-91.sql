----- TABLE : ACM_REPORT_VISIT
ALTER TABLE ACM_REPORT_VISIT
DROP COLUMN TITLE;

ALTER TABLE ACM_REPORT_VISIT
ADD PLANNED_VISIT DATE,
	
ALTER TABLE ACM_REPORT_VISIT
ADD COMMENT VARCHAR (256) NOT NULL,