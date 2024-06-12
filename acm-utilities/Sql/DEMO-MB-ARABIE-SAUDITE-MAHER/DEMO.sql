ALTER TABLE ACM_WORKFLOW_STEP
ADD AUTOMATIC_STEP BIT NULL;


ALTER TABLE ACM_WORKFLOW_STEP
ADD MIN_SCORE_REJECTED BIGINT NULL;

ALTER TABLE ACM_WORKFLOW_STEP
ADD MAX_SCORE_REJECTED BIGINT NULL;

ALTER TABLE ACM_WORKFLOW_STEP
ADD MIN_SCORE_ACCEPTED BIGINT NULL;

ALTER TABLE ACM_WORKFLOW_STEP
ADD MAX_SCORE_ACCEPTED BIGINT NULL;

ALTER TABLE ACM_WORKFLOW_STEP
ADD REJECTION_CONDITION VARCHAR (256) NULL;


ALTER TABLE ACM_WORKFLOW_STEP
ADD ACCEPTATION_CONDITION  VARCHAR (256) NULL;