-- Add category column --
  ALTER TABLE ACM_ENVIRONNEMENT ADD CATEGORY VARCHAR (256) NULL

  UPDATE ACM_ENVIRONNEMENT set CATEGORY = 'FUNCTIONAL'

  UPDATE ACM_ENVIRONNEMENT set CATEGORY = 'TECHNICAL' where ID in (  1,3,4,7,40019,20004,10004,10005,10006,10011,10012,10013,10014,10015,10016,10017)