-- Branche localisation ---
ALTER TABLE IB_SETTING_DATA
    ADD IB_BRANCHE_LOCALISATION VARCHAR (256) NULL;

--  Phone number --
ALTER TABLE IB_SETTING_DATA
    ADD PHONE_NUMBER VARCHAR (256) NULL;

-- add location for jeddah --
UPDATE IB_SETTING_DATA SET IB_BRANCHE_LOCALISATION = 'https://goo.gl/maps/6c3SZnAjH8gUfiT99' where ID = 2;
-- add location for AL Madinah Al Munawwarah --
  UPDATE IB_SETTING_DATA SET IB_BRANCHE_LOCALISATION = 'https://goo.gl/maps/Et6yzRVDRX1hCYHG8' where ID = 3;
  -- add location for Tabuk --
  UPDATE IB_SETTING_DATA SET IB_BRANCHE_LOCALISATION = 'https://goo.gl/maps/52CrigcxFNSdPbD8A' where ID = 4;
  -- add location for Riyadh --
  UPDATE IB_SETTING_DATA SET IB_BRANCHE_LOCALISATION = 'https://goo.gl/maps/ZMjAijGBLnTgiKG8A' where ID = 5;
  -- add location for Dammam --
  UPDATE IB_SETTING_DATA SET IB_BRANCHE_LOCALISATION = 'https://goo.gl/maps/BRsih56ke6FGT61o9' where ID = 6;

  -- add PHONE_NUMBER for jeddah --
  UPDATE IB_SETTING_DATA SET PHONE_NUMBER = '+966126285378' where ID = 2;
  -- add PHONE_NUMBER for AL Madinah Al Munawwarah --
  UPDATE IB_SETTING_DATA SET PHONE_NUMBER = '+966148366100' where ID = 3;
    -- add PHONE_NUMBER for Tabuk --
  UPDATE IB_SETTING_DATA SET PHONE_NUMBER = '+966144441166' where ID = 4;
    -- add PHONE_NUMBER for Riyadh --
  UPDATE IB_SETTING_DATA SET PHONE_NUMBER = '+966114443156' where ID = 5;
   -- add PHONE_NUMBER for Dammam --
  UPDATE IB_SETTING_DATA SET PHONE_NUMBER = '+966138469909' where ID = 6;

  -- add instagram link --
  INSERT INTO [ACM].[dbo].[ACM_ENVIRONNEMENT]
  ([ACM_ENVIRONNEMENT_KEY], [ACM_ENVIRONNEMENT_VALUE], [ACM_ENABLED], [DATE_INSERTION], [ACM_VERSION]) 
VALUES ('INSTAGRAM_LINK', 'https://www.instagram.com/brjmf/', 1, '2021-01-18', 0);