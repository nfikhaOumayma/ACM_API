-- mezacardStatus
-- Add MEZA_CARD_STATUS column --
-- Possible values : NONE | NEW | SENT | TRUSTED | UNTRUSTED
ALTER TABLE ACM_CUSTOMER ADD MEZA_CARD_STATUS VARCHAR(50);

-- default value = NONE
update ACM_CUSTOMER set MEZA_CARD_STATUS ='NONE';