/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SettingJournalEnteries;
import com.acm.utils.models.SettingJournalEntryType;

/**
 * The Interface SettingJournalEnteriesRepository.
 */
@Repository
public interface SettingJournalEnteriesRepository
		extends JpaRepository<SettingJournalEnteries, Long> {

	/**
	 * Delete by setting journal entry type.
	 *
	 * @author kouali
	 * @param settingJournalEntryType the setting journal entry type
	 */
	@Transactional
	void deleteBySettingJournalEntryType(SettingJournalEntryType settingJournalEntryType);

}
