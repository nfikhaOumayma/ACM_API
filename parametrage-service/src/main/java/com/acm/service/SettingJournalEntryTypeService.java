/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.JournalEntryException;
import com.acm.exceptions.type.JournalEntryWorkflowStepException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingJournalEntryTypeDTO;

// TODO: Auto-generated Javadoc
/**
 * The Interface SettingJournalEntryTypeService.
 */
public interface SettingJournalEntryTypeService {

	/**
	 * Find.
	 *
	 * @param settingJournalEntryDTO the setting journal entry DTO
	 * @return the list
	 */
	List<SettingJournalEntryTypeDTO> find(SettingJournalEntryTypeDTO settingJournalEntryDTO);

	/**
	 * Save.
	 *
	 * @param settingJournalEntryDTO the setting journal entry DTO
	 * @return the setting journal entry type DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	SettingJournalEntryTypeDTO save(SettingJournalEntryTypeDTO settingJournalEntryDTO)
			throws CodeSettingExistException;

	// SettingMotifRejetsDTO save(Long id, SettingJournalEntryDTO settingJournalEntryDTO)
	// throws ResourcesNotFoundException, CodeSettingExistException;

	/**
	 * Find.
	 *
	 * @return the list
	 */
	List<SettingJournalEntryTypeDTO> find();

	/**
	 * Delete.
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws JournalEntryWorkflowStepException the journal entry workflow step exception
	 */
	void delete(Long id) throws ResourcesNotFoundException, JournalEntryWorkflowStepException;

	/**
	 * Update.
	 *
	 * @author kouali
	 * @param settingJournalEntryDTO the setting journal entry DTO
	 * @return the setting journal entry type DTO
	 * @throws JournalEntryException the journal entry exception
	 */
	SettingJournalEntryTypeDTO update(SettingJournalEntryTypeDTO settingJournalEntryDTO)
			throws JournalEntryException;

}
