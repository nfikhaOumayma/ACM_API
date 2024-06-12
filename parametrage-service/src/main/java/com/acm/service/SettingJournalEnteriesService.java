/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingJournalEnteriesDTO;

/**
 * The Interface SettingJournalEnteriesService.
 */
public interface SettingJournalEnteriesService {

	/**
	 * Save.
	 *
	 * @param settingJournalEnteriesDTO the setting journal enteries DTO
	 * @return the setting journal enteries DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	SettingJournalEnteriesDTO save(SettingJournalEnteriesDTO settingJournalEnteriesDTO)
			throws CodeSettingExistException;

	// SettingMotifRejetsDTO save(Long id, SettingJournalEntryDTO settingJournalEntryDTO)
	// throws ResourcesNotFoundException, CodeSettingExistException;

	/**
	 * Find.
	 *
	 * @return the list
	 */
	List<SettingJournalEnteriesDTO> find();

	/**
	 * Delete.
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void delete(Long id) throws ResourcesNotFoundException;

	/**
	 * Save all.
	 *
	 * @author kouali
	 * @param settingJournalEnteriesDTO the setting journal enteries DTO
	 * @param settingJournalTypeDTO the setting journal type DTO
	 * @return the setting journal enteries DTO
	 */
	List<SettingJournalEnteriesDTO> saveAll(
			List<SettingJournalEnteriesDTO> settingJournalEnteriesDTO, Long settingJournalTypeDTO);

}
