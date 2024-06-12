/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.client.TransversClient;
import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.JournalEntryException;
import com.acm.exceptions.type.JournalEntryWorkflowStepException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingJournalEntryTypeService;
import com.acm.utils.dtos.SettingJournalEntryTypeDTO;

/**
 * The Class SettingJournalEntryTypeController.
 */
@RestController
@RequestMapping("/setting-journal-entry")
public class SettingJournalEntryTypeController {

	/** The setting journal entry service. */
	@Autowired
	private SettingJournalEntryTypeService settingJournalEntryService;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/**
	 * Find.
	 *
	 * @param settingJournalEntryDTO the setting journal entry DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingJournalEntryTypeDTO> find(
			@RequestBody SettingJournalEntryTypeDTO settingJournalEntryDTO) {

		settingJournalEntryDTO.setEnabled(Boolean.TRUE);
		return settingJournalEntryService.find(settingJournalEntryDTO);
	}

	/**
	 * Creates the.
	 *
	 * @param settingJournalEntryDTO the setting journal entry DTO
	 * @return the setting journal entry type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@PostMapping("/create")
	public SettingJournalEntryTypeDTO create(
			@RequestBody SettingJournalEntryTypeDTO settingJournalEntryDTO)
			throws ResourcesNotFoundException, CodeSettingExistException {

		return settingJournalEntryService.save(settingJournalEntryDTO);
	}

	/**
	 * Find all.
	 *
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingJournalEntryTypeDTO> findAll() {

		return settingJournalEntryService.find();
	}

	/**
	 * Delete.
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws JournalEntryWorkflowStepException the journal entry workflow step exception
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) throws ResourcesNotFoundException, JournalEntryWorkflowStepException {

		settingJournalEntryService.delete(id);
	}

	/**
	 * Update.
	 *
	 * @param settingJournalEntryDTO the setting journal entry DTO
	 * @return the setting journal entry type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 * @throws JournalEntryException the journal entry exception
	 */
	@PutMapping("/update")
	public SettingJournalEntryTypeDTO update(
			@RequestBody SettingJournalEntryTypeDTO settingJournalEntryDTO)
			throws ResourcesNotFoundException, CodeSettingExistException, JournalEntryException {

		return settingJournalEntryService.update(settingJournalEntryDTO);
	}
}
