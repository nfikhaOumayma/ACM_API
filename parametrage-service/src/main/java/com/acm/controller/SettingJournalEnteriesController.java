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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.client.TransversClient;
import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingJournalEnteriesService;
import com.acm.utils.dtos.SettingJournalEnteriesDTO;

/**
 * The Class SettingJournalEnteriesController.
 */
@RestController
@RequestMapping("/setting-journal-enteries")
public class SettingJournalEnteriesController {

	/** The setting journal enteries service. */
	@Autowired
	private SettingJournalEnteriesService settingJournalEnteriesService;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/**
	 * Creates the.
	 *
	 * @param settingJournalEnteriesDTO the setting journal enteries DTO
	 * @return the setting journal enteries DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@PostMapping("/create")
	public SettingJournalEnteriesDTO create(
			@RequestBody SettingJournalEnteriesDTO settingJournalEnteriesDTO)
			throws ResourcesNotFoundException, CodeSettingExistException {

		return settingJournalEnteriesService.save(settingJournalEnteriesDTO);
	}

	/**
	 * Creates the journal enteries.
	 *
	 * @param settingJournalEnteriesDTO the setting journal enteries DTO
	 * @param settingJournalId the setting journal id
	 * @return the setting journal enteries DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@PostMapping("/save-all/{settingJournalId}")
	public List<SettingJournalEnteriesDTO> createJournalEnteries(
			@RequestBody List<SettingJournalEnteriesDTO> settingJournalEnteriesDTO,
			@PathVariable("settingJournalId") Long settingJournalId)
			throws ResourcesNotFoundException, CodeSettingExistException {

		return settingJournalEnteriesService.saveAll(settingJournalEnteriesDTO, settingJournalId);
	}

	/**
	 * Find all.
	 *
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingJournalEnteriesDTO> findAll() {

		return settingJournalEnteriesService.find();
	}

	/**
	 * Delete.
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		settingJournalEnteriesService.delete(id);
	}

}
