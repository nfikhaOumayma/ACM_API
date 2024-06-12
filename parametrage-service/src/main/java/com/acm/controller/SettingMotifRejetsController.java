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
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingMotifRejetsService;
import com.acm.utils.dtos.SettingMotifRejetsDTO;

/**
 * This class @{link SettingMotifRejetsController} used to control all the SettingMotifRejets
 * requests.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@RestController
@RequestMapping("/setting-motif-rejets")
public class SettingMotifRejetsController {

	/** The SettingMotifRejets service. */
	@Autowired
	private SettingMotifRejetsService settingMotifRejetsService;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/**
	 * Find list SettingMotifRejets by given params.
	 * 
	 * @author HaythemBenizid
	 * @param settingMotifRejetsDTO the setting motif rejets DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingMotifRejetsDTO> find(
			@RequestBody SettingMotifRejetsDTO settingMotifRejetsDTO) {

		settingMotifRejetsDTO.setEnabled(Boolean.TRUE);
		return settingMotifRejetsService.find(settingMotifRejetsDTO);
	}

	/**
	 * Create the SettingMotifRejets.
	 *
	 * @author HaythemBenizid
	 * @param settingMotifRejetsDTO the settingMotifRejets DTO
	 * @return the SettingMotifRejets DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@PostMapping("/create")
	public SettingMotifRejetsDTO create(@RequestBody SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws ResourcesNotFoundException, CodeSettingExistException {

		return settingMotifRejetsService.save(settingMotifRejetsDTO);
	}

	/**
	 * Update the SettingMotifRejets by id.
	 *
	 * @author HaythemBenizid
	 * @param settingMotifRejetsDTO the settingMotifRejets DTO
	 * @return the settingMotifRejets DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingMotifRejetsDTO update(@RequestBody SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws CodeSettingExistException, ResourcesNotFoundException {

		return settingMotifRejetsService.save(settingMotifRejetsDTO.getId(), settingMotifRejetsDTO);
	}

	/**
	 * Find {@link List} of {@link SettingMotifRejetsDTO} list.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingMotifRejetsDTO> findAll() {

		return settingMotifRejetsService.find();
	}

	/**
	 * Find {@link List} of {@link SettingMotifRejetsDTO} list.
	 * 
	 * @author ManelLamloum
	 * @return the list
	 */
	@GetMapping("/find-all-abacus")
	public List<SettingMotifRejetsDTO> findAllMotifRejets() {

		return transversClient.findAllMotifRejet();
	}

	/**
	 * Find enabled and disabled motifs de rejets.
	 *
	 * @author ManelLamloum
	 * @param settingMotifRejetsDTO the setting motif rejets DTO
	 * @return the list
	 */
	@PostMapping("/find-enabled-and-disabled")
	public List<SettingMotifRejetsDTO> findEnabledAndDisabledData(
			@RequestBody SettingMotifRejetsDTO settingMotifRejetsDTO) {

		return settingMotifRejetsService.find(settingMotifRejetsDTO);
	}

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		settingMotifRejetsService.delete(id);
	}
}
