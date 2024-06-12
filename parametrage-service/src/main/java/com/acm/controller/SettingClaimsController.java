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

import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingClaimsService;
import com.acm.utils.dtos.SettingClaimsDTO;

/**
 * The Class SettingClaimsController.
 */
@RestController
@RequestMapping("/setting-claims")
public class SettingClaimsController {

	/** The SettingMotifRejets service. */
	@Autowired
	private SettingClaimsService settingClaimsService;

	/**
	 * Find.
	 *
	 * @param settingClaimsDTO the setting claims DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingClaimsDTO> find(@RequestBody SettingClaimsDTO settingClaimsDTO) {

		settingClaimsDTO.setEnabled(Boolean.TRUE);
		return settingClaimsService.find(settingClaimsDTO);
	}

	/**
	 * Creates the.
	 *
	 * @param settingClaimsDTO the setting claims DTO
	 * @return the setting claims DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingClaimsDTO create(@RequestBody SettingClaimsDTO settingClaimsDTO)
			throws ResourcesNotFoundException {

		return settingClaimsService.save(settingClaimsDTO);
	}

	/**
	 * Update.
	 *
	 * @param settingClaimsDTO the setting claims DTO
	 * @return the setting claims DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingClaimsDTO update(@RequestBody SettingClaimsDTO settingClaimsDTO)
			throws CodeSettingExistException, ResourcesNotFoundException {

		return settingClaimsService.save(settingClaimsDTO.getId(), settingClaimsDTO);
	}

	/**
	 * Find all.
	 *
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingClaimsDTO> findAll() {

		return settingClaimsService.find();
	}

	/**
	 * Delete.
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		settingClaimsService.delete(id);
	}
}
