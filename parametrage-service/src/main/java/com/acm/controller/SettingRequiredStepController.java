/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingRequiredStepService;
import com.acm.utils.dtos.SettingRequiredStepDTO;

/**
 * This class @{link SettingRequiredStepController} used to control all the SettingRequiredStep
 * requests.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
@RestController
@RequestMapping("/setting-required-step")
public class SettingRequiredStepController {

	/** The SettingRequiredStep service. */
	@Autowired
	private SettingRequiredStepService settingRequiredStepService;

	/**
	 * Find list SettingRequiredStep by given params.
	 *
	 * @param settingRequiredStepDTO the setting required step DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingRequiredStepDTO> find(
			@RequestBody SettingRequiredStepDTO settingRequiredStepDTO) {

		return settingRequiredStepService.find(settingRequiredStepDTO);
	}

	/**
	 * Create the SettingRequiredStep.
	 *
	 * @author YesserSomai
	 * @param settingRequiredStepDTO the settingRequiredStep DTO
	 * @return the SettingRequiredStep DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingRequiredStepDTO create(@RequestBody SettingRequiredStepDTO settingRequiredStepDTO)
			throws ResourcesNotFoundException {

		return settingRequiredStepService.save(settingRequiredStepDTO);
	}

	/**
	 * Update the SettingRequiredStep by id.
	 *
	 * @author YesserSomai
	 * @param settingRequiredStepDTO the settingRequiredStep DTO
	 * @return the settingRequiredStep DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingRequiredStepDTO update(@RequestBody SettingRequiredStepDTO settingRequiredStepDTO)
			throws ResourcesNotFoundException {

		return settingRequiredStepService.save(settingRequiredStepDTO.getId(),
				settingRequiredStepDTO);
	}

	/**
	 * Find {@link List} of {@link SettingRequiredStepDTO} list.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingRequiredStepDTO> findSettingRequiredStep() {

		return settingRequiredStepService.find();
	}
}
