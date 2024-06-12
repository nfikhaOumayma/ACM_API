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
import com.acm.service.SettingLevelService;
import com.acm.utils.dtos.SettingLevelDTO;

/**
 * This class @{link SettingLevelController} used to control all the SettingLevel requests.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@RestController
@RequestMapping("/setting-level")
public class SettingLevelController {

	/** The SettingLevel service. */
	@Autowired
	private SettingLevelService settingLevelService;

	/**
	 * Find setting by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelDTO the setting level process DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingLevelDTO> find(@RequestBody SettingLevelDTO settingLevelDTO) {

		return settingLevelService.find(settingLevelDTO);
	}

	/**
	 * Find all setting level type.
	 * 
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingLevelDTO> find() {

		return settingLevelService.find();
	}

	/**
	 * Create the SettingLevel.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelDTO the settingLevel DTO
	 * @return the SettingLevel DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingLevelDTO create(@RequestBody SettingLevelDTO settingLevelDTO)
			throws ResourcesNotFoundException {

		return settingLevelService.save(settingLevelDTO);
	}

	/**
	 * Update the SettingLevel by id.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelDTO the settingLevel DTO
	 * @return the settingLevel DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingLevelDTO update(@RequestBody SettingLevelDTO settingLevelDTO)
			throws ResourcesNotFoundException {

		return settingLevelService.save(settingLevelDTO.getId(), settingLevelDTO);
	}

	/**
	 * Update order of the SettingLevel.
	 *
	 * @author AbdelkarimTurki
	 * @param settingLevelDTO the setting level DTO
	 * @return the SettingLevel DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-order")
	public List<SettingLevelDTO> updateOrder(@RequestBody List<SettingLevelDTO> settingLevelDTO)
			throws ResourcesNotFoundException {

		return settingLevelService.updateOrder(settingLevelDTO);
	}
}
