/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.CheckLevelProcessException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingLevelProcessService;
import com.acm.utils.dtos.SettingLevelProcessDTO;

/**
 * This class @{link SettingLevelProcessController} used to control all the SettingLevelProcess
 * requests.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
@RestController
@RequestMapping("/setting-level-process")
public class SettingLevelProcessController {

	/** The SettingLevelProcess service. */
	@Autowired
	private SettingLevelProcessService settingLevelProcessService;

	/**
	 * Find setting by given params : (product ID & amount are required).
	 *
	 * @author HaythemBenizid
	 * @param settingLevelProcessDTO the setting level process DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingLevelProcessDTO> find(
			@RequestBody SettingLevelProcessDTO settingLevelProcessDTO) {

		return settingLevelProcessService.find(settingLevelProcessDTO);
	}

	/**
	 * Find setting by given params : used in setting level process without amount required.
	 *
	 * @author AbdelkarimTurki
	 * @param settingLevelProcessDTO the setting level process DTO
	 * @return the list
	 */
	@PostMapping("/find-setting")
	public List<SettingLevelProcessDTO> findSetting(
			@RequestBody SettingLevelProcessDTO settingLevelProcessDTO) {

		return settingLevelProcessService.findSetting(settingLevelProcessDTO);
	}

	/**
	 * Create the SettingLevelProcess.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelProcessDTO the settingLevelProcess DTO
	 * @return the SettingLevelProcess DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingLevelProcessDTO create(@RequestBody SettingLevelProcessDTO settingLevelProcessDTO)
			throws ResourcesNotFoundException {

		return settingLevelProcessService.save(settingLevelProcessDTO);
	}

	/**
	 * Update the SettingLevelProcess by id.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelProcessDTO the settingLevelProcess DTO
	 * @return the settingLevelProcess DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingLevelProcessDTO update(@RequestBody SettingLevelProcessDTO settingLevelProcessDTO)
			throws ResourcesNotFoundException {

		return settingLevelProcessService.save(settingLevelProcessDTO.getId(),
				settingLevelProcessDTO);
	}

	/**
	 * Update amount.
	 *
	 * @author AbdelkarimTurki
	 * @param settingLevelProcessDTOs the setting level process DTO
	 * @return the boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckLevelProcessException the check level process exception
	 */
	@PutMapping("/update-amount")
	public Boolean updateAmount(@RequestBody List<SettingLevelProcessDTO> settingLevelProcessDTOs)
			throws ResourcesNotFoundException, CheckLevelProcessException {

		return settingLevelProcessService.updateAmount(settingLevelProcessDTOs);
	}
}
