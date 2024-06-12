/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CheckLevelProcessException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingLevelProcessDTO;

/**
 * {@link SettingLevelProcessService} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
public interface SettingLevelProcessService {

	/**
	 * Find setting by given params : (product ID & amount are required).
	 *
	 * @author YesserSomai
	 * @param settingLevelProcessDTO the setting level process DTO
	 * @return the list
	 */
	List<SettingLevelProcessDTO> find(SettingLevelProcessDTO settingLevelProcessDTO);

	/**
	 * Find setting by given params : used in setting level process without amount required.
	 *
	 * @author AbdelkarimTurki
	 * @param settingLevelProcessDTO the setting level process DTO
	 * @return the list
	 */
	List<SettingLevelProcessDTO> findSetting(SettingLevelProcessDTO settingLevelProcessDTO);

	/**
	 * The method used for saving the given {@link SettingLevelProcessDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelProcessDTO the settingLevelProcess DTO
	 * @return the SettingLevelProcessService DTO
	 */
	SettingLevelProcessDTO save(SettingLevelProcessDTO settingLevelProcessDTO);

	/**
	 * The method used for updating the given {@link SettingLevelProcessDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingLevelProcessDTO the settingLevelProcess DTO
	 * @return the SettingLevelProcessService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingLevelProcessDTO save(Long id, SettingLevelProcessDTO settingLevelProcessDTO)
			throws ResourcesNotFoundException;

	/**
	 * The method used to Update amount.
	 *
	 * @author AbdelkarimTurki
	 * @param settingLevelProcessDTO the setting level process DTO
	 * @return boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckLevelProcessException the check level process exception
	 */
	Boolean updateAmount(List<SettingLevelProcessDTO> settingLevelProcessDTO)
			throws ResourcesNotFoundException, CheckLevelProcessException;

	/**
	 * Update setting level process.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelProcessDTO the setting level process DTO
	 */
	void updateSettingLevelProcess(SettingLevelProcessDTO settingLevelProcessDTO);

}
