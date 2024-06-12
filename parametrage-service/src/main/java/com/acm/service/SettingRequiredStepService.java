/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingRequiredStepDTO;

/**
 * {@link SettingRequiredStepService} class.
 *
 * @author YesserSomai
 * @since 1.0.3
 */
public interface SettingRequiredStepService {

	/**
	 * Find by given params.
	 *
	 * @author YesserSomai
	 * @param settingRequiredStepDTO the setting level DTO
	 * @return the list
	 */
	List<SettingRequiredStepDTO> find(SettingRequiredStepDTO settingRequiredStepDTO);

	/**
	 * The method used for saving the given {@link SettingRequiredStepDTO}.
	 *
	 * @author YesserSomai
	 * @param settingRequiredStepDTO the settingRequiredStep DTO
	 * @return the SettingRequiredStepService DTO
	 */
	SettingRequiredStepDTO save(SettingRequiredStepDTO settingRequiredStepDTO);

	/**
	 * The method used for updating the given {@link SettingRequiredStepDTO} by ID.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @param settingRequiredStepDTO the settingRequiredStep DTO
	 * @return the SettingRequiredStepService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingRequiredStepDTO save(Long id, SettingRequiredStepDTO settingRequiredStepDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find all setting required step.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	List<SettingRequiredStepDTO> find();

}
