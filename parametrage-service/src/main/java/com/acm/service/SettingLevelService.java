/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingLevelDTO;

/**
 * {@link SettingLevelService} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
public interface SettingLevelService {

	/**
	 * Find.
	 *
	 * @author YesserSomai
	 * @param idSettingLevel the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingLevelDTO find(Long idSettingLevel) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelDTO the setting level DTO
	 * @return the list
	 */
	List<SettingLevelDTO> find(SettingLevelDTO settingLevelDTO);

	/**
	 * Find all.
	 *
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	List<SettingLevelDTO> find();

	/**
	 * The method used for saving the given {@link SettingLevelDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingLevelDTO the settingLevel DTO
	 * @return the SettingLevelService DTO
	 */
	SettingLevelDTO save(SettingLevelDTO settingLevelDTO);

	/**
	 * The method used for updating the given {@link SettingLevelDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingLevelDTO the settingLevel DTO
	 * @return the SettingLevelService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingLevelDTO save(Long id, SettingLevelDTO settingLevelDTO)
			throws ResourcesNotFoundException;

	/**
	 * The method used to Update order.
	 *
	 * @author AbdelkarimTurki
	 * @param settingLevelDTO the setting level DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<SettingLevelDTO> updateOrder(List<SettingLevelDTO> settingLevelDTO)
			throws ResourcesNotFoundException;

}
