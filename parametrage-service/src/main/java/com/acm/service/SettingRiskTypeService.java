/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingTypeRiskDTO;

/**
 * The Interface SettingRiskTypeService.
 */
public interface SettingRiskTypeService {

	/**
	 * Save.
	 *
	 * @param settingRiskTypeDTO the setting risk type DTO
	 * @return the setting type risk DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingTypeRiskDTO save(SettingTypeRiskDTO settingRiskTypeDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find.
	 *
	 * @return the list
	 */
	List<SettingTypeRiskDTO> find();

	/**
	 * Update.
	 *
	 * @param settingTypeRiskDTO the setting type risk DTO
	 * @return the setting type risk DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingTypeRiskDTO update(SettingTypeRiskDTO settingTypeRiskDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete.
	 *
	 * @param idRiskType the id risk type
	 */
	void delete(Long idRiskType);

}
