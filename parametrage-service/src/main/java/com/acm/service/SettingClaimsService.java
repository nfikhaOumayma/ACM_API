/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingClaimsDTO;

/**
 * The Interface SettingClaimsService.
 */
public interface SettingClaimsService {

	/**
	 * Find.
	 *
	 * @param settingClaimsDTO the setting claims DTO
	 * @return the list
	 */
	List<SettingClaimsDTO> find(SettingClaimsDTO settingClaimsDTO);

	/**
	 * Save.
	 *
	 * @param settingClaimsDTO the setting claims DTO
	 * @return the setting claims DTO
	 */
	SettingClaimsDTO save(SettingClaimsDTO settingClaimsDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param settingClaimsDTO the setting claims DTO
	 * @return the setting claims DTO
	 */
	SettingClaimsDTO save(Long id, SettingClaimsDTO settingClaimsDTO);

	/**
	 * Find.
	 *
	 * @return the list
	 */
	List<SettingClaimsDTO> find();

	/**
	 * Delete.
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void delete(Long id) throws ResourcesNotFoundException;

}
