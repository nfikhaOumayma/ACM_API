/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveSettingDTO;

/**
 * {@link IncentiveSettingService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveSettingService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idIncentiveSetting the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingDTO find(Long idIncentiveSetting) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingDTO the setting level DTO
	 * @return the list
	 */
	List<IncentiveSettingDTO> find(IncentiveSettingDTO incentiveSettingDTO);

	/**
	 * The method used for saving the given {@link IncentiveSettingDTO}.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingDTO the incentiveSetting DTO
	 * @return the IncentiveSettingService DTO
	 */
	IncentiveSettingDTO save(IncentiveSettingDTO incentiveSettingDTO);

	/**
	 * The method used for updating the given {@link IncentiveSettingDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param incentiveSettingDTO the incentiveSetting DTO
	 * @return the IncentiveSettingService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingDTO save(Long id, IncentiveSettingDTO incentiveSettingDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	void delete(Long id);

}
