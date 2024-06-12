/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveSettingConstantDTO;

/**
 * {@link IncentiveSettingConstantService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveSettingConstantService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idIncentiveSettingConstant the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingConstantDTO find(Long idIncentiveSettingConstant)
			throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingConstantDTO the incentive setting constant DTO
	 * @return the list
	 */
	List<IncentiveSettingConstantDTO> find(IncentiveSettingConstantDTO incentiveSettingConstantDTO);

	/**
	 * The method used for saving the given {@link IncentiveSettingConstantDTO}.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingConstantDTO the incentiveSettingConstant DTO
	 * @return the IncentiveSettingConstantService DTO
	 */
	IncentiveSettingConstantDTO save(IncentiveSettingConstantDTO incentiveSettingConstantDTO);

	/**
	 * The method used for updating the given {@link IncentiveSettingConstantDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param incentiveSettingConstantDTO the incentiveSettingConstant DTO
	 * @return the IncentiveSettingConstantService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingConstantDTO save(Long id,
			IncentiveSettingConstantDTO incentiveSettingConstantDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find by categories.
	 * 
	 * @author idridi
	 * @param categories the categories
	 * @return the list
	 */
	List<IncentiveSettingConstantDTO> findByCategories(List<String> categories);

}
