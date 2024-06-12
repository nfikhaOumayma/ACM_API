/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveSettingBranchProdLevelDTO;

/**
 * {@link IncentiveSettingBranchProdLevelService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveSettingBranchProdLevelService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idIncentiveSettingBranchProdLevel the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingBranchProdLevelDTO find(Long idIncentiveSettingBranchProdLevel)
			throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingBranchProdLevelDTO the setting level DTO
	 * @return the list
	 */
	List<IncentiveSettingBranchProdLevelDTO> find(
			IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO);

	/**
	 * The method used for saving the given {@link IncentiveSettingBranchProdLevelDTO}.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingBranchProdLevelDTO the incentiveSettingBranchProdLevel DTO
	 * @return the IncentiveSettingBranchProdLevelService DTO
	 */
	IncentiveSettingBranchProdLevelDTO save(
			IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO);

	/**
	 * The method used for updating the given {@link IncentiveSettingBranchProdLevelDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param incentiveSettingBranchProdLevelDTO the incentiveSettingBranchProdLevel DTO
	 * @return the IncentiveSettingBranchProdLevelService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingBranchProdLevelDTO save(Long id,
			IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	void delete(Long id);

}
