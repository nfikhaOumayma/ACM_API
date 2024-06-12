/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveSettingRunDTO;

/**
 * {@link IncentiveSettingRunService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveSettingRunService {

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingRunDTO the incentive setting run DTO
	 * @return the list
	 */
	List<IncentiveSettingRunDTO> find(IncentiveSettingRunDTO incentiveSettingRunDTO);

	/**
	 * The method used for updating status for given {@link IncentiveSettingRunDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingRunDTO the incentiveSettingRun DTO
	 * @return the IncentiveSettingRunService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingRunDTO updateStatus(IncentiveSettingRunDTO incentiveSettingRunDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find by code.
	 * 
	 * @author idridi
	 * @param code the code
	 * @return the incentive setting run DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingRunDTO findByCode(String code) throws ResourcesNotFoundException;

	/**
	 * Update apply discount or branch.
	 * 
	 * @author idridi
	 * @param incentiveSettingRunDTO the incentive setting run DTO
	 * @return the incentive setting run DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveSettingRunDTO updateApplyDiscountOrBranch(
			IncentiveSettingRunDTO incentiveSettingRunDTO) throws ResourcesNotFoundException;

}
