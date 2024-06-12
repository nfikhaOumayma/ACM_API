/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.IncentiveRegistrationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveOperationDTO;
import com.acm.utils.dtos.pagination.IncentiveOperationPaginationDTO;

/**
 * {@link IncentiveOperationService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveOperationService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idIncentiveOperation the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveOperationDTO find(Long idIncentiveOperation) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveOperationDTO the setting level DTO
	 * @return the list
	 */
	List<IncentiveOperationDTO> find(IncentiveOperationDTO incentiveOperationDTO);

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param incentiveOperationPaginationDTO the incentive operation pagination DTO
	 * @return the incentive operation pagination DTO
	 */
	IncentiveOperationPaginationDTO find(
			IncentiveOperationPaginationDTO incentiveOperationPaginationDTO);

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the incentive operation DTO
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	IncentiveOperationDTO save(IncentiveOperationDTO incentiveOperationDTO)
			throws IncentiveRegistrationException;

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param id the id
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the incentive operation DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	IncentiveOperationDTO save(Long id, IncentiveOperationDTO incentiveOperationDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException;

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void delete(Long id) throws ResourcesNotFoundException;

	/**
	 * Save status by given product ID.
	 * 
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the list
	 */
	List<IncentiveOperationDTO> saveStatus(IncentiveOperationDTO incentiveOperationDTO);
}
