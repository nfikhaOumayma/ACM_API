/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.IncentiveRegistrationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveRegistrationDTO;
import com.acm.utils.dtos.pagination.IncentiveRegistrationPaginationDTO;

/**
 * {@link IncentiveRegistrationService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveRegistrationService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idIncentiveRegistration the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveRegistrationDTO find(Long idIncentiveRegistration) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveRegistrationDTO the setting level DTO
	 * @return the list
	 */
	List<IncentiveRegistrationDTO> find(IncentiveRegistrationDTO incentiveRegistrationDTO);

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationPaginationDTO the incentive registration pagination DTO
	 * @return the incentive registration pagination DTO
	 */
	IncentiveRegistrationPaginationDTO find(
			IncentiveRegistrationPaginationDTO incentiveRegistrationPaginationDTO);

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the incentive registration DTO
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	IncentiveRegistrationDTO save(IncentiveRegistrationDTO incentiveRegistrationDTO)
			throws IncentiveRegistrationException;

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param id the id
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the incentive registration DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	IncentiveRegistrationDTO save(Long id, IncentiveRegistrationDTO incentiveRegistrationDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException;

	/**
	 * Delete.
	 *
	 * @author idridi
	 * @param id the id
	 */
	void delete(Long id);

	/**
	 * Save status.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the list
	 */
	List<IncentiveRegistrationDTO> saveStatus(IncentiveRegistrationDTO incentiveRegistrationDTO);

}
