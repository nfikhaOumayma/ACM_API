/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.IncentiveRegistrationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.IncentiveRepaymentDTO;
import com.acm.utils.dtos.pagination.IncentiveRepaymentPaginationDTO;

/**
 * {@link IncentiveRepaymentService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface IncentiveRepaymentService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idIncentiveRepayment the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	IncentiveRepaymentDTO find(Long idIncentiveRepayment) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveRepaymentDTO the setting level DTO
	 * @return the list
	 */
	List<IncentiveRepaymentDTO> find(IncentiveRepaymentDTO incentiveRepaymentDTO);

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param incentiveRepaymentPaginationDTO the incentive repayment pagination DTO
	 * @return the incentive repayment pagination DTO
	 */
	IncentiveRepaymentPaginationDTO find(
			IncentiveRepaymentPaginationDTO incentiveRepaymentPaginationDTO);

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param incentiveRepaymentDTO the incentive repayment DTO
	 * @return the incentive repayment DTO
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	IncentiveRepaymentDTO save(IncentiveRepaymentDTO incentiveRepaymentDTO)
			throws IncentiveRegistrationException;

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param id the id
	 * @param incentiveRepaymentDTO the incentive repayment DTO
	 * @return the incentive repayment DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	IncentiveRepaymentDTO save(Long id, IncentiveRepaymentDTO incentiveRepaymentDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException;

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	void delete(Long id);

}
