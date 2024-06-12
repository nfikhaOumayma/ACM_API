/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CustomerDecisionDTO;

/**
 * {@link CustomerDecisionService} interface.
 *
 * @author YesserSomai
 * @since 0.5.0
 */
public interface CustomerDecisionService {

	/**
	 * Find {@link CustomerDecisionDTO} by given ID.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @return the CustomerDesicionService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDecisionDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link CustomerDecisionDTO} by given params.
	 *
	 * @author YesserSomai
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the list
	 */
	List<CustomerDecisionDTO> find(CustomerDecisionDTO customerDesicionDTO);

	/**
	 * The method used for saving the given {@link CustomerDecisionDTO}.
	 *
	 * @author YesserSomai
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the CustomerDesicionService DTO
	 */
	CustomerDecisionDTO save(CustomerDecisionDTO customerDesicionDTO);

	/**
	 * Save.
	 *
	 * @param customerDesicionDTO the customer desicion DTO
	 * @param InsertBy the insert by
	 * @return the customer decision DTO
	 */
	CustomerDecisionDTO save(CustomerDecisionDTO customerDesicionDTO, String InsertBy);

	/**
	 * The method used for updating the given {@link CustomerDecisionDTO} by ID.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the CustomerDesicionService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDecisionDTO save(Long id, CustomerDecisionDTO customerDesicionDTO)
			throws ResourcesNotFoundException;

	/**
	 * Validate : process la request selon l'action () .
	 *
	 * @author HaythemBenizid
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the customer desicion DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerDecisionDTO validate(CustomerDecisionDTO customerDesicionDTO)
			throws ResourcesNotFoundException;
}
