/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import org.dozer.MappingException;

import com.acm.exceptions.type.CustomerContactException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CustomerContactDTO;

/**
 * {@link CustomerContactService} interface.
 *
 * @author AbdelkarimTurki
 * @since 0.17.0
 */
public interface CustomerContactService {

	/**
	 * Find {@link CustomerContactDTO} by given ID.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 * @return the CustomerContactService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerContactDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link CustomerContactDTO} by given params.
	 * 
	 * @author AbdelkarimTurki
	 * @param customerContactDTO the customerContact DTO
	 * @return the list
	 */
	List<CustomerContactDTO> find(CustomerContactDTO customerContactDTO);

	/**
	 * The method used for saving the given {@link CustomerContactDTO}.
	 *
	 * @author AbdelkarimTurki
	 * @param customerContactDTO the customerContact DTO
	 * @return the CustomerContactService DTO
	 * @throws MappingException the mapping exception
	 */
	CustomerContactDTO save(CustomerContactDTO customerContactDTO);

	/**
	 * The method used for updating the given {@link CustomerContactDTO} by ID.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 * @param customerContactDTO the customerContact DTO
	 * @return the CustomerContactService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CustomerContactDTO save(Long id, CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException;

	/**
	 * The method used for saving the given {@link CustomerContactDTO}.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the customerContact DTO
	 * @return the CustomerContactService DTO
	 * @throws CustomerContactException the customer contact exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws MappingException the mapping exception
	 */
	CustomerContactDTO saveMail(CustomerContactDTO customerContactDTO)
			throws CustomerContactException, ResourcesNotFoundException;

	/**
	 * The method disable message in database.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the messages DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void disableContact(CustomerContactDTO customerContactDTO) throws ResourcesNotFoundException;
}
