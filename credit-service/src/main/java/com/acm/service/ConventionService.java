/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.ConventionDTO;

/**
 * {@link ConventionService} interface.
 *
 * @author KhaledOuali
 * @since 1.12
 */
public interface ConventionService {

	/**
	 * Find {@link AddressDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ConventionDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link AddressDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param conventionDTO the convention DTO
	 * @return the list
	 */
	List<ConventionDTO> find(ConventionDTO conventionDTO);

	/**
	 * The method used for saving the given {@link AddressDTO}.
	 *
	 * @author HaythemBenizid
	 * @param conventionDTO the convention DTO
	 * @return the address DTO
	 */
	List<ConventionDTO> saveAll(List<ConventionDTO> conventionDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param conventionDTO the convention DTO
	 * @return the convention DTO
	 */
	ConventionDTO save(Long id, ConventionDTO conventionDTO);

	/**
	 * Find by id supplier.
	 *
	 * @param idSupplier the id supplier
	 * @return the list
	 */
	List<ConventionDTO> findByIdSupplier(Long idSupplier);

}
