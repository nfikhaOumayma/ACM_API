/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.SupplierDTO;
import com.acm.utils.dtos.pagination.SupplierPaginationDTO;

/**
 * {@link SupplierService} interface.
 *
 * @author KhaledOuali
 * @since 1.12
 */
public interface SupplierService {

	/**
	 * Find {@link AddressDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param supplierPaginationDTO the supplier pagination DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SupplierPaginationDTO find(SupplierPaginationDTO supplierPaginationDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link AddressDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param supplierDTO the supplier DTO
	 * @return the list
	 */
	List<SupplierDTO> find(SupplierDTO supplierDTO);

	/**
	 * The method used for saving the given {@link AddressDTO}.
	 *
	 * @author HaythemBenizid
	 * @param supplierDTO the supplier DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	SupplierDTO save(SupplierDTO supplierDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException;

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param supplierDTO the supplier DTO
	 * @return the supplier DTO
	 */
	SupplierDTO save(Long id, SupplierDTO supplierDTO);

	/**
	 * Find by id.
	 *
	 * @param id the id
	 * @return the supplier DTO
	 */
	SupplierDTO findById(Long id);

}
