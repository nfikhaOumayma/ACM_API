/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AddressService;
import com.acm.service.ConventionService;
import com.acm.service.SupplierService;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.ConventionDTO;
import com.acm.utils.dtos.SupplierDTO;
import com.acm.utils.dtos.pagination.SupplierPaginationDTO;

/**
 * The Class SupplierController.
 */
@RestController
@RequestMapping("/supplier")
public class SupplierController {

	/** The supplier service. */
	@Autowired
	private SupplierService supplierService;

	/** The convention service. */
	@Autowired
	private ConventionService conventionService;

	/** The address service. */
	@Autowired
	private AddressService addressService;

	/**
	 * Create the Supplier.
	 *
	 * @author khaledOuali
	 * @param supplierDTO the supplier DTO
	 * @return the supplier DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	@PostMapping("/create")
	public SupplierDTO create(@RequestBody SupplierDTO supplierDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException {

		return supplierService.save(supplierDTO);
	}

	/**
	 * Update the Customer by id.
	 *
	 * @author khaledOuali
	 * @param supplierDTO the supplier DTO
	 * @return the supplier DTO
	 */
	@PutMapping("/update")
	public SupplierDTO update(@RequestBody SupplierDTO supplierDTO) {

		return supplierService.save(supplierDTO.getId(), supplierDTO);
	}

	/**
	 * Create the Supplier.
	 *
	 * @author khaledOuali
	 * @param conventionDTOLst the convention DTO lst
	 * @return the supplier DTO
	 */
	@PostMapping("/convention/create")
	public List<ConventionDTO> create(@RequestBody List<ConventionDTO> conventionDTOLst) {

		return conventionService.saveAll(conventionDTOLst);
	}

	/**
	 * Find pagination.
	 * 
	 * @param supplierPaginationDTO the supplier pagination DTO
	 * @return the supplier pagination DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-pagination")
	public SupplierPaginationDTO findPagination(
			@RequestBody SupplierPaginationDTO supplierPaginationDTO)
			throws ResourcesNotFoundException {

		return supplierService.find(supplierPaginationDTO);
	}

	/**
	 * Find by id.
	 *
	 * @param id the id
	 * @return the supplier DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public SupplierDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		System.out.println(id);
		return supplierService.findById(id);
	}

	/**
	 * Creates the lst address.
	 *
	 * @param addressDTOLst the address DTO lst
	 * @return the list
	 */
	@PostMapping("/address/create")
	public List<AddressDTO> createLstAddress(@RequestBody List<AddressDTO> addressDTOLst) {

		return addressService.saveAllAddress(addressDTOLst);
	}

}
