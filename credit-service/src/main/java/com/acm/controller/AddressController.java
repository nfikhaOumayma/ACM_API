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

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AddressService;
import com.acm.utils.dtos.AddressDTO;

/**
 * This class @{link AddressController} used to control all the {@link AddressDTO} requests.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/address")
public class AddressController {

	/** The Address service. */
	@Autowired
	private AddressService addressService;

	/**
	 * Find Address by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public AddressDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return addressService.find(id);
	}

	/**
	 * Find {@link List} of {@link AddressDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AddressDTO> find(@RequestBody AddressDTO addressDTO) {

		return addressService.find(addressDTO);
	}

	/**
	 * Create the Address.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the Address DTO
	 */
	@PostMapping("/create")
	public AddressDTO create(@RequestBody AddressDTO addressDTO) {

		return addressService.save(addressDTO);
	}

	/**
	 * Create the Address by BATCH.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the Address DTO
	 */
	@PostMapping("/create-by-batch")
	public AddressDTO createByBatch(@RequestBody AddressDTO addressDTO) {

		return addressService.saveByBatch(addressDTO);
	}

	/**
	 * Update the Address by id.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AddressDTO update(@RequestBody AddressDTO addressDTO) throws ResourcesNotFoundException {

		return addressService.save(addressDTO.getId(), addressDTO);
	}

	/**
	 * Update all.
	 *
	 * @author YesserSomai
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-all")
	public AddressDTO updateAll(@RequestBody AddressDTO addressDTO)
			throws ResourcesNotFoundException {

		return addressService.saveAll(addressDTO);
	}

}
