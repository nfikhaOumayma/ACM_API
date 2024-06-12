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

import com.acm.exceptions.type.CustomerContactException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.CustomerContactService;
import com.acm.utils.dtos.CustomerContactDTO;

/**
 * This class @{link CustomerContactController} used to control all the CustomerContact requests.
 *
 * @author AbdelkarimTurki
 * @since 0.17.0
 */
@RestController
@RequestMapping("/customer-contact")
public class CustomerContactController {

	/** The CustomerContact service. */
	@Autowired
	private CustomerContactService customerContactService;

	/**
	 * Find CustomerContact by id.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 * @return the customerContact DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public CustomerContactDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return customerContactService.find(id);
	}

	/**
	 * Create the CustomerContact.
	 *
	 * @author AbdelkarimTurki
	 * @param customerContactDTO the customerContact DTO
	 * @return the CustomerContact DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public CustomerContactDTO create(@RequestBody CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException {

		return customerContactService.save(customerContactDTO);
	}

	/**
	 * Create the CustomerContact.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the customerContact DTO
	 * @return the CustomerContact DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CustomerContactException the customer contact exception
	 */
	@PostMapping("/create-mail")
	public CustomerContactDTO createMail(@RequestBody CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException, CustomerContactException {

		return customerContactService.saveMail(customerContactDTO);
	}

	/**
	 * Update the CustomerContact by id.
	 *
	 * @author AbdelkarimTurki
	 * @param customerContactDTO the customerContact DTO
	 * @return the customerContact DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public CustomerContactDTO update(@RequestBody CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException {

		return customerContactService.save(customerContactDTO.getId(), customerContactDTO);
	}

	/**
	 * Find {@link List} of {@link CustomerContactDTO} by Requested params.
	 *
	 * @author AbdelkarimTurki
	 * @param customerContactDTO the customerContact DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<CustomerContactDTO> find(@RequestBody CustomerContactDTO customerContactDTO) {

		return customerContactService.find(customerContactDTO);
	}

	/**
	 * disable message from database.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the messages loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/disable-message")
	public void disableMessage(@RequestBody CustomerContactDTO customerContactDTO)
			throws ResourcesNotFoundException {

		customerContactService.disableContact(customerContactDTO);
	}
}
