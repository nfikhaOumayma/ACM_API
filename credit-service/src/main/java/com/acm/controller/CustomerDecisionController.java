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
import com.acm.service.CustomerDecisionService;
import com.acm.utils.dtos.CustomerDecisionDTO;

/**
 * This class @{link CustomerDesicionController} used to control all the CustomerDesicion requests.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
@RestController
@RequestMapping("/customer-decision")
public class CustomerDecisionController {

	/** The CustomerDesicion service. */
	@Autowired
	private CustomerDecisionService customerDesicionService;

	/**
	 * Find CustomerDesicionF by id.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @return the customer decision DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public CustomerDecisionDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return customerDesicionService.find(id);
	}

	/**
	 * Find {@link List} of {@link CustomerDecisionDTO} by Requested params.
	 *
	 * @author YesserSomai
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<CustomerDecisionDTO> find(@RequestBody CustomerDecisionDTO customerDesicionDTO) {

		return customerDesicionService.find(customerDesicionDTO);
	}

	/**
	 * Create the CustomerDesicion.
	 *
	 * @author YesserSomai
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the CustomerDesicion DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public CustomerDecisionDTO create(@RequestBody CustomerDecisionDTO customerDesicionDTO)
			throws ResourcesNotFoundException {

		return customerDesicionService.save(customerDesicionDTO);
	}

	/**
	 * Update the CustomerDesicion by id.
	 *
	 * @author YesserSomai
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the customer decision DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public CustomerDecisionDTO update(@RequestBody CustomerDecisionDTO customerDesicionDTO)
			throws ResourcesNotFoundException {

		return customerDesicionService.save(customerDesicionDTO.getId(), customerDesicionDTO);
	}

	/**
	 * Validate the Customer Desicion.
	 *
	 * @author HaythemBenizid
	 * @param customerDesicionDTO the customer desicion DTO
	 * @return the customer desicion DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/validate")
	public CustomerDecisionDTO validate(@RequestBody CustomerDecisionDTO customerDesicionDTO)
			throws ResourcesNotFoundException {

		return customerDesicionService.validate(customerDesicionDTO);
	}
}
