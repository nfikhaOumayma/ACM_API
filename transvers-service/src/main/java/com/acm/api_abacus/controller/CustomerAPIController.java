/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.controller;

import java.io.IOException;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.acm.api_abacus.model.CustomerAbacusAPIModel;
import com.acm.api_abacus.service.CustomerAbacusApiService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.ScheduleDTO;

/**
 * This class @{link CustomerAPIController}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/load-data-api-abacus")
public class CustomerAPIController {

	/** The customer abacus api service. */
	@Autowired
	private CustomerAbacusApiService customerAbacusApiService;

	// @Autowired
	// private Tracer tracer;

	/**
	 * Add Customer Abacus Api Service.
	 *
	 * @author YesserSomai
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/add-customer")
	public CustomerDTO addCustomer(@RequestBody CustomerDTO customerDTO)
			throws IOException, ApiAbacusException {

		// Span controllerSpan = tracer.buildSpan("Transvers-service/add-customer: Controller")
		// .withTag("Customer", "Customer Save Abacus Tracking").start();

		CustomerDTO apiCustomerDTO = customerAbacusApiService.save(customerDTO);
		// controllerSpan.finish();

		return apiCustomerDTO;

	}

	/**
	 * update Customer Abacus Api Service.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/update-customer")
	public void updateCustomer(@RequestBody CustomerDTO customerDTO)
			throws IOException, ApiAbacusException {

		customerAbacusApiService.update(customerDTO);
	}

	
	
	/**
	 * Find Customer by id.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the customer abacus API model
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@GetMapping("/customer-data/{idCustomer}")
	public CustomerAbacusAPIModel findById(@PathVariable("idCustomer") Long idCustomer)
			throws ApiAbacusException, IOException, URISyntaxException {

		return customerAbacusApiService.getData(idCustomer);
	}
	
	/**
	 * Find Customer by id.
	 *
	 * @author MoezMhiri
	 * @param idCustomerExtern the id customer extern
	 * @param accountNumberExtern the account number extern
	 * @return the customer abacus API model
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ParseException the parse exception
	 */
	@GetMapping("/customer-account-schedule/{idCustomerExtern}/{accountNumberExtern}")
	public List<ScheduleDTO> findAccountScheduleByCustomerId(@PathVariable("idCustomerExtern") Long idCustomerExtern,
			@PathVariable("accountNumberExtern") String accountNumberExtern)
			throws ApiAbacusException, IOException, URISyntaxException, ParseException {

		return customerAbacusApiService.findAccountScheduleByCustomerId(idCustomerExtern, accountNumberExtern);
	}
}
