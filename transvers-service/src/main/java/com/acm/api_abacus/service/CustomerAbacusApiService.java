/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.List;

import com.acm.api_abacus.model.CustomerAbacusAPIModel;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.ScheduleDTO;

// TODO: Auto-generated Javadoc
/**
 * {@link CustomerAbacusApiService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface CustomerAbacusApiService {

	/**
	 * Add Customer by given params in ABACUS-DB using API.
	 *
	 * @author YesserSomai
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	CustomerDTO save(CustomerDTO customerDTO) throws IOException, ApiAbacusException;

	/**
	 * Update Customer by given params in ABACUS-DB using API.
	 *
	 * @author YesserSomai
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void update(CustomerDTO customerDTO) throws IOException, ApiAbacusException;

	/**
	 * Gets the data by given ID.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the data
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	CustomerAbacusAPIModel getData(Long idCustomer)
			throws IOException, ApiAbacusException, URISyntaxException;

	/**
	 * Find account schedule by customer id.
	 *
	 * @author MoezMhiri
	 * @param idCustomer the id customer
	 * @param accountNumberExtern the account number extern
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ParseException the parse exception
	 */
	List<ScheduleDTO> findAccountScheduleByCustomerId(Long idCustomer, String accountNumberExtern) throws IOException, ApiAbacusException, URISyntaxException, ParseException;
}
