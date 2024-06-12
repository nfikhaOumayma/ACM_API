/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.service.LoanScheduleAPIService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanSchedulesApiDTO;

/**
 * The Class LoanScheduleAPIController.
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoanScheduleAPIController {

	/** The loan schedule api service. */
	@Autowired
	private LoanScheduleAPIService loanScheduleApiService;

	/**
	 * Gets the all schedules by customer.
	 *
	 * @param idCustomerExtern the id customer extern
	 * @return the all schedules by customer
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@GetMapping("/loan-schedules/{idCustomerExtern}")
	public List<LoanSchedulesApiDTO> getAllSchedulesByCustomer(
			@PathVariable("idCustomerExtern") String idCustomerExtern)
			throws ApiAbacusException, IOException {

		return loanScheduleApiService.getAllSchedules(idCustomerExtern);
	}

}
