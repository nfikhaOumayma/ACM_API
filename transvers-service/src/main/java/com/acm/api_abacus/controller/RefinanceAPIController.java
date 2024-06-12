/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.service.RefinanceApiService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link RefinanceAPIController} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/load-data-api-abacus")
public class RefinanceAPIController {

	/** The refinance api service. */
	@Autowired
	private RefinanceApiService refinanceApiService;

	/**
	 * Creates the refinance loan.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PutMapping("/create-refinance-loan")
	public void createRefinanceLoan(@RequestBody LoanDTO loanDTO)
			throws IOException, ApiAbacusException {

		refinanceApiService.save(loanDTO);
	}
}
