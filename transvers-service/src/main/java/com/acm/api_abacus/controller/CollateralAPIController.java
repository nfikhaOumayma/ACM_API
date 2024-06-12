/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.controller;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.service.CollateralApiService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link CollateralAPIController } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/load-data-api-abacus")
public class CollateralAPIController {

	/** The collateral api service. */
	@Autowired
	private CollateralApiService collateralApiService;

	/**
	 * Adds the loan collateral.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the acm collateral
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	@PostMapping("/add-loan-collateral")
	public List<AcmCollateralDTO> addLoanCollateral(@RequestBody LoanDTO loanDTO)
			throws IOException, ApiAbacusException, URISyntaxException {

		return collateralApiService.save(loanDTO);

	}
}
