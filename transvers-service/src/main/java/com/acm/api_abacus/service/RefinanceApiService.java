/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link RefinanceApiService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface RefinanceApiService {

	/**
	 * Save.
	 * 
	 * @author mlamloum
	 * @param customerDTO the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void save(LoanDTO customerDTO) throws IOException, ApiAbacusException;

}
