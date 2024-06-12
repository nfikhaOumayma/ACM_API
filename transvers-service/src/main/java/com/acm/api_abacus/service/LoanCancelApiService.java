/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link LoanCancelApiService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
public interface LoanCancelApiService {

	/**
	 * Cancel Loan by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void cancel(LoanDTO loanDTO) throws IOException, ApiAbacusException;

}
