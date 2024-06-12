/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.models.transvers.LoanScheduleAPI;

/**
 * {@link LoanCalculateApiService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
public interface LoanCalculateApiService {

	/**
	 * calculate Loan Schedules by given params from ABACUS using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	LoanScheduleAPI calculate(LoanDTO loanDTO) throws IOException, ApiAbacusException;

}
