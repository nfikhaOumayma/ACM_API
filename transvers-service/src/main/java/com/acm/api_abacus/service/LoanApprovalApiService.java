/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;
import java.util.List;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link LoanApprovalApiService} interface.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public interface LoanApprovalApiService {

	/**
	 * approvel Loan by given params in ABACUS-DB using API (INDIV / ORG).
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	void approvel(LoanDTO loanDTO)
			throws IOException, CheckApprovelLevelException, ApiAbacusException;

	/**
	 * approvel GROUP Loan by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	void approvelGroup(List<LoanDTO> loanDTOs)
			throws IOException, CheckApprovelLevelException, ApiAbacusException;

}
