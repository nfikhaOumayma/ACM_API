/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

/**
 * {@link ActivitiProcessService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public interface ActivitiProcessService {

	/**
	 * Process ACTIVITI request for given Loan using PROCESS_INSTANCE_ID .
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	// LoanDTO process(LoanDTO loanDTO);

	/**
	 * Inits the process for given Loan.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param processName the process name
	 * @return the process instance
	 */
	// ProcessInstance initProcess(LoanDTO loanDTO, String processName);

}
