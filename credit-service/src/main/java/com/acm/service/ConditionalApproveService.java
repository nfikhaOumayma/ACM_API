/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AcmConditionnalApproveDTO;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link ConditionalApproveService} class.
 * 
 * @author kouali
 * @since 0.1.0
 */
public interface ConditionalApproveService {

	/**
	 * Creates the.
	 * 
	 * @author kouali
	 * @param acmConditionnalApproveDTOs the acm conditionnal approve DT os
	 * @return the list
	 */
	List<AcmConditionnalApproveDTO> create(
			List<AcmConditionnalApproveDTO> acmConditionnalApproveDTOs);

	/**
	 * Find.
	 * 
	 * @author kouali
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	List<AcmConditionnalApproveDTO> find(LoanDTO loanDTO);

	/**
	 * Update.
	 * 
	 * @author kouali
	 * @param acmConditionnalApproveDTO the acm conditionnal approve DTO
	 * @return the acm conditionnal approve DTO
	 */
	AcmConditionnalApproveDTO update(AcmConditionnalApproveDTO acmConditionnalApproveDTO);

	/**
	 * Count by id loan.
	 * 
	 * @author kouali
	 * @param idLoan the id loan
	 * @return the long
	 */
	Long countByIdLoanAndConditionnalValidation(Long idLoan);

	/**
	 * Find.
	 *
	 * @param acmConditionnalApproveDTO the acm conditionnal approve DTO
	 * @return the list
	 */
	List<AcmConditionnalApproveDTO> find(AcmConditionnalApproveDTO acmConditionnalApproveDTO);

	/**
	 * Count by item.
	 *
	 * @param idItem the id item
	 * @return the long
	 */
	Long countByItem(Long idItem);

}
