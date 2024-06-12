/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.FinancialAnalysisDTO;

/**
 * {@link FinancialAnalysisAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface FinancialAnalysisAbacusService {

	/**
	 * Find Financial Analysis by given ID.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	List<FinancialAnalysisDTO> find(Long idLoan);
}
