/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.FinancialAnalysisAbacusService;
import com.acm.utils.dtos.FinancialAnalysisDTO;

/**
 * This class @{link LoadDataFinancialAnalysisController}.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataFinancialAnalysisController {

	/** The financialAnalysis abacus service. */
	@Autowired
	private FinancialAnalysisAbacusService financialAnalysisAbacusService;

	/**
	 * Find financialAnalysis by idLoan.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id Loan
	 * @return the financialAnalysis DTO
	 */
	@GetMapping("/financial-analysis/{idLoan}")
	public List<FinancialAnalysisDTO> findByLoan(@PathVariable("idLoan") Long idLoan) {

		return financialAnalysisAbacusService.find(idLoan);
	}
}
