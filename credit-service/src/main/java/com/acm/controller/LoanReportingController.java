/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.LoanService;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingListDTO;

/**
 * This class @{link LoanReportingController} used to control all the loan reporting requests.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
@RestController
@RequestMapping("/loans-reporting")
public class LoanReportingController {

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/**
	 * Find {@link ReportingListDTO} by Requested params {@link reportingDTO}.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the reporting list DTO
	 */
	@PostMapping("/reporting-loan-application")
	public ReportingListDTO find(@RequestBody ReportingDTO reportingDTO) {

		return loanService.find(reportingDTO);
	}
}
