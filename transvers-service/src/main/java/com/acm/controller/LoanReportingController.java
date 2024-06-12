/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.LoanReportingService;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingSchedulesStatusDTO;

/**
 * This class @{link LoadDataUserController}.
 *
 * @author HaythemBenizid
 * @since 1.1.2
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoanReportingController {

	/** The loan reporting service. */
	@Autowired
	private LoanReportingService loanReportingService;

	/**
	 * Reporting schedules status.
	 * 
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the list
	 */
	@PostMapping("/reporting-schedules-status")
	public List<ReportingSchedulesStatusDTO> reportingSchedulesStatus(
			@RequestBody ReportingDTO reportingDTO) {

		return loanReportingService.findSchedulesStatus(reportingDTO);
	}
}
