/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.controller;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ReportingException;
import com.acm.service.ReportService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ReportDTO;

import net.sf.jasperreports.engine.JREmptyDataSource;

/**
 * {@link EditionController} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/edition")
public class EditionController {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(EditionController.class);

	/** The report service. */
	@Autowired
	private ReportService reportService;

	/** The Constant SIMPLE_JRXML_FILE. */
	public static final String SIMPLE_JRXML_FILE = "simple_report";

	/** The Constant TEMPLATE_TAYSIR_JRXML_FILE. */
	public static final String TEMPLATE_TAYSIR_JRXML_FILE = "template_taysir";

	/** The Constant JRXML_FILE. */
	public static final String JRXML_FILE = "advanced_report";

	/**
	 * TODO ### FOR TEST PURPOSE ### Loading data source. Generate simple report : format PDF.
	 *
	 * @param username the username
	 * @return the response entity
	 * @throws ReportingException the reporting exception
	 */
	@GetMapping("/generateSimpleReport/{username}")
	public ResponseEntity<byte[]> generateSimpleReport(
			@PathVariable(required = false) String username) throws ReportingException {

		logger.info("generate simple report : PDF");
		Map<String, Object> params = new HashMap<>();
		params.put("username", username);
		byte[] bytes =
				reportService.generatePDFReport(SIMPLE_JRXML_FILE, params, new JREmptyDataSource());
		return ResponseEntity.ok().header("Content-Type", "application/pdf; charset=UTF-8")
				.header("Content-Disposition", "inline; filename=\"" + username + ".pdf\"")
				.body(bytes);
	}

	/**
	 * Generate report.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the response entity
	 * @throws ReportingException the reporting exception
	 */
	@PostMapping("/generateReport")
	public ResponseEntity<byte[]> generateReport(@RequestBody LoanDTO loanDTO)
			throws ReportingException {

		logger.info("generate report : PDF");
		Map<String, Object> params = new HashMap<>();
		params.put("accountNumber", loanDTO.getAccountNumber());
		byte[] bytes = reportService.generatePDFReport(TEMPLATE_TAYSIR_JRXML_FILE, params,
				new JREmptyDataSource());
		return ResponseEntity.ok().header("Content-Type", "application/pdf; charset=UTF-8")
				.header("Content-Disposition",
						"inline; filename=\"" + loanDTO.getAccountNumber() + ".pdf\"")
				.body(bytes);
	}

	/**
	 * Generate Report Jasper LoanReportDTO.
	 *
	 * @author MoezMhiri
	 * @param loanReportDTO the loan report DTO
	 * @return the byte[]
	 * @throws ReportingException the reporting exception
	 */
	@PostMapping("/generatePDF/loanReport")
	public byte[] generatePDF(@RequestBody ReportDTO loanReportDTO) throws ReportingException {

		return reportService.generatePDF(loanReportDTO);
	}

	/**
	 * Generate Report BRJMF by given params {@link ReportDTO}.
	 *
	 * @author HaythemBenizid
	 * @param reportDTO the report DTO
	 * @return the byte[]
	 * @throws SQLException the SQL exception
	 * @throws ReportingException the reporting exception
	 */
	@PostMapping("/generate-report")
	public byte[] generateReport(@RequestBody ReportDTO reportDTO)
			throws SQLException, ReportingException {

		return reportService.generateReport(reportDTO);
	}
}
