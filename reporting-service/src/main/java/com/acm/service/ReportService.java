/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.service;

import java.sql.SQLException;
import java.util.Map;

import com.acm.exceptions.type.ReportingException;
import com.acm.utils.dtos.ReportDTO;

import net.sf.jasperreports.engine.JRDataSource;

/**
 * {@link ReportService} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface ReportService {

	/**
	 * Inits the.
	 */
	void init();

	/**
	 * Delete all.
	 */
	void deleteAll();

	/**
	 * Generates a PDF report with the given input file.
	 *
	 * @author HaythemBenizid
	 * @param inputFileName report source file without extension
	 * @param params report parameters
	 * @param dataSource the source of data
	 * @return the byte[] containing the PDF
	 * @throws ReportingException the reporting exception
	 */
	byte[] generatePDFReport(String inputFileName, Map<String, Object> params,
			JRDataSource dataSource) throws ReportingException;

	/**
	 * Generate PDF.
	 *
	 * @author HaythemBenizid
	 * @param loanReportDTO the loan report DTO
	 * @return the byte[]
	 * @throws ReportingException the reporting exception
	 */
	byte[] generatePDF(ReportDTO loanReportDTO) throws ReportingException;

	/**
	 * Generate report.
	 *
	 * @author HaythemBenizid
	 * @param loanReportDTO the loan report DTO
	 * @return the byte[]
	 * @throws SQLException the SQL exception
	 * @throws ReportingException the reporting exception
	 */
	byte[] generateReport(ReportDTO loanReportDTO) throws SQLException, ReportingException;
}
