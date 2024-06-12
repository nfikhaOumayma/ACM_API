/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.controller;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.ReportExcelService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;

/**
 * {@link ReportingController} class.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
@RestController
@RequestMapping("/reporting")
public class ReportingController {

	/** The report excel service. */
	@Autowired
	private ReportExcelService reportExcelService;

	/**
	 * Generate excel loan application.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the reporting DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/excel/loan-application")
	public byte[] generateExcelLoanApplication(@RequestBody ReportingDTO reportingDTO)
			throws IOException {

		return reportExcelService.generateExcelLoanApplication(reportingDTO);
	}

	/**
	 * Generate excel collection follow-up report.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the reporting DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/excel/collection-follow-up")
	public byte[] generateExcelCollectionFollowupReport(@RequestBody ReportingDTO reportingDTO)
			throws IOException {

		return reportExcelService.generateExcelCollectionFollowupReport(reportingDTO);
	}

	/**
	 * REPORT TEST EXCEL.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the reporting DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/test/loan-application")
	public byte[] testExcelLoanApplication(@RequestBody ReportingDTO reportingDTO)
			throws IOException {

		@SuppressWarnings("resource")
		XSSFWorkbook workBook = new XSSFWorkbook();
		XSSFSheet sheet = workBook.createSheet("My Sheet");
		sheet.setColumnWidth(0, 2560);
		sheet.setColumnWidth(1, 2560);
		Row row = sheet.createRow(0);
		row.createCell(0).setCellValue("Hello World");

		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			workBook.write(bos);
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			bos.close();
		}
		byte[] bytes = bos.toByteArray();

		return bytes;
	}

	/**
	 * Generate AML report.
	 *
	 * @author Yesser Somai
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/report-aml-excel")
	public byte[] generateExcel(@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws IOException {

		return reportExcelService.generateAMLReport(thirdPartyHistoriqueDTO);
	}

	/**
	 * Generate excel schedule.
	 *
	 * @author Ines Dridi
	 * @param loanSchedule the loan schedule
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/report-schedule-excel")
	public byte[] generateExcelSchedule(@RequestBody LoanScheduleDTO loanSchedule)
			throws IOException {

		return reportExcelService.generateExcelSchedule(loanSchedule);
	}

	/**
	 * Calculate financial report.
	 *
	 * @param loanDTO the loan DTO
	 * @return the map
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/calculate_financial_report")
	public Map<String, Object> calculateFinancialReport(@RequestBody LoanDTO loanDTO)
			throws IOException {

		return reportExcelService.calculateFinancialReport(loanDTO);
	}

}
