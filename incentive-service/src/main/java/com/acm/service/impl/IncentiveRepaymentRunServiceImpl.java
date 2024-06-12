/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.repository.IncentiveRepaymentRunRepository;
import com.acm.service.IncentiveRepaymentRunService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.models.IncentiveRepaymentRun;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link IncentiveRepaymentRunServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveRepaymentRunServiceImpl implements IncentiveRepaymentRunService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveRepaymentRunServiceImpl.class);

	/** The incentive repayment run repository. */
	@Autowired
	private IncentiveRepaymentRunRepository incentiveRepaymentRunRepository;

	/** The sheet printed on. */
	private static final String PRINTED_ON = "Printed On : ";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRunService#runIncentiveCalculate()
	 */
	@Override
	public void runIncentiveCalculate() {

		logger.info("RUN incentive Repayment Procedure");
		Integer numberRows = incentiveRepaymentRunRepository.runProcedureCalculateIncentive();
		logger.info("Number of processing LOAN = {}", numberRows);
		logger.info("RUN incentive Repayment Procedure :: DONE");
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentRunService#generateIncentiveReport(java.lang.Integer,
	 * java.lang.Integer)
	 */
	@Override
	public byte[] generateIncentiveReport(Integer year, Integer month) {

		logger.info("START generate Incentive Repayment Excel Report");
		// init instant datetime
		DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
		String printedOn = dateFormat.format(new Date());
		// check & init year
		if (ACMValidationUtils.isNullOrEmpty(year)) {
			year = DateUtil.getYearFromDate(new Date());
		}
		logger.info("YEAR of request = {}", year);
		// check & init month
		if (ACMValidationUtils.isNullOrEmpty(month)) {
			month = DateUtil.getMonthFromDate(new Date());
		}
		logger.info("MONTH of request = {}", month);
		// load && parse response data
		List<IncentiveRepaymentRun> incentiveRepaymentRuns =
				incentiveRepaymentRunRepository.getByYearAndMonth(year, month);
		logger.info("All founded data: incentiveRepaymentRuns = {}", incentiveRepaymentRuns.size());
		// init run datetime
		String runDate = "NONE";
		String runMonth = "NONE";
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRepaymentRuns)) {
			// init run datetime
			runDate = dateFormat.format(incentiveRepaymentRuns.get(0).getRunDate());
			runMonth = incentiveRepaymentRuns.get(0).getMonth();
		}

		List<IncentiveRepaymentRun> incentiveRepaymentRunsBranchManager = incentiveRepaymentRuns
				.stream().filter(incentive -> incentive.getRole().equals("BRANCH_MANAGER"))
				.sorted(Comparator.comparing(IncentiveRepaymentRun::getBranch))
				.collect(Collectors.toList());
		logger.info("incentiveRepaymentRunsBranchManager = {}",
				incentiveRepaymentRunsBranchManager.size());
		List<IncentiveRepaymentRun> incentiveRepaymentRunsSupervisor = incentiveRepaymentRuns
				.stream().filter(incentive -> incentive.getRole().equals("SUPERVISOR"))
				.sorted(Comparator.comparing(IncentiveRepaymentRun::getLoanSupervisor))
				.collect(Collectors.toList());
		logger.info("incentiveRepaymentRunsSupervisor = {}",
				incentiveRepaymentRunsSupervisor.size());
		List<IncentiveRepaymentRun> incentiveRepaymentRunsLoanOfficer = incentiveRepaymentRuns
				.stream().filter(incentive -> incentive.getRole().equals("LOAN_OFFICER"))
				.sorted(Comparator.comparing(IncentiveRepaymentRun::getBranch))
				.collect(Collectors.toList());
		logger.info("incentiveRepaymentRunsLoanOfficer = {}",
				incentiveRepaymentRunsLoanOfficer.size());

		// The sheet name
		final String SHEET_BRANCH_MANAGER = "مدراء الفروع";
		final String[] COLUMNS_BRANCH_MANAGER = {"الفرع", "عددالعملاءالاجمالي", "مبلغ_القرض",
				"عملاءالشهر", "مسئول_تمويل_الاخصائي", "مدير_الفرع", "الحافز"};

		final String SHEET_SUPERVISOR = "رئيس مجموعة";
		final String[] COLUMNS_SUPERVISOR = {"الفرع", "عددالعملاءالاجمالي", "مبلغ_القرض",
				"عملاءالشهر", "مسئول_تمويل_الاخصائي", "رئيس_مجموعه", "الحافز"};

		final String SHEET_LOAN_OFFICER = "مسئول تمويل";
		final String[] COLUMNS_LOAN_OFFICER = {"الفرع", "عددالعملاءالاجمالي", "مبلغ_القرض",
				"عملاءالشهر", "مسئول_تمويل_الاخصائي", "الحافز"};

		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet1 = workbook.createSheet(SHEET_BRANCH_MANAGER);
			int sheet1RowIdx = 1;
			Sheet sheet2 = workbook.createSheet(SHEET_SUPERVISOR);
			int sheet2RowIdx = 1;
			Sheet sheet3 = workbook.createSheet(SHEET_LOAN_OFFICER);
			int sheet3RowIdx = 1;

			// header tables styles
			Font headerFont = workbook.createFont();
			// inverse from right to left
			((XSSFSheet) sheet1).getCTWorksheet().getSheetViews().getSheetViewArray(0)
					.setRightToLeft(true);
			((XSSFSheet) sheet2).getCTWorksheet().getSheetViews().getSheetViewArray(0)
					.setRightToLeft(true);
			((XSSFSheet) sheet3).getCTWorksheet().getSheetViews().getSheetViewArray(0)
					.setRightToLeft(true);
			headerFont.setBold(true);
			headerFont.setFontHeightInPoints((short) 12);
			headerFont.setColor(IndexedColors.BLACK.index);
			CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);
			headerCellStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
			// add border for header
			headerCellStyle.setBorderBottom(BorderStyle.THIN);
			headerCellStyle.setBorderLeft(BorderStyle.THIN);
			headerCellStyle.setBorderRight(BorderStyle.THIN);
			headerCellStyle.setBorderTop(BorderStyle.THIN);

			// Style the cell with borders
			CellStyle styleBorder = workbook.createCellStyle();
			styleBorder.setBorderBottom(BorderStyle.THIN);
			styleBorder.setBorderLeft(BorderStyle.THIN);
			styleBorder.setBorderRight(BorderStyle.THIN);
			styleBorder.setBorderTop(BorderStyle.THIN);

			// INIT INDEX
			sheet1RowIdx++;
			sheet2RowIdx++;
			sheet3RowIdx++;

			// PRINTED_ON
			CellRangeAddress regionPrintedOn =
					new CellRangeAddress(sheet1RowIdx, sheet1RowIdx, 0, 2);
			sheet1.addMergedRegion(regionPrintedOn);
			Row printedOnRow = sheet1.createRow(sheet1RowIdx);
			Cell cellPrintedOn = printedOnRow.createCell(0);
			cellPrintedOn.setCellValue(PRINTED_ON + printedOn);
			RegionUtil.setBorderBottom(BorderStyle.MEDIUM, regionPrintedOn, sheet1);
			RegionUtil.setBorderTop(BorderStyle.MEDIUM, regionPrintedOn, sheet1);
			RegionUtil.setBorderLeft(BorderStyle.MEDIUM, regionPrintedOn, sheet1);
			RegionUtil.setBorderRight(BorderStyle.MEDIUM, regionPrintedOn, sheet1);
			sheet1RowIdx = sheet1RowIdx + 2;

			// RUN ON
			CellRangeAddress regionRunOn = new CellRangeAddress(sheet1RowIdx, sheet1RowIdx, 4, 5);
			sheet1.addMergedRegion(regionRunOn);
			Row runOnRow = sheet1.createRow(sheet1RowIdx);
			Cell cellrunOn = runOnRow.createCell(4);
			cellrunOn.setCellValue("Run on : " + runDate);
			RegionUtil.setBorderBottom(BorderStyle.MEDIUM, regionRunOn, sheet1);
			RegionUtil.setBorderTop(BorderStyle.MEDIUM, regionRunOn, sheet1);
			RegionUtil.setBorderLeft(BorderStyle.MEDIUM, regionRunOn, sheet1);
			RegionUtil.setBorderRight(BorderStyle.MEDIUM, regionRunOn, sheet1);
			sheet1RowIdx++;

			// MONTH of RUN
			CellRangeAddress regionRunMonth =
					new CellRangeAddress(sheet1RowIdx, sheet1RowIdx, 4, 5);
			sheet1.addMergedRegion(regionRunMonth);
			Row monthRow = sheet1.createRow(sheet1RowIdx);
			Cell cellmonth = monthRow.createCell(4);
			cellmonth.setCellValue("Month : " + runMonth.toUpperCase());
			RegionUtil.setBorderBottom(BorderStyle.MEDIUM, regionRunMonth, sheet1);
			RegionUtil.setBorderTop(BorderStyle.MEDIUM, regionRunMonth, sheet1);
			RegionUtil.setBorderLeft(BorderStyle.MEDIUM, regionRunMonth, sheet1);
			RegionUtil.setBorderRight(BorderStyle.MEDIUM, regionRunMonth, sheet1);
			sheet1RowIdx = sheet1RowIdx + 2;

			// Header Sheet 1
			Row headerRowSheet1 = sheet1.createRow(sheet1RowIdx++);
			for (int col = 0; col < COLUMNS_BRANCH_MANAGER.length; col++) {
				Cell cellHeaderRow = headerRowSheet1.createCell(col);
				cellHeaderRow.setCellValue(COLUMNS_BRANCH_MANAGER[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}
			// Header Sheet 2
			Row headerRowSheet2 = sheet2.createRow(sheet2RowIdx++);
			for (int col = 0; col < COLUMNS_SUPERVISOR.length; col++) {
				Cell cellHeaderRow = headerRowSheet2.createCell(col);
				cellHeaderRow.setCellValue(COLUMNS_SUPERVISOR[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}
			// Header Sheet 3
			Row headerRowSheet3 = sheet3.createRow(sheet3RowIdx++);
			for (int col = 0; col < COLUMNS_LOAN_OFFICER.length; col++) {
				Cell cellHeaderRow = headerRowSheet3.createCell(col);
				cellHeaderRow.setCellValue(COLUMNS_LOAN_OFFICER[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}

			// build table data for COLUMNS_BRANCH_MANAGER
			for (IncentiveRepaymentRun incentive : incentiveRepaymentRunsBranchManager) {
				Row row = sheet1.createRow(sheet1RowIdx++);
				// الفرع
				row.createCell(0).setCellValue(incentive.getBranch());
				row.getCell(0).setCellStyle(styleBorder);
				// عددالعملاءالاجمالي
				row.createCell(1).setCellValue(incentive.getActiveCustomer());
				row.getCell(1).setCellStyle(styleBorder);
				// مبلغ_القرض
				row.createCell(2).setCellValue(incentive.getTotalLoanAmount());
				row.getCell(2).setCellStyle(styleBorder);
				// عملاءالشهر
				row.createCell(3).setCellValue(incentive.getIssueLoanMonth());
				row.getCell(3).setCellStyle(styleBorder);
				// مسئول_تمويل_الاخصائي
				row.createCell(4).setCellValue(incentive.getLoanOfficerName());
				row.getCell(4).setCellStyle(styleBorder);
				// مدير_الفرع
				row.createCell(5).setCellValue(incentive.getLoanBranchManger());
				row.getCell(5).setCellStyle(styleBorder);
				// الحافز
				row.createCell(6).setCellValue(incentive.getIncentiveValue());
				row.getCell(6).setCellStyle(styleBorder);
			}

			// build table data for COLUMNS_SUPERVISOR
			for (IncentiveRepaymentRun incentive : incentiveRepaymentRunsSupervisor) {
				Row row = sheet2.createRow(sheet2RowIdx++);
				// الفرع
				row.createCell(0).setCellValue(incentive.getBranch());
				row.getCell(0).setCellStyle(styleBorder);
				// عددالعملاءالاجمالي
				row.createCell(1).setCellValue(incentive.getActiveCustomer());
				row.getCell(1).setCellStyle(styleBorder);
				// مبلغ_القرض
				row.createCell(2).setCellValue(incentive.getTotalLoanAmount());
				row.getCell(2).setCellStyle(styleBorder);
				// عملاءالشهر
				row.createCell(3).setCellValue(incentive.getIssueLoanMonth());
				row.getCell(3).setCellStyle(styleBorder);				
				// مسئول_تمويل_الاخصائي
				row.createCell(4).setCellValue(incentive.getLoanOfficerName());
				row.getCell(4).setCellStyle(styleBorder);
				// رئيس_مجموعه
				row.createCell(5).setCellValue(incentive.getLoanSupervisor());
				row.getCell(5).setCellStyle(styleBorder);
				// الحافز
				row.createCell(6).setCellValue(incentive.getIncentiveValue());
				row.getCell(6).setCellStyle(styleBorder);
			}

			// build table data for COLUMNS_LOAN_OFFICER
			for (IncentiveRepaymentRun incentive : incentiveRepaymentRunsLoanOfficer) {
				Row row = sheet3.createRow(sheet3RowIdx++);
				// الفرع
				row.createCell(0).setCellValue(incentive.getBranch());
				row.getCell(0).setCellStyle(styleBorder);
				// عددالعملاءالاجمالي
				row.createCell(1).setCellValue(incentive.getActiveCustomer());
				row.getCell(1).setCellStyle(styleBorder);
				// مبلغ_القرض
				row.createCell(2).setCellValue(incentive.getTotalLoanAmount());
				row.getCell(2).setCellStyle(styleBorder);
				// عملاءالشهر
				row.createCell(3).setCellValue(incentive.getIssueLoanMonth());
				row.getCell(3).setCellStyle(styleBorder);
				// مسئول_تمويل_الاخصائي
				row.createCell(4).setCellValue(incentive.getLoanOfficerName());
				row.getCell(4).setCellStyle(styleBorder);
				// الحافز
				row.createCell(5).setCellValue(incentive.getIncentiveValue());
				row.getCell(5).setCellStyle(styleBorder);
			}

			// auto size cells : sheet1
			for (int i = 0; i < COLUMNS_BRANCH_MANAGER.length + 1; i++) {
				sheet1.autoSizeColumn(i);
			}
			// auto size cells : sheet2
			for (int i = 0; i < COLUMNS_SUPERVISOR.length + 1; i++) {
				sheet2.autoSizeColumn(i);
			}
			// auto size cells : sheet3
			for (int i = 0; i < COLUMNS_LOAN_OFFICER.length + 1; i++) {
				sheet3.autoSizeColumn(i);
			}
			// create excel file
			workbook.write(out);
			workbook.close();
			logger.info("Generating excel report INCENTIVE Repayment : {} - {} - {} :: DONE",
					SHEET_BRANCH_MANAGER, SHEET_SUPERVISOR, SHEET_LOAN_OFFICER);
			return out.toByteArray();
		}
		catch (IOException e) {
			throw new RuntimeException(
					"fail to import data to Excel INCENTIVE Repayment Report: " + e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentRunService#getRunYear()
	 */
	@Override
	public List<Integer> getRunYear() {

		// get data from DB
		List<Integer> years = incentiveRepaymentRunRepository.getRunYear();
		logger.info("Founded YEARS = {}", years);
		if (!ACMValidationUtils.isNullOrEmpty(years)) {
			return years;
		}
		// if list is Empty => return current year
		return Arrays.asList(DateUtil.getYearFromDate(new Date()));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentRunService#getRunMonth()
	 */
	@Override
	public List<Integer> getRunMonth() {

		// get data from DB
		List<Integer> months = incentiveRepaymentRunRepository.getRunMonth();
		logger.info("Founded MONTHS = {}", months);
		if (!ACMValidationUtils.isNullOrEmpty(months)) {
			return months;
		}
		// if list is Empty => return current month
		return Arrays.asList(DateUtil.getMonthFromDate(new Date()));
	}
}
