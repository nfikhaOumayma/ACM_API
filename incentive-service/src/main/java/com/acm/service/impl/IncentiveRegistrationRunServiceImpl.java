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
import java.util.Date;
import java.util.List;

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

import com.acm.repository.IncentiveRegistrationRunRepository;
import com.acm.service.IncentiveRegistrationRunService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.models.IncentiveRegistrationRun;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link IncentiveRegistrationRunServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveRegistrationRunServiceImpl implements IncentiveRegistrationRunService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveRegistrationRunServiceImpl.class);

	/** The incentive registration run repository. */
	@Autowired
	private IncentiveRegistrationRunRepository incentiveRegistrationRunRepository;

	/** The sheet printed on. */
	private static final String PRINTED_ON = "Printed On : ";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRunService#runIncentiveCalculate()
	 */
	@Override
	public void runIncentiveCalculate() {

		logger.info("RUN incentive Registration Procedure");
		Integer numberRows = incentiveRegistrationRunRepository.runProcedureCalculateIncentive();
		logger.info("Number of processing LOAN = {}", numberRows);
		logger.info("RUN incentive Registration Procedure :: DONE");
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.IncentiveRegistrationRunService#generateIncentiveReport(java.lang.Integer,
	 * java.lang.Integer)
	 */
	@Override
	public byte[] generateIncentiveReport(Integer year, Integer month) {

		logger.info("START generate Incentive Registration Excel Report");
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
		List<IncentiveRegistrationRun> incentiveRegistrationRuns =
				incentiveRegistrationRunRepository.getByYearAndMonth(year, month);
		logger.info("All founded data: incentiveRegistrationRuns = {}",
				incentiveRegistrationRuns.size());

		// init run datetime
		String runDate = "NONE";
		String runMonth = "NONE";
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRegistrationRuns)) {
			// init run datetime
			runDate = dateFormat.format(incentiveRegistrationRuns.get(0).getRunDate());
			runMonth = incentiveRegistrationRuns.get(0).getMonth();
		}
		// The sheet name
		final String SHEET_LOAN_OFFICER = "مسئول تمويل";
		final String[] COLUMNS_LOAN_OFFICER = {"الفرع", "مسئول_تمويل_الاخصائي",
				"عددالعملاءالاجمالي", "عميل جديد", "تجديد العميل", "الحافز"};

		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet = workbook.createSheet(SHEET_LOAN_OFFICER);
			int sheetRowIdx = 1;

			// header tables styles
			Font headerFont = workbook.createFont();
			// inverse from right to left
			((XSSFSheet) sheet).getCTWorksheet().getSheetViews().getSheetViewArray(0)
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
			sheetRowIdx++;

			// PRINTED_ON
			CellRangeAddress regionPrintedOn = new CellRangeAddress(sheetRowIdx, sheetRowIdx, 0, 2);
			sheet.addMergedRegion(regionPrintedOn);
			Row printedOnRow = sheet.createRow(sheetRowIdx);
			Cell cellPrintedOn = printedOnRow.createCell(0);
			cellPrintedOn.setCellValue(PRINTED_ON + printedOn);
			RegionUtil.setBorderBottom(BorderStyle.MEDIUM, regionPrintedOn, sheet);
			RegionUtil.setBorderTop(BorderStyle.MEDIUM, regionPrintedOn, sheet);
			RegionUtil.setBorderLeft(BorderStyle.MEDIUM, regionPrintedOn, sheet);
			RegionUtil.setBorderRight(BorderStyle.MEDIUM, regionPrintedOn, sheet);
			sheetRowIdx = sheetRowIdx + 2;

			// RUN ON
			CellRangeAddress regionRunOn = new CellRangeAddress(sheetRowIdx, sheetRowIdx, 4, 5);
			sheet.addMergedRegion(regionRunOn);
			Row runOnRow = sheet.createRow(sheetRowIdx);
			Cell cellrunOn = runOnRow.createCell(4);
			cellrunOn.setCellValue("Run on : " + runDate);
			RegionUtil.setBorderBottom(BorderStyle.MEDIUM, regionRunOn, sheet);
			RegionUtil.setBorderTop(BorderStyle.MEDIUM, regionRunOn, sheet);
			RegionUtil.setBorderLeft(BorderStyle.MEDIUM, regionRunOn, sheet);
			RegionUtil.setBorderRight(BorderStyle.MEDIUM, regionRunOn, sheet);
			sheetRowIdx++;

			// MONTH of RUN
			CellRangeAddress regionRunMonth = new CellRangeAddress(sheetRowIdx, sheetRowIdx, 4, 5);
			sheet.addMergedRegion(regionRunMonth);
			Row monthRow = sheet.createRow(sheetRowIdx);
			Cell cellmonth = monthRow.createCell(4);
			cellmonth.setCellValue("Month : " + runMonth.toUpperCase());
			RegionUtil.setBorderBottom(BorderStyle.MEDIUM, regionRunMonth, sheet);
			RegionUtil.setBorderTop(BorderStyle.MEDIUM, regionRunMonth, sheet);
			RegionUtil.setBorderLeft(BorderStyle.MEDIUM, regionRunMonth, sheet);
			RegionUtil.setBorderRight(BorderStyle.MEDIUM, regionRunMonth, sheet);
			sheetRowIdx = sheetRowIdx + 2;

			// Header Sheet 1
			Row headerRowSheet1 = sheet.createRow(sheetRowIdx++);
			for (int col = 0; col < COLUMNS_LOAN_OFFICER.length; col++) {
				Cell cellHeaderRow = headerRowSheet1.createCell(col);
				cellHeaderRow.setCellValue(COLUMNS_LOAN_OFFICER[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}

			// build table data for COLUMNS_LOAN_OFFICER
			for (IncentiveRegistrationRun incentive : incentiveRegistrationRuns) {
				Row row = sheet.createRow(sheetRowIdx++);
				// الفرع
				row.createCell(0).setCellValue(incentive.getBranch());
				row.getCell(0).setCellStyle(styleBorder);
				// مسئول_تمويل_الاخصائي
				row.createCell(1).setCellValue(incentive.getLoanOfficerName());
				row.getCell(1).setCellStyle(styleBorder);
				// عددالعملاءالاجمالي
				row.createCell(2).setCellValue(
						incentive.getTotalCustomerMEL() + incentive.getTotalCustomerVSE());
				row.getCell(2).setCellStyle(styleBorder);
				// عميل جديد
				row.createCell(3).setCellValue(
						incentive.getNewCustomerMEL() + incentive.getNewCustomerVSE());
				row.getCell(3).setCellStyle(styleBorder);
				// تجديد العميل
				row.createCell(4).setCellValue(
						incentive.getReNewalCustomerMEL() + incentive.getReNewalCustomerVSE());
				row.getCell(4).setCellStyle(styleBorder);
				// الحافز
				row.createCell(5).setCellValue(incentive.getIncentiveValue());
				row.getCell(5).setCellStyle(styleBorder);
			}

			// auto size cells : sheet1
			for (int i = 0; i < COLUMNS_LOAN_OFFICER.length + 1; i++) {
				sheet.autoSizeColumn(i);
			}
			// create excel file
			workbook.write(out);
			workbook.close();
			logger.info("Generating excel report INCENTIVE Registration :  {} :: DONE",
					SHEET_LOAN_OFFICER);
			return out.toByteArray();

		}
		catch (IOException e) {
			throw new RuntimeException(
					"fail to import data to Excel INCENTIVE Registration Report: "
							+ e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationRunService#getRunYear()
	 */
	@Override
	public List<Integer> getRunYear() {

		// get data from DB
		List<Integer> years = incentiveRegistrationRunRepository.getRunYear();
		logger.info("Founded YEARS = {}", years);
		if (!ACMValidationUtils.isNullOrEmpty(years)) {
			return years;
		}
		// if list is Empty => return current year
		return Arrays.asList(DateUtil.getYearFromDate(new Date()));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationRunService#getRunMonth()
	 */
	@Override
	public List<Integer> getRunMonth() {

		// get data from DB
		List<Integer> months = incentiveRegistrationRunRepository.getRunMonth();
		logger.info("Founded MONTHS = {}", months);
		if (!ACMValidationUtils.isNullOrEmpty(months)) {
			return months;
		}
		// if list is Empty => return current month
		return Arrays.asList(DateUtil.getMonthFromDate(new Date()));
	}
}
