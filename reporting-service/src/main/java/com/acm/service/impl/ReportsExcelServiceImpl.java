/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.service.impl;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFClientAnchor;
import org.apache.poi.xssf.usermodel.XSSFDrawing;
import org.apache.poi.xssf.usermodel.XSSFFormulaEvaluator;
import org.apache.poi.xssf.usermodel.XSSFPicture;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.client.GedClient;
import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.service.ReportExcelService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AMLDataDTO;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.BrancheDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.LoanSourceOfFundsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingListDTO;
import com.acm.utils.dtos.ReportingListGroupByDTO;
import com.acm.utils.dtos.ReportingSchedulesStatusDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsModelDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

/**
 * {@link ReportsExcelServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
@Service
public class ReportsExcelServiceImpl implements ReportExcelService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ReportsExcelServiceImpl.class);

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The file system storage service impl. */
	@Autowired
	FileSystemStorageServiceImpl fileSystemStorageServiceImpl;

	/** The type. */
	public static final String TYPE =
			"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

	/** The Constant REQUEST_PARAMS_TITEL. */
	private static final String REQUEST_PARAMS_TITEL = " محددات التقرير";

	/** The sheet printed on. */
	private static final String SHEET_PRINTED_ON = "printed On : ";

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ReportService#generateExcelLoanApplication(com.acm.utils.dtos.ReportingDTO)
	 */
	@Override
	public byte[] generateExcelLoanApplication(ReportingDTO reportingDTO) throws IOException {

		// load loan application by given params
		ReportingListDTO reportingListDTO = creditClient.find(reportingDTO);
		// generate excel file
		return generateLoanApplicationReport(reportingListDTO, reportingDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ReportExcelService#generateExcelCollectionFollowupReport(com.acm.utils.dtos.
	 * ReportingDTO)
	 */
	@Override
	public byte[] generateExcelCollectionFollowupReport(ReportingDTO reportingDTO)
			throws IOException {

		// load Collection Follow up list from ABACUS DB
		List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs =
				transversClient.reportingSchedulesStatus(reportingDTO);

		// filter && processing founded data
		ReportingListDTO reportingListDTO = new ReportingListDTO();
		if (Boolean.TRUE.equals(reportingDTO.getProduct())) {
			buildReportDataGroupByProduct(reportingSchedulesStatusDTOs, reportingListDTO);
		}
		else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
			buildReportDataGroupByLoanOfficer(reportingSchedulesStatusDTOs, reportingListDTO);
		}
		else if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
			buildReportDataGroupByBranch(reportingSchedulesStatusDTOs, reportingListDTO);
		}
		else {
			// default by product
			buildReportDataGroupByProduct(reportingSchedulesStatusDTOs, reportingListDTO);
		}

		// generate excel file
		return generateCollectionFollowupReport(reportingListDTO, reportingDTO);
	}

	/**
	 * Builds the report data group by product.
	 *
	 * @author HaythemBenizid
	 * @param reportingSchedulesStatusDTOs the reporting schedules status DT os
	 * @param reportingListDTO the reporting list DTO
	 * @return the reporting list DTO
	 */
	private ReportingListDTO buildReportDataGroupByProduct(
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs,
			ReportingListDTO reportingListDTO) {

		List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOListFiltered =
				reportingSchedulesStatusDTOs.stream()
						.filter(CommonFunctions
								.distinctByKey(ReportingSchedulesStatusDTO::getProductID))
						.collect(Collectors.toList());
		logger.debug("List PRODUCT : {} ", reportingSchedulesStatusDTOListFiltered.size());
		List<ReportingListGroupByDTO> reportingListGroupByDTOs = new ArrayList<>();

		// init response list && calculate TOTAL
		for (ReportingSchedulesStatusDTO reportingSchedulesStatusProduct : reportingSchedulesStatusDTOListFiltered) {
			Integer selectedProductId = reportingSchedulesStatusProduct.getProductID();
			ReportingListGroupByDTO reportingListGroupByDTO = new ReportingListGroupByDTO();
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOListFilteredByProduct =
					reportingSchedulesStatusDTOs.stream()
							.filter(l -> l.getProductID() == selectedProductId)
							.collect(Collectors.toList());
			logger.debug("reportingSchedulesStatusDTOListFilteredByProduct by {} = {} ",
					selectedProductId, reportingSchedulesStatusDTOListFilteredByProduct.size());
			reportingListGroupByDTO.setReportingSchedulesStatusDTOs(
					reportingSchedulesStatusDTOListFilteredByProduct);
			// setting TotalRecords
			reportingListGroupByDTO
					.setTotalRecords(reportingSchedulesStatusDTOListFilteredByProduct.size());
			// Calculate && setting totals
			Long totalInstalmentAmount = 0L;
			Long totalInstalmentPrincipal = 0L;
			Long totalInstalmentInterest = 0L;
			Long totalInstalPrincipalPaid = 0L;
			Long totalInstalInterestPaid = 0L;
			Long totalInstalTotalPaid = 0L;
			Long totalTotalPrincipalPaid = 0L;
			Long totalTotalInterestPaid = 0L;
			Long totalTotalPaidAmount = 0L;
			Long totalUnpaidPrincipal = 0L;
			Long totalUnpaidInterest = 0L;
			Long totalUnpaidAmount = 0L;
			Long totalRemainingPrincipal = 0L;
			Long totalRemaininginterest = 0L;
			Long totalRemainingAmount = 0L;
			for (ReportingSchedulesStatusDTO rss : reportingSchedulesStatusDTOListFilteredByProduct) {
				totalInstalmentAmount += rss.getInstalmentAmount();
				totalInstalmentPrincipal += rss.getInstalmentPrincipal();
				totalInstalmentInterest += rss.getInstalmentInterest();
				totalInstalPrincipalPaid += rss.getInstalmentPrincipalPaid();
				totalInstalInterestPaid += rss.getInstalmentinterestPaid();
				totalInstalTotalPaid += rss.getInstalmentTotalPaid();
				totalTotalPrincipalPaid += rss.getTotalPrincipalPaid();
				totalTotalInterestPaid += rss.getTotalInterestPaid();
				totalTotalPaidAmount += rss.getTotalPaidAmount();
				totalUnpaidPrincipal += rss.getUnpaidPrincipal();
				totalUnpaidInterest += rss.getUpaidInterest();
				totalUnpaidAmount += rss.getUpaidAmount();
				totalRemainingPrincipal += rss.getRemainingPrincipal();
				totalRemaininginterest += rss.getRemainingInterest();
				totalRemainingAmount += rss.getRemainingAmount();
			}
			reportingListGroupByDTO.setTotalInstalmentAmount(totalInstalmentAmount);
			reportingListGroupByDTO.setTotalInstalmentPrincipal(totalInstalmentPrincipal);
			reportingListGroupByDTO.setTotalInstalmentInterest(totalInstalmentInterest);
			reportingListGroupByDTO.setTotalInstalPrincipalPaid(totalInstalPrincipalPaid);
			reportingListGroupByDTO.setTotalInstalInterestPaid(totalInstalInterestPaid);
			reportingListGroupByDTO.setTotalInstalTotalPaid(totalInstalTotalPaid);
			reportingListGroupByDTO.setTotalTotalPrincipalPaid(totalTotalPrincipalPaid);
			reportingListGroupByDTO.setTotalTotalInterestPaid(totalTotalInterestPaid);
			reportingListGroupByDTO.setTotalTotalPaidAmount(totalTotalPaidAmount);
			reportingListGroupByDTO.setTotalUnpaidPrincipal(totalUnpaidPrincipal);
			reportingListGroupByDTO.setTotalUnpaidInterest(totalUnpaidInterest);
			reportingListGroupByDTO.setTotalUnpaidAmount(totalUnpaidAmount);
			reportingListGroupByDTO.setTotalRemainingPrincipal(totalRemainingPrincipal);
			reportingListGroupByDTO.setTotalRemaininginterest(totalRemaininginterest);
			reportingListGroupByDTO.setTotalRemainingAmount(totalRemainingAmount);
			reportingListGroupByDTOs.add(reportingListGroupByDTO);
		}
		// init response object
		reportingListDTO.setReportingListGroupByDTOs(reportingListGroupByDTOs);
		// setting TotalRecords
		reportingListDTO.setTotalRecords(reportingSchedulesStatusDTOs.size());
		// Calculate && setting totals
		Long totalInstalmentAmount = 0L;
		Long totalInstalmentPrincipal = 0L;
		Long totalInstalmentInterest = 0L;
		Long totalInstalPrincipalPaid = 0L;
		Long totalInstalInterestPaid = 0L;
		Long totalInstalTotalPaid = 0L;
		Long totalTotalPrincipalPaid = 0L;
		Long totalTotalInterestPaid = 0L;
		Long totalTotalPaidAmount = 0L;
		Long totalUnpaidPrincipal = 0L;
		Long totalUnpaidInterest = 0L;
		Long totalUnpaidAmount = 0L;
		Long totalRemainingPrincipal = 0L;
		Long totalRemaininginterest = 0L;
		Long totalRemainingAmount = 0L;
		for (ReportingSchedulesStatusDTO rss : reportingSchedulesStatusDTOs) {
			totalInstalmentAmount += rss.getInstalmentAmount();
			totalInstalmentPrincipal += rss.getInstalmentPrincipal();
			totalInstalmentInterest += rss.getInstalmentInterest();
			totalInstalPrincipalPaid += rss.getInstalmentPrincipalPaid();
			totalInstalInterestPaid += rss.getInstalmentinterestPaid();
			totalInstalTotalPaid += rss.getInstalmentTotalPaid();
			totalTotalPrincipalPaid += rss.getTotalPrincipalPaid();
			totalTotalInterestPaid += rss.getTotalInterestPaid();
			totalTotalPaidAmount += rss.getTotalPaidAmount();
			totalUnpaidPrincipal += rss.getUnpaidPrincipal();
			totalUnpaidInterest += rss.getUpaidInterest();
			totalUnpaidAmount += rss.getUpaidAmount();
			totalRemainingPrincipal += rss.getRemainingPrincipal();
			totalRemaininginterest += rss.getRemainingInterest();
			totalRemainingAmount += rss.getRemainingAmount();
		}
		reportingListDTO.setTotalInstalmentAmount(totalInstalmentAmount);
		reportingListDTO.setTotalInstalmentPrincipal(totalInstalmentPrincipal);
		reportingListDTO.setTotalInstalmentInterest(totalInstalmentInterest);
		reportingListDTO.setTotalInstalPrincipalPaid(totalInstalPrincipalPaid);
		reportingListDTO.setTotalInstalInterestPaid(totalInstalInterestPaid);
		reportingListDTO.setTotalInstalTotalPaid(totalInstalTotalPaid);
		reportingListDTO.setTotalTotalPrincipalPaid(totalTotalPrincipalPaid);
		reportingListDTO.setTotalTotalInterestPaid(totalTotalInterestPaid);
		reportingListDTO.setTotalTotalPaidAmount(totalTotalPaidAmount);
		reportingListDTO.setTotalUnpaidPrincipal(totalUnpaidPrincipal);
		reportingListDTO.setTotalUnpaidInterest(totalUnpaidInterest);
		reportingListDTO.setTotalUnpaidAmount(totalUnpaidAmount);
		reportingListDTO.setTotalRemainingPrincipal(totalRemainingPrincipal);
		reportingListDTO.setTotalRemaininginterest(totalRemaininginterest);
		reportingListDTO.setTotalRemainingAmount(totalRemainingAmount);
		return reportingListDTO;
	}

	/**
	 * Builds the report data group by branch.
	 *
	 * @author HaythemBenizid
	 * @param reportingSchedulesStatusDTOs the reporting schedules status DT os
	 * @param reportingListDTO the reporting list DTO
	 * @return the reporting list DTO
	 */
	private ReportingListDTO buildReportDataGroupByBranch(
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs,
			ReportingListDTO reportingListDTO) {

		List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOListFiltered =
				reportingSchedulesStatusDTOs.stream()
						.filter(CommonFunctions
								.distinctByKey(ReportingSchedulesStatusDTO::getBranche))
						.collect(Collectors.toList());
		logger.debug("List  Branch : {} ", reportingSchedulesStatusDTOListFiltered.size());
		List<ReportingListGroupByDTO> reportingListGroupByDTOs = new ArrayList<>();
		// init response list && calculate TOTAL
		for (ReportingSchedulesStatusDTO reportingSchedulesStatus : reportingSchedulesStatusDTOListFiltered) {
			Integer selectedBranchId = reportingSchedulesStatus.getBranche();
			ReportingListGroupByDTO reportingListGroupByDTO = new ReportingListGroupByDTO();
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOListFilteredByBranch =
					reportingSchedulesStatusDTOs.stream()
							.filter(l -> l.getBranche() == selectedBranchId)
							.collect(Collectors.toList());
			logger.debug("reportingSchedulesStatusDTOListFilteredByBranch by {} = {} ",
					selectedBranchId, reportingSchedulesStatusDTOListFilteredByBranch.size());
			reportingListGroupByDTO.setReportingSchedulesStatusDTOs(
					reportingSchedulesStatusDTOListFilteredByBranch);
			// setting TotalRecords
			reportingListGroupByDTO
					.setTotalRecords(reportingSchedulesStatusDTOListFilteredByBranch.size());
			// Calculate && setting totals
			Long totalInstalmentAmount = 0L;
			Long totalInstalmentPrincipal = 0L;
			Long totalInstalmentInterest = 0L;
			Long totalInstalPrincipalPaid = 0L;
			Long totalInstalInterestPaid = 0L;
			Long totalInstalTotalPaid = 0L;
			Long totalTotalPrincipalPaid = 0L;
			Long totalTotalInterestPaid = 0L;
			Long totalTotalPaidAmount = 0L;
			Long totalUnpaidPrincipal = 0L;
			Long totalUnpaidInterest = 0L;
			Long totalUnpaidAmount = 0L;
			Long totalRemainingPrincipal = 0L;
			Long totalRemaininginterest = 0L;
			Long totalRemainingAmount = 0L;
			for (ReportingSchedulesStatusDTO rss : reportingSchedulesStatusDTOListFilteredByBranch) {
				totalInstalmentAmount += rss.getInstalmentAmount();
				totalInstalmentPrincipal += rss.getInstalmentPrincipal();
				totalInstalmentInterest += rss.getInstalmentInterest();
				totalInstalPrincipalPaid += rss.getInstalmentPrincipalPaid();
				totalInstalInterestPaid += rss.getInstalmentinterestPaid();
				totalInstalTotalPaid += rss.getInstalmentTotalPaid();
				totalTotalPrincipalPaid += rss.getTotalPrincipalPaid();
				totalTotalInterestPaid += rss.getTotalInterestPaid();
				totalTotalPaidAmount += rss.getTotalPaidAmount();
				totalUnpaidPrincipal += rss.getUnpaidPrincipal();
				totalUnpaidInterest += rss.getUpaidInterest();
				totalUnpaidAmount += rss.getUpaidAmount();
				totalRemainingPrincipal += rss.getRemainingPrincipal();
				totalRemaininginterest += rss.getRemainingInterest();
				totalRemainingAmount += rss.getRemainingAmount();
			}
			reportingListGroupByDTO.setTotalInstalmentAmount(totalInstalmentAmount);
			reportingListGroupByDTO.setTotalInstalmentPrincipal(totalInstalmentPrincipal);
			reportingListGroupByDTO.setTotalInstalmentInterest(totalInstalmentInterest);
			reportingListGroupByDTO.setTotalInstalPrincipalPaid(totalInstalPrincipalPaid);
			reportingListGroupByDTO.setTotalInstalInterestPaid(totalInstalInterestPaid);
			reportingListGroupByDTO.setTotalInstalTotalPaid(totalInstalTotalPaid);
			reportingListGroupByDTO.setTotalTotalPrincipalPaid(totalTotalPrincipalPaid);
			reportingListGroupByDTO.setTotalTotalInterestPaid(totalTotalInterestPaid);
			reportingListGroupByDTO.setTotalTotalPaidAmount(totalTotalPaidAmount);
			reportingListGroupByDTO.setTotalUnpaidPrincipal(totalUnpaidPrincipal);
			reportingListGroupByDTO.setTotalUnpaidInterest(totalUnpaidInterest);
			reportingListGroupByDTO.setTotalUnpaidAmount(totalUnpaidAmount);
			reportingListGroupByDTO.setTotalRemainingPrincipal(totalRemainingPrincipal);
			reportingListGroupByDTO.setTotalRemaininginterest(totalRemaininginterest);
			reportingListGroupByDTO.setTotalRemainingAmount(totalRemainingAmount);
			reportingListGroupByDTOs.add(reportingListGroupByDTO);
		}
		// init response object
		reportingListDTO.setReportingListGroupByDTOs(reportingListGroupByDTOs);
		// setting TotalRecords
		reportingListDTO.setTotalRecords(reportingSchedulesStatusDTOs.size());
		// Calculate && setting totals
		Long totalInstalmentAmount = 0L;
		Long totalInstalmentPrincipal = 0L;
		Long totalInstalmentInterest = 0L;
		Long totalInstalPrincipalPaid = 0L;
		Long totalInstalInterestPaid = 0L;
		Long totalInstalTotalPaid = 0L;
		Long totalTotalPrincipalPaid = 0L;
		Long totalTotalInterestPaid = 0L;
		Long totalTotalPaidAmount = 0L;
		Long totalUnpaidPrincipal = 0L;
		Long totalUnpaidInterest = 0L;
		Long totalUnpaidAmount = 0L;
		Long totalRemainingPrincipal = 0L;
		Long totalRemaininginterest = 0L;
		Long totalRemainingAmount = 0L;
		for (ReportingSchedulesStatusDTO rss : reportingSchedulesStatusDTOs) {
			totalInstalmentAmount += rss.getInstalmentAmount();
			totalInstalmentPrincipal += rss.getInstalmentPrincipal();
			totalInstalmentInterest += rss.getInstalmentInterest();
			totalInstalPrincipalPaid += rss.getInstalmentPrincipalPaid();
			totalInstalInterestPaid += rss.getInstalmentinterestPaid();
			totalInstalTotalPaid += rss.getInstalmentTotalPaid();
			totalTotalPrincipalPaid += rss.getTotalPrincipalPaid();
			totalTotalInterestPaid += rss.getTotalInterestPaid();
			totalTotalPaidAmount += rss.getTotalPaidAmount();
			totalUnpaidPrincipal += rss.getUnpaidPrincipal();
			totalUnpaidInterest += rss.getUpaidInterest();
			totalUnpaidAmount += rss.getUpaidAmount();
			totalRemainingPrincipal += rss.getRemainingPrincipal();
			totalRemaininginterest += rss.getRemainingInterest();
			totalRemainingAmount += rss.getRemainingAmount();
		}
		reportingListDTO.setTotalInstalmentAmount(totalInstalmentAmount);
		reportingListDTO.setTotalInstalmentPrincipal(totalInstalmentPrincipal);
		reportingListDTO.setTotalInstalmentInterest(totalInstalmentInterest);
		reportingListDTO.setTotalInstalPrincipalPaid(totalInstalPrincipalPaid);
		reportingListDTO.setTotalInstalInterestPaid(totalInstalInterestPaid);
		reportingListDTO.setTotalInstalTotalPaid(totalInstalTotalPaid);
		reportingListDTO.setTotalTotalPrincipalPaid(totalTotalPrincipalPaid);
		reportingListDTO.setTotalTotalInterestPaid(totalTotalInterestPaid);
		reportingListDTO.setTotalTotalPaidAmount(totalTotalPaidAmount);
		reportingListDTO.setTotalUnpaidPrincipal(totalUnpaidPrincipal);
		reportingListDTO.setTotalUnpaidInterest(totalUnpaidInterest);
		reportingListDTO.setTotalUnpaidAmount(totalUnpaidAmount);
		reportingListDTO.setTotalRemainingPrincipal(totalRemainingPrincipal);
		reportingListDTO.setTotalRemaininginterest(totalRemaininginterest);
		reportingListDTO.setTotalRemainingAmount(totalRemainingAmount);
		return reportingListDTO;
	}

	/**
	 * Builds the report data group by loan officer.
	 * 
	 * @author HaythemBenizid
	 * @param reportingSchedulesStatusDTOs the reportingSchedulesStatus DT os
	 * @param reportingListDTO the reporting list DTO
	 * @return the reporting list DTO
	 */
	private ReportingListDTO buildReportDataGroupByLoanOfficer(
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOs,
			ReportingListDTO reportingListDTO) {

		List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOListFiltered =
				reportingSchedulesStatusDTOs.stream()
						.filter(CommonFunctions
								.distinctByKey(ReportingSchedulesStatusDTO::getLoanOfficer))
						.collect(Collectors.toList());
		logger.debug("List LoanOfficer : {} ", reportingSchedulesStatusDTOListFiltered.size());
		List<ReportingListGroupByDTO> reportingListGroupByDTOs = new ArrayList<>();
		// init response list && calculate TOTAL
		for (ReportingSchedulesStatusDTO reportingSchedulesStatus : reportingSchedulesStatusDTOListFiltered) {
			Integer selectedPortfolio = reportingSchedulesStatus.getLoanOfficer();
			ReportingListGroupByDTO reportingListGroupByDTO = new ReportingListGroupByDTO();
			List<ReportingSchedulesStatusDTO> reportingSchedulesStatusDTOListFilteredByPortfolio =
					reportingSchedulesStatusDTOs.stream()
							.filter(l -> l.getLoanOfficer().equals(selectedPortfolio))
							.collect(Collectors.toList());
			logger.debug("reportingSchedulesStatusDTOListFilteredByLoan officer by {} = {} ",
					selectedPortfolio, reportingSchedulesStatusDTOListFilteredByPortfolio.size());
			reportingListGroupByDTO.setReportingSchedulesStatusDTOs(
					reportingSchedulesStatusDTOListFilteredByPortfolio);
			// setting TotalRecords
			reportingListGroupByDTO
					.setTotalRecords(reportingSchedulesStatusDTOListFilteredByPortfolio.size());
			// Calculate && setting totals
			Long totalInstalmentAmount = 0L;
			Long totalInstalmentPrincipal = 0L;
			Long totalInstalmentInterest = 0L;
			Long totalInstalPrincipalPaid = 0L;
			Long totalInstalInterestPaid = 0L;
			Long totalInstalTotalPaid = 0L;
			Long totalTotalPrincipalPaid = 0L;
			Long totalTotalInterestPaid = 0L;
			Long totalTotalPaidAmount = 0L;
			Long totalUnpaidPrincipal = 0L;
			Long totalUnpaidInterest = 0L;
			Long totalUnpaidAmount = 0L;
			Long totalRemainingPrincipal = 0L;
			Long totalRemaininginterest = 0L;
			Long totalRemainingAmount = 0L;
			for (ReportingSchedulesStatusDTO rss : reportingSchedulesStatusDTOListFilteredByPortfolio) {
				totalInstalmentAmount += rss.getInstalmentAmount();
				totalInstalmentPrincipal += rss.getInstalmentPrincipal();
				totalInstalmentInterest += rss.getInstalmentInterest();
				totalInstalPrincipalPaid += rss.getInstalmentPrincipalPaid();
				totalInstalInterestPaid += rss.getInstalmentinterestPaid();
				totalInstalTotalPaid += rss.getInstalmentTotalPaid();
				totalTotalPrincipalPaid += rss.getTotalPrincipalPaid();
				totalTotalInterestPaid += rss.getTotalInterestPaid();
				totalTotalPaidAmount += rss.getTotalPaidAmount();
				totalUnpaidPrincipal += rss.getUnpaidPrincipal();
				totalUnpaidInterest += rss.getUpaidInterest();
				totalUnpaidAmount += rss.getUpaidAmount();
				totalRemainingPrincipal += rss.getRemainingPrincipal();
				totalRemaininginterest += rss.getRemainingInterest();
				totalRemainingAmount += rss.getRemainingAmount();
			}
			reportingListGroupByDTO.setTotalInstalmentAmount(totalInstalmentAmount);
			reportingListGroupByDTO.setTotalInstalmentPrincipal(totalInstalmentPrincipal);
			reportingListGroupByDTO.setTotalInstalmentInterest(totalInstalmentInterest);
			reportingListGroupByDTO.setTotalInstalPrincipalPaid(totalInstalPrincipalPaid);
			reportingListGroupByDTO.setTotalInstalInterestPaid(totalInstalInterestPaid);
			reportingListGroupByDTO.setTotalInstalTotalPaid(totalInstalTotalPaid);
			reportingListGroupByDTO.setTotalTotalPrincipalPaid(totalTotalPrincipalPaid);
			reportingListGroupByDTO.setTotalTotalInterestPaid(totalTotalInterestPaid);
			reportingListGroupByDTO.setTotalTotalPaidAmount(totalTotalPaidAmount);
			reportingListGroupByDTO.setTotalUnpaidPrincipal(totalUnpaidPrincipal);
			reportingListGroupByDTO.setTotalUnpaidInterest(totalUnpaidInterest);
			reportingListGroupByDTO.setTotalUnpaidAmount(totalUnpaidAmount);
			reportingListGroupByDTO.setTotalRemainingPrincipal(totalRemainingPrincipal);
			reportingListGroupByDTO.setTotalRemaininginterest(totalRemaininginterest);
			reportingListGroupByDTO.setTotalRemainingAmount(totalRemainingAmount);
			reportingListGroupByDTOs.add(reportingListGroupByDTO);
		}
		// init response object
		reportingListDTO.setReportingListGroupByDTOs(reportingListGroupByDTOs);
		// setting TotalRecords
		reportingListDTO.setTotalRecords(reportingSchedulesStatusDTOs.size());
		// Calculate && setting totals
		Long totalInstalmentAmount = 0L;
		Long totalInstalmentPrincipal = 0L;
		Long totalInstalmentInterest = 0L;
		Long totalInstalPrincipalPaid = 0L;
		Long totalInstalInterestPaid = 0L;
		Long totalInstalTotalPaid = 0L;
		Long totalTotalPrincipalPaid = 0L;
		Long totalTotalInterestPaid = 0L;
		Long totalTotalPaidAmount = 0L;
		Long totalUnpaidPrincipal = 0L;
		Long totalUnpaidInterest = 0L;
		Long totalUnpaidAmount = 0L;
		Long totalRemainingPrincipal = 0L;
		Long totalRemaininginterest = 0L;
		Long totalRemainingAmount = 0L;
		for (ReportingSchedulesStatusDTO rss : reportingSchedulesStatusDTOs) {
			totalInstalmentAmount += rss.getInstalmentAmount();
			totalInstalmentPrincipal += rss.getInstalmentPrincipal();
			totalInstalmentInterest += rss.getInstalmentInterest();
			totalInstalPrincipalPaid += rss.getInstalmentPrincipalPaid();
			totalInstalInterestPaid += rss.getInstalmentinterestPaid();
			totalInstalTotalPaid += rss.getInstalmentTotalPaid();
			totalTotalPrincipalPaid += rss.getTotalPrincipalPaid();
			totalTotalInterestPaid += rss.getTotalInterestPaid();
			totalTotalPaidAmount += rss.getTotalPaidAmount();
			totalUnpaidPrincipal += rss.getUnpaidPrincipal();
			totalUnpaidInterest += rss.getUpaidInterest();
			totalUnpaidAmount += rss.getUpaidAmount();
			totalRemainingPrincipal += rss.getRemainingPrincipal();
			totalRemaininginterest += rss.getRemainingInterest();
			totalRemainingAmount += rss.getRemainingAmount();
		}
		reportingListDTO.setTotalInstalmentAmount(totalInstalmentAmount);
		reportingListDTO.setTotalInstalmentPrincipal(totalInstalmentPrincipal);
		reportingListDTO.setTotalInstalmentInterest(totalInstalmentInterest);
		reportingListDTO.setTotalInstalPrincipalPaid(totalInstalPrincipalPaid);
		reportingListDTO.setTotalInstalInterestPaid(totalInstalInterestPaid);
		reportingListDTO.setTotalInstalTotalPaid(totalInstalTotalPaid);
		reportingListDTO.setTotalTotalPrincipalPaid(totalTotalPrincipalPaid);
		reportingListDTO.setTotalTotalInterestPaid(totalTotalInterestPaid);
		reportingListDTO.setTotalTotalPaidAmount(totalTotalPaidAmount);
		reportingListDTO.setTotalUnpaidPrincipal(totalUnpaidPrincipal);
		reportingListDTO.setTotalUnpaidInterest(totalUnpaidInterest);
		reportingListDTO.setTotalUnpaidAmount(totalUnpaidAmount);
		reportingListDTO.setTotalRemainingPrincipal(totalRemainingPrincipal);
		reportingListDTO.setTotalRemaininginterest(totalRemaininginterest);
		reportingListDTO.setTotalRemainingAmount(totalRemainingAmount);
		return reportingListDTO;
	}

	/**
	 * Generate Excel file => return byte[] array.
	 *
	 * @author HaythemBenizid
	 * @param reportingListDTO the reporting list DTO
	 * @param reportingDTO the reporting DTO
	 * @return the byte array input stream
	 */
	private byte[] generateLoanApplicationReport(ReportingListDTO reportingListDTO,
			ReportingDTO reportingDTO) {

		// The sheet name
		final String SHEET = "loan-application";

		// The AR HEADErs
		final String[] AR_HEADERS = {"الفرع", "الأخصائي", "رقم الطلب", "اسم العميل", "رقم المجموعة",
				"أسم المجموعة", "نوع القرض", "عدد الأقساط", "قيمة القسط", "تاريخ بداية السداد",
				"قيمة القرض المطلوب", "قيمة القرض المصروفة", "تاريخ الموافقة", "تاريخ صرف القرض",
				"حالة القرض", "دور العضو في المجموعة"};

		// The EN HEADErs
		@SuppressWarnings("unused")
		final String[] EN_HEADERS = {"Branch Name", "Loan Officer", "Loan Account Number",
				"Client Name", "Groupe N°", "Group Name", "Product", "Loan Term",
				"Repayment Amount", "First repayment date", "Applied Amount", "Issue Amount",
				"Last Approval Level Date", "Issue date", "Loan status", "Member Role"};

		// The Constant REQUEST_PARAMS
		final String[] REQUEST_PARAMS = {"المنتج", "الفرع", "الأخصائي", "مبلغ القرض",
				"تاريخ طلب القرض", "تاريخ إصدار القرض", "حالة القرض", "رقم العميل"};

		// The sheet title
		final String SHEET_TITLE = "تقرير حالة القرض";

		logger.info("START generating excel report");
		// init instant datetime
		DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
		String printedOn = dateFormat.format(new Date());
		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet = workbook.createSheet(SHEET);
			int rowIdx = 1;
			try {
				// get photo client from DB
				byte[] photoClientByte = gedClient.findPhotoClient(0L);
				// ============= Inserting image - START
				if (!ACMValidationUtils.isNullOrEmpty(photoClientByte)) {
					/* Add Picture to Workbook, Specify picture type as PNG and Get an Index */
					int logoId = workbook.addPicture(photoClientByte, Workbook.PICTURE_TYPE_PNG);

					/* Create the drawing container */
					XSSFDrawing drawing = (XSSFDrawing) sheet.createDrawingPatriarch();
					/* Create an anchor point */
					// ============= Inserting image - END
					// ========adding image START
					XSSFClientAnchor anchor = new XSSFClientAnchor();
					/* Define top left corner, and we can resize picture suitable from there */
					anchor.setCol1(0); // Column B
					anchor.setRow1(rowIdx - 1); // Row 3
					anchor.setCol2(1); // Column C
					anchor.setRow2(rowIdx); // Row 4
					/* Invoke createPicture and pass the anchor point and ID */
					XSSFPicture myPicture = drawing.createPicture(anchor, logoId);
					// Reset the image to the original size
					myPicture.resize();
					// ========adding image END
				}
			}
			catch (Exception e) {
				logger.error("Failed to get Log from GED");
				e.printStackTrace();
			}
			String SHEET_CLIENT_NAME = "الأهلي تمكين";
			AcmEnvironnementDTO acmEnvironnementDTO = parametrageClient.find("REPORT_CLIENT_NAME");
			if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO)) {
				SHEET_CLIENT_NAME = acmEnvironnementDTO.getValue();
			}
			rowIdx++;
			// create border style for cells
			CellStyle styleBorder = workbook.createCellStyle();
			styleBorder.setBorderBottom(BorderStyle.THIN);
			styleBorder.setBorderLeft(BorderStyle.THIN);
			styleBorder.setBorderRight(BorderStyle.THIN);
			styleBorder.setBorderTop(BorderStyle.THIN);

			// Create a Font for styling Title cells
			Font titleFont = workbook.createFont();
			titleFont.setBold(true);
			titleFont.setFontHeightInPoints((short) 16);
			titleFont.setColor(IndexedColors.BLUE_GREY.index);
			// Create a titleCellStyle with the font
			CellStyle titleCellStyle = workbook.createCellStyle();
			titleCellStyle.setFont(titleFont);
			titleCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// will merge from
			CellRangeAddress regionTitle = new CellRangeAddress(rowIdx, rowIdx, 1, 6);
			sheet.addMergedRegion(regionTitle);
			Row titleClientRow = sheet.createRow(rowIdx);
			Cell cellTitleClient = titleClientRow.createCell(1);
			cellTitleClient.setCellValue(SHEET_CLIENT_NAME);
			cellTitleClient.setCellStyle(titleCellStyle);

			CellRangeAddress regionPrintedOn = new CellRangeAddress(rowIdx, rowIdx, 8, 10);
			sheet.addMergedRegion(regionPrintedOn);
			Cell cellPrintedOn = titleClientRow.createCell(8);
			cellPrintedOn.setCellValue(SHEET_PRINTED_ON + printedOn);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionPrintedOn, sheet);

			rowIdx++;
			// will merge from
			CellRangeAddress regionTitleReport = new CellRangeAddress(rowIdx, rowIdx, 1, 6);
			sheet.addMergedRegion(regionTitleReport);
			Row titleReportRow = sheet.createRow(rowIdx);
			Cell cellTitleReport = titleReportRow.createCell(1);
			cellTitleReport.setCellValue(SHEET_TITLE);
			cellTitleReport.setCellStyle(titleCellStyle);

			// step 3 row
			rowIdx = rowIdx + 3;

			// Create style
			CellStyle styleBold = workbook.createCellStyle();
			// Create font
			Font font = workbook.createFont();
			// Make font bold
			font.setBold(true);
			// set it to bold
			styleBold.setFont(font);
			// set border
			styleBold.setBorderBottom(BorderStyle.THIN);
			styleBold.setBorderLeft(BorderStyle.THIN);
			styleBold.setBorderRight(BorderStyle.THIN);
			styleBold.setBorderTop(BorderStyle.THIN);

			// REQUEST_PARAMS_TITEL row
			Row requestParamTitleRow = sheet.createRow(rowIdx++);
			Cell cell = requestParamTitleRow.createCell(3);
			cell.setCellValue(REQUEST_PARAMS_TITEL);
			cell.setCellStyle(styleBold);

			// requestParamRow
			String productsValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getProductDTOs())) {
				for (ProductDTO productDTO : reportingDTO.getProductDTOs()) {
					productsValues = productsValues + " ; " + productDTO.getDescription();
				}
			}
			String branchsValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getBrancheDTOs())) {
				for (BrancheDTO brancheDTO : reportingDTO.getBrancheDTOs()) {
					branchsValues = branchsValues + " ; " + brancheDTO.getDescription();
				}
			}

			String usersValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getUserDTOs())) {
				for (UserDTO userDTO : reportingDTO.getUserDTOs()) {
					usersValues = usersValues + " ; " + userDTO.getSimpleName();
				}
			}
			String min = "Min = ";
			String max = " - Max= ";

			// amount
			String amountMin = reportingDTO.getLoanAmountMin() != null
					? reportingDTO.getLoanAmountMin().toPlainString()
					: "";
			String amountMax = reportingDTO.getLoanAmountMax() != null
					? reportingDTO.getLoanAmountMax().toPlainString()
					: "";
			// loan create date
			String dateLoanCreateMin =
					reportingDTO.getLoanCreateDateMin() != null ? DateUtil.formatDate(
							reportingDTO.getLoanCreateDateMin(), CommonConstants.PATTREN_DATE) : "";
			String dateLoanCreateMax =
					reportingDTO.getLoanCreateDateMax() != null ? DateUtil.formatDate(
							reportingDTO.getLoanCreateDateMax(), CommonConstants.PATTREN_DATE) : "";
			String creationDateMinMax = min + dateLoanCreateMin + max + dateLoanCreateMax;

			// issueDate
			String dateLoanIssueMin =
					reportingDTO.getLoanIssueDateMin() != null ? DateUtil.formatDate(
							reportingDTO.getLoanIssueDateMin(), CommonConstants.PATTREN_DATE) : "";
			String dateLoanIssueMax =
					reportingDTO.getLoanIssueDateMax() != null ? DateUtil.formatDate(
							reportingDTO.getLoanIssueDateMax(), CommonConstants.PATTREN_DATE) : "";
			String issueDateMinMax = min + dateLoanIssueMin + max + dateLoanIssueMax;

			String status = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanStatus())) {
				for (AcmStatutsDTO acmStatutsDTO : reportingDTO.getLoanStatus()) {
					status = status + " ; " + acmStatutsDTO.getValue();
				}
			}
			String[] paramsValues = {productsValues, branchsValues, usersValues,
					min + amountMin + max + amountMax, creationDateMinMax, issueDateMinMax, status,
					reportingDTO.getCustomerNumber()};

			// requestParamRow
			for (int col = 0; col < REQUEST_PARAMS.length; col++) {
				Row requestParamRow = sheet.createRow(rowIdx++);
				Cell cell1 = requestParamRow.createCell(3);
				cell1.setCellValue(REQUEST_PARAMS[col]);
				cell1.setCellStyle(styleBold);

				CellRangeAddress region = new CellRangeAddress(rowIdx - 1, rowIdx - 1, 4, 6);
				sheet.addMergedRegion(region);
				Cell cell2 = requestParamRow.createCell(4);
				cell2.setCellValue(paramsValues[col]);
				RegionUtil.setBorderBottom(BorderStyle.THIN, region, sheet);
				RegionUtil.setBorderTop(BorderStyle.THIN, region, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, region, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, region, sheet);
			}

			rowIdx++;
			// group by type
			Row requestParamRow = sheet.createRow(rowIdx++);
			Cell cell1 = requestParamRow.createCell(3);
			cell1.setCellValue("Group By");
			cell1.setCellStyle(styleBold);

			CellRangeAddress region = new CellRangeAddress(rowIdx - 1, rowIdx - 1, 4, 6);
			sheet.addMergedRegion(region);
			Cell cell2 = requestParamRow.createCell(4);
			String groupBy = "Product";
			if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
				groupBy = "Branch";
			}
			else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
				groupBy = "Loan Officer";
			}
			cell2.setCellValue(groupBy);
			RegionUtil.setBorderBottom(BorderStyle.THIN, region, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, region, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, region, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, region, sheet);

			// type report
			Row requestTypeRow = sheet.createRow(rowIdx++);
			Cell cellType = requestTypeRow.createCell(3);
			cellType.setCellValue("Report Type");
			cellType.setCellStyle(styleBold);
			CellRangeAddress regionType = new CellRangeAddress(rowIdx - 1, rowIdx - 1, 4, 6);
			sheet.addMergedRegion(regionType);
			Cell cellTypeValue = requestTypeRow.createCell(4);
			String type = "Summary";
			if (Boolean.TRUE.equals(reportingDTO.getDetails())) {
				type = "Details";
			}
			cellTypeValue.setCellValue(type);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionType, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionType, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionType, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionType, sheet);

			// step 3 row
			rowIdx = rowIdx + 3;

			// Create a Font for styling header cells
			Font headerFont = workbook.createFont();
			headerFont.setBold(true);
			headerFont.setFontHeightInPoints((short) 12);
			headerFont.setColor(IndexedColors.WHITE.index);

			// Create a header CellStyle with the font
			CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);
			headerCellStyle.setFillBackgroundColor(IndexedColors.GREEN.index);
			headerCellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
			headerCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// Header
			Row headerRow = sheet.createRow(rowIdx++);
			for (int col = 0; col < AR_HEADERS.length; col++) {
				Cell cellHeaderRow = headerRow.createCell(col);
				cellHeaderRow.setCellValue(AR_HEADERS[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}

			/*
			 * CreationHelper helps us create instances of various things like DataFormat,
			 * Hyperlink, RichTextString etc, in a format (HSSF, XSSF) independent way
			 */
			CreationHelper createHelper = workbook.getCreationHelper();

			// Create a Font for styling Totals cells
			Font totalFont = workbook.createFont();
			totalFont.setBold(true);
			totalFont.setColor(IndexedColors.DARK_BLUE.index);
			// Create a CellStyle with the font
			CellStyle totalCellStyle = workbook.createCellStyle();
			totalCellStyle.setFont(totalFont);
			totalCellStyle.setAlignment(HorizontalAlignment.RIGHT);
			totalCellStyle.setDataFormat(createHelper.createDataFormat().getFormat("#,##0.000"));

			// Create Cell Style for formatting Date
			CellStyle dateCellStyle = workbook.createCellStyle();
			dateCellStyle.setDataFormat(
					createHelper.createDataFormat().getFormat(CommonConstants.PATTREN_DATE));
			dateCellStyle.setAlignment(HorizontalAlignment.CENTER);
			// create cell style for number
			CellStyle numberCellStyle = workbook.createCellStyle();
			numberCellStyle.setDataFormat(createHelper.createDataFormat().getFormat("#,##0.000"));
			numberCellStyle.setAlignment(HorizontalAlignment.CENTER);

			for (ReportingListGroupByDTO reportingListGroupByDTO : reportingListDTO
					.getReportingListGroupByDTOs()) {
				if (Boolean.TRUE.equals(reportingDTO.getSummary())) {
					Row row = sheet.createRow(rowIdx++);
					if (Boolean.TRUE.equals(reportingDTO.getProduct())) {
						row.createCell(6).setCellValue(reportingListGroupByDTO.getLoanDTOs().get(0)
								.getProductDescription());
					}
					else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
						row.createCell(1).setCellValue(reportingListGroupByDTO.getLoanDTOs().get(0)
								.getPortfolioDescription());
					}
					else if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
						row.createCell(0).setCellValue(
								reportingListGroupByDTO.getLoanDTOs().get(0).getBranchName());
					}
					else {
						// default by product
						row.createCell(6).setCellValue(reportingListGroupByDTO.getLoanDTOs().get(0)
								.getProductDescription());
					}
				}
				else {
					for (LoanDTO loanDTO : reportingListGroupByDTO.getLoanDTOs()) {
						Row row = sheet.createRow(rowIdx++);
						// BranchName
						row.createCell(0).setCellValue(loanDTO.getBranchName());
						// loan officer
						row.createCell(1).setCellValue(loanDTO.getPortfolioDescription());
						// loan account number
						row.createCell(2).setCellValue(loanDTO.getAccountNumber());
						// customer full name
						String fullName =
								loanDTO.getCustomerDTO().getCustomerName().replace("|", " ");
						row.createCell(3).setCellValue(fullName);
						// groupe number
						row.createCell(4).setCellValue(loanDTO.getCustomerGroupeNumber());

						// Group Name
						row.createCell(5)
								.setCellValue(loanDTO.getCustomerDTO().getSolidarityName());
						// product name
						row.createCell(6).setCellValue(loanDTO.getProductDescription());
						// loan term
						row.createCell(7).setCellValue(loanDTO.getTermPeriodNum());
						// repayment amount
						if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getNormalPayment())) {
							row.createCell(8).setCellValue(loanDTO.getNormalPayment());
						}
						else {
							row.createCell(8).setCellValue("");
						}
						// First repayment date
						if (loanDTO.getInitialPaymentDate() != null) {
							Cell cellInitialPaymentDate = row.createCell(9);
							cellInitialPaymentDate.setCellValue(loanDTO.getInitialPaymentDate());
							cellInitialPaymentDate.setCellStyle(dateCellStyle);
						}
						else {
							row.createCell(9).setCellValue("");
						}

						// applied amount
						Cell cellApplyAmountTotal = row.createCell(10);
						cellApplyAmountTotal
								.setCellValue(loanDTO.getApplyAmountTotal().doubleValue());
						cellApplyAmountTotal.setCellStyle(numberCellStyle);

						// issue amount
						Cell cellApprovelAmount = row.createCell(11);
						cellApprovelAmount.setCellValue(loanDTO.getApprovelAmount().doubleValue());
						cellApprovelAmount.setCellStyle(numberCellStyle);

						// Last Approval Level Date
						if (loanDTO.getLastApprovalDate() != null) {
							Cell cellLastApprovalDate = row.createCell(12);
							cellLastApprovalDate.setCellValue(loanDTO.getLastApprovalDate());
							cellLastApprovalDate.setCellStyle(dateCellStyle);
						}
						else {
							row.createCell(12).setCellValue("");
						}

						// Issue date
						if (loanDTO.getIssueDate() != null) {
							Cell cellIssueDate = row.createCell(13);
							cellIssueDate.setCellValue(loanDTO.getIssueDate());
							cellIssueDate.setCellStyle(dateCellStyle);
						}
						else {
							row.createCell(13).setCellValue("");
						}
						// status loan
						row.createCell(14).setCellValue(loanDTO.getStatutLibelle());

						// Member Role
						row.createCell(15).setCellValue(loanDTO.getCustomerRole());
					}
				}
				// init total rows
				sheet.addMergedRegion(new CellRangeAddress(rowIdx, rowIdx, 0, 9));
				Row row = sheet.createRow(rowIdx++);
				Cell cell9 = row.createCell(0);
				cell9.setCellValue(
						"Total Number of Records : " + reportingListGroupByDTO.getTotalRecords());
				cell9.setCellStyle(totalCellStyle);

				Cell cell10 = row.createCell(10);
				cell10.setCellValue(reportingListGroupByDTO.getTotalAmount());
				cell10.setCellStyle(totalCellStyle);

				Cell cell11 = row.createCell(11);
				cell11.setCellValue(reportingListGroupByDTO.getTotalIssueAmount());
				cell11.setCellStyle(totalCellStyle);
			}
			// init grand total rows
			sheet.addMergedRegion(new CellRangeAddress(rowIdx, rowIdx, 0, 9));
			Row row = sheet.createRow(rowIdx++);
			Cell cell9 = row.createCell(0);
			cell9.setCellValue(
					"Grand Total Number of Records : " + reportingListDTO.getTotalRecords());
			cell9.setCellStyle(totalCellStyle);

			Cell cell10 = row.createCell(10);
			cell10.setCellValue(reportingListDTO.getTotalAmount());
			cell10.setCellStyle(totalCellStyle);

			Cell cell11 = row.createCell(11);
			cell11.setCellValue(reportingListDTO.getTotalIssueAmount());
			cell11.setCellStyle(totalCellStyle);

			// Setting Auto Column Width
			for (int i = 0; i < AR_HEADERS.length; i++) {
				sheet.autoSizeColumn(i);
				sheet.setColumnWidth(i, sheet.getColumnWidth(i) * 10 / 10);
			}

			// generate file
			workbook.write(out);

			// create && save file in disk
			// try (FileOutputStream outputStream =
			// new FileOutputStream("C:\\workspacetalys\\" + SHEET + ".xlsx")) {
			// workbook.write(outputStream);
			// }
			workbook.close();
			logger.info("Generating excel report : {} :: DONE", SHEET);
			return out.toByteArray();
		}
		catch (IOException e) {
			throw new RuntimeException("fail to import data to Excel file: " + e.getMessage());
		}
	}

	/**
	 * Generate collection followup report.
	 * 
	 * @author HaythemBenizid
	 * @param reportingListDTO the reporting list DTO
	 * @param reportingDTO the reporting DTO
	 * @return the byte[]
	 */
	private byte[] generateCollectionFollowupReport(ReportingListDTO reportingListDTO,
			ReportingDTO reportingDTO) {

		// The sheet name
		final String SHEET = "CollectionFollowupReport";

		// The AR HEADErs
		final String[] AR_HEADERS = {"جهة التمويل", "الفرع", "الأخصائي", "رقم المجموعة",
				"اسم المجموعة", "رقم العميل", "اسم العميل", "رقم القرض", "المنتج", "تاريخ الدفعة",
				"القسط الشهري", "الاصل", "الرسوم", "تحصيل اصل الدفعة", "تحصيل رسوم الدفعة",
				"تحصيل الدفعة", "تحصيل اصل القرض", "تحصيل رسوم القرض", "تحصيل القرض",
				"الاصل المتأخر", "الرسوم المتأخرة", "المبلغ المتأخر", "عدد أيام التأخير",
				"عدد الاقساط المتأخرة", "الاصل المتبقي", "الرسوم المتبقية", "المبلغ المتبقي",
				"تاريخ صرف القرض"};
		// The EN HEADErs
		@SuppressWarnings("unused")
		final String[] EN_HEADERS = {"Source of Fund", "Branch Name", "Loan Officer", "Groupe N°",
				"Client N°", "Client Name", "Account N°", "Product Name", "Instalment Date",
				"Instalment Amount", "Instalment Principal", "Instalment Interest",
				"Instal. Principal Paid", "Instal. Interest Paid", "Instal. Total Paid",
				"Total Principal Paid", "Total Interest Paid", "Total Paid Amount",
				"Unpaid Principal", "Unpaid Interest", "Unpaid Amount", "Late Days",
				"NB. unpaid Instalment", "Remaining Principal", "Remaining interest",
				"Remaining Amount", "Issue date"};

		// The sheet title
		final String SHEET_TITLE = "تقرير متابعة التحصيل";

		logger.info("START generating excel report");
		// init instant datetime
		DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
		String printedOn = dateFormat.format(new Date());
		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet = workbook.createSheet(SHEET);
			int rowIdx = 1;
			// if exist in ged
			try {
				// get photo client from DB
				byte[] photoClientByte = gedClient.findPhotoClient(0L);
				// ============= Inserting image - START
				if (!ACMValidationUtils.isNullOrEmpty(photoClientByte)) {
					/* Add Picture to Workbook, Specify picture type as PNG and Get an Index */
					int logoId = workbook.addPicture(photoClientByte, Workbook.PICTURE_TYPE_PNG);

					/* Create the drawing container */
					XSSFDrawing drawing = (XSSFDrawing) sheet.createDrawingPatriarch();
					/* Create an anchor point */
					// ============= Inserting image - END
					// ========adding image START
					XSSFClientAnchor anchor = new XSSFClientAnchor();
					/* Define top left corner, and we can resize picture suitable from there */
					anchor.setCol1(0); // Column B
					anchor.setRow1(rowIdx - 1); // Row 3
					anchor.setCol2(1); // Column C
					anchor.setRow2(rowIdx); // Row 4
					/* Invoke createPicture and pass the anchor point and ID */
					XSSFPicture myPicture = drawing.createPicture(anchor, logoId);
					// Reset the image to the original size
					myPicture.resize();
					// ========adding image END
				}
			}
			catch (Exception e) {
				logger.error("Failed to get Log from GED");
				e.printStackTrace();
			}
			rowIdx++;
			String SHEET_CLIENT_NAME = "الأهلي تمكين";
			AcmEnvironnementDTO acmEnvironnementDTO = parametrageClient.find("REPORT_CLIENT_NAME");
			if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO)) {
				SHEET_CLIENT_NAME = acmEnvironnementDTO.getValue();
			}
			// create border style for cells
			CellStyle styleBorder = workbook.createCellStyle();
			styleBorder.setBorderBottom(BorderStyle.THIN);
			styleBorder.setBorderLeft(BorderStyle.THIN);
			styleBorder.setBorderRight(BorderStyle.THIN);
			styleBorder.setBorderTop(BorderStyle.THIN);

			// Create a Font for styling Title cells
			Font titleFont = workbook.createFont();
			titleFont.setBold(true);
			titleFont.setFontHeightInPoints((short) 16);
			titleFont.setColor(IndexedColors.BLUE_GREY.index);
			// Create a titleCellStyle with the font
			CellStyle titleCellStyle = workbook.createCellStyle();
			titleCellStyle.setFont(titleFont);
			titleCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// will merge from
			CellRangeAddress regionTitle = new CellRangeAddress(rowIdx, rowIdx, 1, 6);
			sheet.addMergedRegion(regionTitle);
			Row titleClientRow = sheet.createRow(rowIdx);
			Cell cellTitleClient = titleClientRow.createCell(1);
			cellTitleClient.setCellValue(SHEET_CLIENT_NAME);
			cellTitleClient.setCellStyle(titleCellStyle);

			CellRangeAddress regionPrintedOn = new CellRangeAddress(rowIdx, rowIdx, 8, 10);
			sheet.addMergedRegion(regionPrintedOn);
			Cell cellPrintedOn = titleClientRow.createCell(8);
			cellPrintedOn.setCellValue(SHEET_PRINTED_ON + printedOn);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionPrintedOn, sheet);

			rowIdx++;
			// will merge from
			CellRangeAddress regionTitleReport = new CellRangeAddress(rowIdx, rowIdx, 1, 6);
			sheet.addMergedRegion(regionTitleReport);
			Row titleReportRow = sheet.createRow(rowIdx);
			Cell cellTitleReport = titleReportRow.createCell(1);
			cellTitleReport.setCellValue(SHEET_TITLE);
			cellTitleReport.setCellStyle(titleCellStyle);

			// step 3 row
			rowIdx = rowIdx + 3;

			// Create style
			CellStyle styleBold = workbook.createCellStyle();
			// Create font
			Font font = workbook.createFont();
			// Make font bold
			font.setBold(true);
			// set it to bold
			styleBold.setFont(font);
			// set border
			styleBold.setBorderBottom(BorderStyle.THIN);
			styleBold.setBorderLeft(BorderStyle.THIN);
			styleBold.setBorderRight(BorderStyle.THIN);
			styleBold.setBorderTop(BorderStyle.THIN);

			// REQUEST_PARAMS_TITEL row
			Row requestParamTitleRow = sheet.createRow(rowIdx++);
			Cell cell = requestParamTitleRow.createCell(3);
			cell.setCellValue(REQUEST_PARAMS_TITEL);
			cell.setCellStyle(styleBold);

			// requestParamRow
			String productsValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getProductDTOs())) {
				for (ProductDTO productDTO : reportingDTO.getProductDTOs()) {
					productsValues = productsValues + " ; " + productDTO.getDescription();
				}
			}
			String branchsValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getBrancheDTOs())) {
				for (BrancheDTO brancheDTO : reportingDTO.getBrancheDTOs()) {
					branchsValues = branchsValues + " ; " + brancheDTO.getDescription();
				}
			}

			String usersValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getUserDTOs())) {
				for (UserDTO userDTO : reportingDTO.getUserDTOs()) {
					usersValues = usersValues + " ; " + userDTO.getSimpleName();
				}
			}

			String sourceOfFundsValues = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanSourceOfFundsDTOs())) {
				for (LoanSourceOfFundsDTO loanSourceOfFundsDTO : reportingDTO
						.getLoanSourceOfFundsDTOs()) {
					sourceOfFundsValues =
							sourceOfFundsValues + " ; " + loanSourceOfFundsDTO.getCode();
				}
			}

			String min = "Min = ";
			String max = " - Max= ";
			// loan Instalment date
			String dateInstalmentDateMin = reportingDTO.getInstalmentDateMin() != null
					? DateUtil.formatDate(reportingDTO.getInstalmentDateMin(),
							CommonConstants.PATTREN_DATE)
					: "01/01/" + DateUtil.getYearFromDate(new Date());
			String dateInstalmentDateMax = reportingDTO.getInstalmentDateMax() != null
					? DateUtil.formatDate(reportingDTO.getInstalmentDateMax(),
							CommonConstants.PATTREN_DATE)
					: "31/12/" + DateUtil.getYearFromDate(new Date());
			String instalmentDateMinMax = min + dateInstalmentDateMin + max + dateInstalmentDateMax;

			String status = "";
			if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanStatus())) {
				for (AcmStatutsDTO acmStatutsDTO : reportingDTO.getLoanStatus()) {
					status = status + " ; " + acmStatutsDTO.getValue();
				}
			}

			// The Constant REQUEST_PARAMS
			final String[] REQUEST_PARAMS = {"جهة التمويل", "الفرع", "الأخصائي", "المنتج",
					"تاريخ الإستحقاق", "حالة القرض", "رقم العميل", "رقم المجموعة"};

			// The Constant Values_PARAMS
			String[] paramsValues = {sourceOfFundsValues, branchsValues, usersValues,
					productsValues, instalmentDateMinMax, status, reportingDTO.getCustomerNumber(),
					reportingDTO.getGroupNumber()};

			// requestParamRow
			for (int col = 0; col < REQUEST_PARAMS.length; col++) {
				Row requestParamRow = sheet.createRow(rowIdx++);
				Cell cell1 = requestParamRow.createCell(3);
				cell1.setCellValue(REQUEST_PARAMS[col]);
				cell1.setCellStyle(styleBold);

				CellRangeAddress region = new CellRangeAddress(rowIdx - 1, rowIdx - 1, 4, 6);
				sheet.addMergedRegion(region);
				Cell cell2 = requestParamRow.createCell(4);
				cell2.setCellValue(paramsValues[col]);
				RegionUtil.setBorderBottom(BorderStyle.THIN, region, sheet);
				RegionUtil.setBorderTop(BorderStyle.THIN, region, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, region, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, region, sheet);
			}

			rowIdx++;
			// group by type
			Row requestParamRow = sheet.createRow(rowIdx++);
			Cell cell1 = requestParamRow.createCell(3);
			cell1.setCellValue("Group By");
			cell1.setCellStyle(styleBold);

			CellRangeAddress region = new CellRangeAddress(rowIdx - 1, rowIdx - 1, 4, 6);
			sheet.addMergedRegion(region);
			Cell cell2 = requestParamRow.createCell(4);
			String groupBy = "Product";
			if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
				groupBy = "Branch";
			}
			else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
				groupBy = "Loan Officer";
			}
			cell2.setCellValue(groupBy);
			RegionUtil.setBorderBottom(BorderStyle.THIN, region, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, region, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, region, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, region, sheet);

			// type report
			Row requestTypeRow = sheet.createRow(rowIdx++);
			Cell cellType = requestTypeRow.createCell(3);
			cellType.setCellValue("Report Type");
			cellType.setCellStyle(styleBold);
			CellRangeAddress regionType = new CellRangeAddress(rowIdx - 1, rowIdx - 1, 4, 6);
			sheet.addMergedRegion(regionType);
			Cell cellTypeValue = requestTypeRow.createCell(4);
			String type = "Summary";
			if (Boolean.TRUE.equals(reportingDTO.getDetails())) {
				type = "Details";
			}
			cellTypeValue.setCellValue(type);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionType, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionType, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionType, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionType, sheet);

			// step 3 row
			rowIdx = rowIdx + 3;

			// Create a Font for styling header cells
			Font headerFont = workbook.createFont();
			headerFont.setBold(true);
			headerFont.setFontHeightInPoints((short) 12);
			headerFont.setColor(IndexedColors.WHITE.index);

			// Create a header CellStyle with the font
			CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);
			headerCellStyle.setFillBackgroundColor(IndexedColors.GREEN.index);
			headerCellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
			headerCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// Header
			Row headerRow = sheet.createRow(rowIdx++);
			for (int col = 0; col < AR_HEADERS.length; col++) {
				Cell cellHeaderRow = headerRow.createCell(col);
				cellHeaderRow.setCellValue(AR_HEADERS[col]);
				cellHeaderRow.setCellStyle(headerCellStyle);
			}

			/*
			 * CreationHelper helps us create instances of various things like DataFormat,
			 * Hyperlink, RichTextString etc, in a format (HSSF, XSSF) independent way
			 */
			CreationHelper createHelper = workbook.getCreationHelper();

			// Create a Font for styling Totals cells
			Font totalFont = workbook.createFont();
			totalFont.setBold(true);
			totalFont.setColor(IndexedColors.DARK_BLUE.index);
			// Create a CellStyle with the font
			CellStyle totalCellStyle = workbook.createCellStyle();
			totalCellStyle.setFont(totalFont);
			totalCellStyle.setAlignment(HorizontalAlignment.RIGHT);
			totalCellStyle.setDataFormat(createHelper.createDataFormat().getFormat("#,##0.000"));

			// Create Cell Style for formatting Date
			CellStyle dateCellStyle = workbook.createCellStyle();
			dateCellStyle.setDataFormat(
					createHelper.createDataFormat().getFormat(CommonConstants.PATTREN_DATE));
			dateCellStyle.setAlignment(HorizontalAlignment.CENTER);
			// create cell style for number
			CellStyle numberCellStyle = workbook.createCellStyle();
			numberCellStyle.setDataFormat(createHelper.createDataFormat().getFormat("#,##0.000"));
			numberCellStyle.setAlignment(HorizontalAlignment.CENTER);

			for (ReportingListGroupByDTO reportingListGroupByDTO : reportingListDTO
					.getReportingListGroupByDTOs()) {
				if (Boolean.TRUE.equals(reportingDTO.getSummary())) {
					Row row = sheet.createRow(rowIdx++);
					if (Boolean.TRUE.equals(reportingDTO.getProduct())) {
						row.createCell(6).setCellValue(reportingListGroupByDTO.getLoanDTOs().get(0)
								.getProductDescription());
					}
					else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
						row.createCell(1).setCellValue(reportingListGroupByDTO.getLoanDTOs().get(0)
								.getPortfolioDescription());
					}
					else if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
						row.createCell(0).setCellValue(
								reportingListGroupByDTO.getLoanDTOs().get(0).getBranchName());
					}
					else {
						// default by product
						row.createCell(6).setCellValue(reportingListGroupByDTO.getLoanDTOs().get(0)
								.getProductDescription());
					}
				}
				else {
					for (ReportingSchedulesStatusDTO dto : reportingListGroupByDTO
							.getReportingSchedulesStatusDTOs()) {
						Row row = sheet.createRow(rowIdx++);
						// getSourceOffundsLabel
						row.createCell(0).setCellValue(dto.getSourceOffundsLabel());
						// getBrancheLabel
						row.createCell(1).setCellValue(dto.getBrancheLabel());
						// getLoanOfficerLabel
						row.createCell(2).setCellValue(dto.getLoanOfficerLabel());
						// getGroupNumber
						row.createCell(3).setCellValue(dto.getGroupNumber());
						// getGroupName
						row.createCell(4).setCellValue(dto.getGroupName());
						// getCustomerNumber
						row.createCell(5).setCellValue(dto.getCustomerNumber());
						// getCustomerName
						row.createCell(6).setCellValue(dto.getCustomerName());
						// getAccountNumber
						row.createCell(7).setCellValue(dto.getAccountNumber());
						// getProductIDLabel
						row.createCell(8).setCellValue(dto.getProductIDLabel());
						// getRepaymentDate
						if (dto.getRepaymentDate() != null) {
							Cell cellIssueDate = row.createCell(9);
							cellIssueDate.setCellValue(dto.getRepaymentDate());
							cellIssueDate.setCellStyle(dateCellStyle);
						}
						else {
							row.createCell(9).setCellValue("");
						}

						// InstalmentAmount
						Cell cellInstalmentAmount = row.createCell(10);
						cellInstalmentAmount.setCellValue(dto.getInstalmentAmount());
						cellInstalmentAmount.setCellStyle(numberCellStyle);
						// InstalmentPrincipal
						Cell cellInstalmentPrincipal = row.createCell(11);
						cellInstalmentPrincipal.setCellValue(dto.getInstalmentPrincipal());
						cellInstalmentPrincipal.setCellStyle(numberCellStyle);
						// InstalmentInterest
						Cell cellInstalmentInterest = row.createCell(12);
						cellInstalmentInterest.setCellValue(dto.getInstalmentInterest());
						cellInstalmentInterest.setCellStyle(numberCellStyle);
						// *************************//

						// InstalmentPrincipalPaid
						Cell cellInstalmentPrincipalPaid = row.createCell(13);
						cellInstalmentPrincipalPaid.setCellValue(dto.getInstalmentPrincipalPaid());
						cellInstalmentPrincipalPaid.setCellStyle(numberCellStyle);
						// InstalmentinterestPaid
						Cell cellInstalmentinterestPaid = row.createCell(14);
						cellInstalmentinterestPaid.setCellValue(dto.getInstalmentinterestPaid());
						cellInstalmentinterestPaid.setCellStyle(numberCellStyle);
						// InstalmentTotalPaid
						Cell cellInstalmentTotalPaid = row.createCell(15);
						cellInstalmentTotalPaid.setCellValue(dto.getInstalmentTotalPaid());
						cellInstalmentTotalPaid.setCellStyle(numberCellStyle);
						// *************************//

						// TotalPrincipalPaid
						Cell cellTotalPrincipalPaid = row.createCell(16);
						cellTotalPrincipalPaid.setCellValue(dto.getTotalPrincipalPaid());
						cellTotalPrincipalPaid.setCellStyle(numberCellStyle);
						// TotalInterestPaid
						Cell cellTotalInterestPaid = row.createCell(17);
						cellTotalInterestPaid.setCellValue(dto.getTotalInterestPaid());
						cellTotalInterestPaid.setCellStyle(numberCellStyle);
						// TotalPaidAmount
						Cell cellTotalPaidAmount = row.createCell(18);
						cellTotalPaidAmount.setCellValue(dto.getTotalPaidAmount());
						cellTotalPaidAmount.setCellStyle(numberCellStyle);
						// *************************//

						// UnpaidPrincipal
						Cell cellUnpaidPrincipal = row.createCell(19);
						cellUnpaidPrincipal.setCellValue(dto.getUnpaidPrincipal());
						cellUnpaidPrincipal.setCellStyle(numberCellStyle);
						// UpaidInterest
						Cell cellUpaidInterest = row.createCell(20);
						cellUpaidInterest.setCellValue(dto.getUpaidInterest());
						cellUpaidInterest.setCellStyle(numberCellStyle);
						// UpaidAmount
						Cell cellUpaidAmount = row.createCell(21);
						cellUpaidAmount.setCellValue(dto.getUpaidAmount());
						cellUpaidAmount.setCellStyle(numberCellStyle);
						// getLateDays
						row.createCell(22).setCellValue(dto.getLateDays());
						// getNbUnpaidinstalment
						row.createCell(23).setCellValue(dto.getNbUnpaidinstalment());
						// *************************//

						// RemainingPrincipal
						Cell cellRemainingPrincipal = row.createCell(24);
						cellRemainingPrincipal.setCellValue(dto.getRemainingPrincipal());
						cellRemainingPrincipal.setCellStyle(numberCellStyle);
						// RemainingInterest
						Cell cellRemainingInterest = row.createCell(25);
						cellRemainingInterest.setCellValue(dto.getRemainingInterest());
						cellRemainingInterest.setCellStyle(numberCellStyle);
						// RemainingAmount
						Cell cellRemainingAmount = row.createCell(26);
						cellRemainingAmount.setCellValue(dto.getRemainingAmount());
						cellRemainingAmount.setCellStyle(numberCellStyle);
						// *************************//

						// getIssueDate
						if (dto.getIssueDate() != null) {
							Cell cellIssueDate = row.createCell(27);
							cellIssueDate.setCellValue(dto.getIssueDate());
							cellIssueDate.setCellStyle(dateCellStyle);
						}
						else {
							row.createCell(27).setCellValue("");
						}
					}
				}
				// init total rows
				sheet.addMergedRegion(new CellRangeAddress(rowIdx, rowIdx, 0, 9));
				Row row = sheet.createRow(rowIdx++);
				Cell cell9 = row.createCell(0);
				cell9.setCellValue(
						"Total Number of Records : " + reportingListGroupByDTO.getTotalRecords());
				cell9.setCellStyle(totalCellStyle);

				// Total InstalmentAmount
				Cell cellInstalmentAmount = row.createCell(10);
				cellInstalmentAmount
						.setCellValue(reportingListGroupByDTO.getTotalInstalmentAmount());
				cellInstalmentAmount.setCellStyle(totalCellStyle);
				// Total InstalmentPrincipal
				Cell cellInstalmentPrincipal = row.createCell(11);
				cellInstalmentPrincipal
						.setCellValue(reportingListGroupByDTO.getTotalInstalmentPrincipal());
				cellInstalmentPrincipal.setCellStyle(totalCellStyle);
				// Total InstalmentInterest
				Cell cellInstalmentInterest = row.createCell(12);
				cellInstalmentInterest
						.setCellValue(reportingListGroupByDTO.getTotalInstalmentInterest());
				cellInstalmentInterest.setCellStyle(totalCellStyle);
				// *************************//

				// Total InstalmentPrincipalPaid
				Cell cellInstalmentPrincipalPaid = row.createCell(13);
				cellInstalmentPrincipalPaid
						.setCellValue(reportingListGroupByDTO.getTotalInstalPrincipalPaid());
				cellInstalmentPrincipalPaid.setCellStyle(totalCellStyle);
				// Total InstalmentinterestPaid
				Cell cellInstalmentinterestPaid = row.createCell(14);
				cellInstalmentinterestPaid
						.setCellValue(reportingListGroupByDTO.getTotalInstalInterestPaid());
				cellInstalmentinterestPaid.setCellStyle(totalCellStyle);
				// Total InstalmentTotalPaid
				Cell cellInstalmentTotalPaid = row.createCell(15);
				cellInstalmentTotalPaid
						.setCellValue(reportingListGroupByDTO.getTotalInstalTotalPaid());
				cellInstalmentTotalPaid.setCellStyle(totalCellStyle);
				// *************************//

				// Total TotalPrincipalPaid
				Cell cellTotalPrincipalPaid = row.createCell(16);
				cellTotalPrincipalPaid
						.setCellValue(reportingListGroupByDTO.getTotalTotalPrincipalPaid());
				cellTotalPrincipalPaid.setCellStyle(totalCellStyle);
				// Total TotalInterestPaid
				Cell cellTotalInterestPaid = row.createCell(17);
				cellTotalInterestPaid
						.setCellValue(reportingListGroupByDTO.getTotalTotalInterestPaid());
				cellTotalInterestPaid.setCellStyle(totalCellStyle);
				// Total TotalPaidAmount
				Cell cellTotalPaidAmount = row.createCell(18);
				cellTotalPaidAmount.setCellValue(reportingListGroupByDTO.getTotalTotalPaidAmount());
				cellTotalPaidAmount.setCellStyle(totalCellStyle);
				// *************************//

				// Total UnpaidPrincipal
				Cell cellUnpaidPrincipal = row.createCell(19);
				cellUnpaidPrincipal.setCellValue(reportingListGroupByDTO.getTotalUnpaidPrincipal());
				cellUnpaidPrincipal.setCellStyle(totalCellStyle);
				// Total UpaidInterest
				Cell cellUpaidInterest = row.createCell(20);
				cellUpaidInterest.setCellValue(reportingListGroupByDTO.getTotalUnpaidInterest());
				cellUpaidInterest.setCellStyle(totalCellStyle);
				// Total UpaidAmount
				Cell cellUpaidAmount = row.createCell(21);
				cellUpaidAmount.setCellValue(reportingListGroupByDTO.getTotalUnpaidAmount());
				cellUpaidAmount.setCellStyle(totalCellStyle);
				// getLateDays
				row.createCell(22).setCellValue("");
				// getNbUnpaidinstalment
				row.createCell(23).setCellValue("");
				// *************************//

				// Total RemainingPrincipal
				Cell cellRemainingPrincipal = row.createCell(24);
				cellRemainingPrincipal
						.setCellValue(reportingListGroupByDTO.getTotalRemainingPrincipal());
				cellRemainingPrincipal.setCellStyle(totalCellStyle);
				// Total RemainingInterest
				Cell cellRemainingInterest = row.createCell(25);
				cellRemainingInterest
						.setCellValue(reportingListGroupByDTO.getTotalRemaininginterest());
				cellRemainingInterest.setCellStyle(totalCellStyle);
				// Total RemainingAmount
				Cell cellRemainingAmount = row.createCell(26);
				cellRemainingAmount.setCellValue(reportingListGroupByDTO.getTotalRemainingAmount());
				cellRemainingAmount.setCellStyle(totalCellStyle);
				// *************************//
			}
			// init grand total rows
			sheet.addMergedRegion(new CellRangeAddress(rowIdx, rowIdx, 0, 9));
			Row row = sheet.createRow(rowIdx++);
			Cell cell9 = row.createCell(0);
			cell9.setCellValue(
					"Grand Total Number of Records : " + reportingListDTO.getTotalRecords());
			cell9.setCellStyle(totalCellStyle);

			// Total InstalmentAmount
			Cell cellInstalmentAmount = row.createCell(10);
			cellInstalmentAmount.setCellValue(reportingListDTO.getTotalInstalmentAmount());
			cellInstalmentAmount.setCellStyle(totalCellStyle);
			// Total InstalmentPrincipal
			Cell cellInstalmentPrincipal = row.createCell(11);
			cellInstalmentPrincipal.setCellValue(reportingListDTO.getTotalInstalmentPrincipal());
			cellInstalmentPrincipal.setCellStyle(totalCellStyle);
			// Total InstalmentInterest
			Cell cellInstalmentInterest = row.createCell(12);
			cellInstalmentInterest.setCellValue(reportingListDTO.getTotalInstalmentInterest());
			cellInstalmentInterest.setCellStyle(totalCellStyle);
			// *************************//

			// Total InstalmentPrincipalPaid
			Cell cellInstalmentPrincipalPaid = row.createCell(13);
			cellInstalmentPrincipalPaid
					.setCellValue(reportingListDTO.getTotalInstalPrincipalPaid());
			cellInstalmentPrincipalPaid.setCellStyle(totalCellStyle);
			// Total InstalmentinterestPaid
			Cell cellInstalmentinterestPaid = row.createCell(14);
			cellInstalmentinterestPaid.setCellValue(reportingListDTO.getTotalInstalInterestPaid());
			cellInstalmentinterestPaid.setCellStyle(totalCellStyle);
			// Total InstalmentTotalPaid
			Cell cellInstalmentTotalPaid = row.createCell(15);
			cellInstalmentTotalPaid.setCellValue(reportingListDTO.getTotalInstalTotalPaid());
			cellInstalmentTotalPaid.setCellStyle(totalCellStyle);
			// *************************//

			// Total TotalPrincipalPaid
			Cell cellTotalPrincipalPaid = row.createCell(16);
			cellTotalPrincipalPaid.setCellValue(reportingListDTO.getTotalTotalPrincipalPaid());
			cellTotalPrincipalPaid.setCellStyle(totalCellStyle);
			// Total TotalInterestPaid
			Cell cellTotalInterestPaid = row.createCell(17);
			cellTotalInterestPaid.setCellValue(reportingListDTO.getTotalTotalInterestPaid());
			cellTotalInterestPaid.setCellStyle(totalCellStyle);
			// Total TotalPaidAmount
			Cell cellTotalPaidAmount = row.createCell(18);
			cellTotalPaidAmount.setCellValue(reportingListDTO.getTotalTotalPaidAmount());
			cellTotalPaidAmount.setCellStyle(totalCellStyle);
			// *************************//

			// Total UnpaidPrincipal
			Cell cellUnpaidPrincipal = row.createCell(19);
			cellUnpaidPrincipal.setCellValue(reportingListDTO.getTotalUnpaidPrincipal());
			cellUnpaidPrincipal.setCellStyle(totalCellStyle);
			// Total UpaidInterest
			Cell cellUpaidInterest = row.createCell(20);
			cellUpaidInterest.setCellValue(reportingListDTO.getTotalUnpaidInterest());
			cellUpaidInterest.setCellStyle(totalCellStyle);
			// Total UpaidAmount
			Cell cellUpaidAmount = row.createCell(21);
			cellUpaidAmount.setCellValue(reportingListDTO.getTotalUnpaidAmount());
			cellUpaidAmount.setCellStyle(totalCellStyle);
			// getLateDays
			row.createCell(22).setCellValue("");
			// getNbUnpaidinstalment
			row.createCell(23).setCellValue("");
			// *************************//

			// Total RemainingPrincipal
			Cell cellRemainingPrincipal = row.createCell(24);
			cellRemainingPrincipal.setCellValue(reportingListDTO.getTotalRemainingPrincipal());
			cellRemainingPrincipal.setCellStyle(totalCellStyle);
			// Total RemainingInterest
			Cell cellRemainingInterest = row.createCell(25);
			cellRemainingInterest.setCellValue(reportingListDTO.getTotalRemaininginterest());
			cellRemainingInterest.setCellStyle(totalCellStyle);
			// Total RemainingAmount
			Cell cellRemainingAmount = row.createCell(26);
			cellRemainingAmount.setCellValue(reportingListDTO.getTotalRemainingAmount());
			cellRemainingAmount.setCellStyle(totalCellStyle);
			// *************************//

			// Setting Auto Column Width
			for (int i = 0; i < AR_HEADERS.length; i++) {
				sheet.autoSizeColumn(i);
				sheet.setColumnWidth(i, sheet.getColumnWidth(i) * 10 / 10);
			}

			// generate file
			workbook.write(out);

			// // create && save file in disk
			// try (FileOutputStream outputStream =
			// new FileOutputStream("C:\\projet_brjmf\\" + SHEET + ".xlsx")) {
			// workbook.write(outputStream);
			// }
			workbook.close();
			logger.info("Generating excel report : {} :: DONE", SHEET);
			return out.toByteArray();
		}
		catch (IOException e) {
			throw new RuntimeException("fail to import data to Excel file: " + e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ReportExcelService#generateExcelSchedule(com.acm.utils.dtos.LoanScheduleDTO)
	 */
	@Override
	public byte[] generateExcelSchedule(LoanScheduleDTO loanSchedule) throws IOException {

		final String SHEET = "Loan-schedule";
		final String TOTAL = "Total";
		final String LOAN_APPLICATION = "ACCOUNT NUMBER";
		final String LOAN_AMOUNT = "LOAN AMOUNT";
		final String CUSTOMER = "CUSTOMER NAME";
		final String CUSTOMER_NUMBER = "CUSTOMER NUMBER";
		final String TITLE = "LOAN SCHEDULE";
		final String[] columns =
				{"Period", "Repayment Date", "Total Repayment", "Interest", "Loan", "Balance"};

		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {

			Sheet sheet = workbook.createSheet(SHEET);
			int rowCount = 1;
			try {
				// get photo client from DB
				byte[] photoClientByte = gedClient.findPhotoClient(0L);
				// ============= Inserting image - START
				if (!ACMValidationUtils.isNullOrEmpty(photoClientByte)) {
					/* Add Picture to Workbook, Specify picture type as PNG and Get an Index */
					int logoId = workbook.addPicture(photoClientByte, Workbook.PICTURE_TYPE_PNG);

					/* Create the drawing container */
					XSSFDrawing drawing = (XSSFDrawing) sheet.createDrawingPatriarch();
					/* Create an anchor point */
					// ============= Inserting image - END
					// ========adding image START
					XSSFClientAnchor anchor = new XSSFClientAnchor();
					/* Define top left corner, and we can resize picture suitable from there */
					anchor.setCol1(0); // Column B
					anchor.setRow1(rowCount - 1); // Row 3
					anchor.setCol2(1); // Column C
					anchor.setRow2(rowCount); // Row 4
					/* Invoke createPicture and pass the anchor point and ID */
					XSSFPicture myPicture = drawing.createPicture(anchor, logoId);
					// Reset the image to the original size
					myPicture.resize();
					// ========adding image END
				}
			}
			catch (Exception e) {
				logger.error("Failed to get Log from GED");
				e.printStackTrace();
			}
			// init instant datetime
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
			String printedOn = dateFormat.format(new Date());
			// merge cells printed on
			CellRangeAddress regionPrintedOn = new CellRangeAddress(1, 1, 11, 14);
			sheet.addMergedRegion(regionPrintedOn);
			// create row printed on with border
			Row printedOnRow = sheet.createRow(1);
			Cell cellPrintedOn = printedOnRow.createCell(11);
			cellPrintedOn.setCellValue(SHEET_PRINTED_ON + printedOn);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionPrintedOn, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionPrintedOn, sheet);
			CellStyle styleTitle = workbook.createCellStyle();
			Font fontTitle = workbook.createFont();
			fontTitle.setBold(true);
			fontTitle.setFontHeightInPoints((short) 24);
			fontTitle.setColor(IndexedColors.ORANGE.index);
			styleTitle.setFont(fontTitle);
			styleTitle.setAlignment(HorizontalAlignment.CENTER);
			// Create style
			CellStyle styleBold = workbook.createCellStyle();
			// Create font
			Font font = workbook.createFont();
			// Make font bold
			font.setBold(true);
			// set it to bold
			styleBold.setFont(font);

			// set border
			styleBold.setBorderBottom(BorderStyle.THIN);
			styleBold.setBorderLeft(BorderStyle.THIN);
			styleBold.setBorderRight(BorderStyle.THIN);
			styleBold.setBorderTop(BorderStyle.THIN);
			styleBold.setAlignment(HorizontalAlignment.CENTER);

			// create style for customer and loan informations
			CellStyle styleCustomerLoanInformation = workbook.createCellStyle();
			// Create font
			Font fontCustomerLoanInformation = workbook.createFont();
			// Make font bold
			fontCustomerLoanInformation.setBold(true);

			// set it to bold
			styleCustomerLoanInformation.setFont(fontCustomerLoanInformation);
			styleCustomerLoanInformation.setBorderBottom(BorderStyle.THIN);
			styleCustomerLoanInformation.setBorderLeft(BorderStyle.THIN);
			styleCustomerLoanInformation.setBorderRight(BorderStyle.THIN);
			styleCustomerLoanInformation.setBorderTop(BorderStyle.THIN);
			styleCustomerLoanInformation.setAlignment(HorizontalAlignment.LEFT);
			String currency = "";
			if (!ACMValidationUtils.isNullOrEmpty(loanSchedule.getLoanDTO())
					&& !ACMValidationUtils.isNullOrEmpty(loanSchedule.getLoanDTO().getProductDTO())
					&& !ACMValidationUtils.isNullOrEmpty(
							loanSchedule.getLoanDTO().getProductDTO().getCurrency())) {
				currency = loanSchedule.getLoanDTO().getProductDTO().getCurrency();
			}
			// Loan informations
			Row rowLoan = sheet.createRow(2);
			Cell cellLoan = rowLoan.createCell(4);
			cellLoan.setCellValue(LOAN_APPLICATION + " : "
					+ (!ACMValidationUtils.isNullOrEmpty(loanSchedule.getLoanDTO())
							&& !ACMValidationUtils
									.isNullOrEmpty(loanSchedule.getLoanDTO().getAccountNumber())
											? loanSchedule.getLoanDTO().getAccountNumber()
											: ""));
			cellLoan.setCellStyle(styleCustomerLoanInformation);
			CellRangeAddress regionLoan = new CellRangeAddress(2, 2, 4, 6);
			sheet.addMergedRegion(regionLoan);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionLoan, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionLoan, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionLoan, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionLoan, sheet);
			// Loan amount
			Row rowLoanAmount = sheet.createRow(3);
			Cell cellLoanAmount = rowLoanAmount.createCell(4);
			cellLoanAmount.setCellValue(LOAN_AMOUNT + " : "
					+ (!ACMValidationUtils.isNullOrEmpty(loanSchedule.getLoanDTO())
							&& !ACMValidationUtils
									.isNullOrEmpty(loanSchedule.getLoanDTO().getApplyAmountTotal())
											? loanSchedule.getLoanDTO().getApplyAmountTotal()
													.doubleValue() + " " + currency
											: ""));
			cellLoanAmount.setCellStyle(styleCustomerLoanInformation);
			CellRangeAddress regionLoanAmount = new CellRangeAddress(3, 3, 4, 6);
			sheet.addMergedRegion(regionLoanAmount);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionLoanAmount, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionLoanAmount, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionLoanAmount, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionLoanAmount, sheet);
			// Customer name
			Row rowCustomer = sheet.createRow(4);
			Cell cellCustomer = rowCustomer.createCell(4);
			cellCustomer.setCellValue(CUSTOMER + " : "
					+ (!ACMValidationUtils.isNullOrEmpty(loanSchedule.getLoanDTO())
							&& !ACMValidationUtils.isNullOrEmpty(
									loanSchedule.getLoanDTO().getCustomerNameNoPipe())
											? loanSchedule.getLoanDTO().getCustomerNameNoPipe()
											: ""));
			cellCustomer.setCellStyle(styleCustomerLoanInformation);
			CellRangeAddress regionCustomer = new CellRangeAddress(4, 4, 4, 6);
			sheet.addMergedRegion(regionCustomer);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionCustomer, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionCustomer, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionCustomer, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionCustomer, sheet);
			// Customer number
			Row rowCustomerNumber = sheet.createRow(5);
			Cell cellCustomerNumber = rowCustomerNumber.createCell(4);
			cellCustomerNumber.setCellValue(CUSTOMER_NUMBER + " : " + (!ACMValidationUtils
					.isNullOrEmpty(loanSchedule.getLoanDTO())
					&& !ACMValidationUtils.isNullOrEmpty(loanSchedule.getLoanDTO().getCustomerDTO())
					&& !ACMValidationUtils.isNullOrEmpty(
							loanSchedule.getLoanDTO().getCustomerDTO().getCustomerNumber())
									? loanSchedule.getLoanDTO().getCustomerDTO().getCustomerNumber()
									: ""));
			cellCustomerNumber.setCellStyle(styleCustomerLoanInformation);
			CellRangeAddress regionCustomerNumber = new CellRangeAddress(5, 5, 4, 6);
			sheet.addMergedRegion(regionCustomerNumber);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionCustomerNumber, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionCustomerNumber, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionCustomerNumber, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionCustomerNumber, sheet);

			// TABLE TITLE
			Row rowTitle = sheet.createRow(11);
			Cell cellTitle = rowTitle.createCell(4);
			cellTitle.setCellValue(TITLE);
			cellTitle.setCellStyle(styleTitle);
			CellRangeAddress regionTitle = new CellRangeAddress(11, 11, 4, 7);
			sheet.addMergedRegion(regionTitle);
			// header tables styles
			Font headerFont = workbook.createFont();
			headerFont.setBold(true);
			headerFont.setFontHeightInPoints((short) 12);
			headerFont.setColor(IndexedColors.GREEN.index);
			CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);
			headerCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// set border for header
			headerCellStyle.setBorderBottom(BorderStyle.THIN);
			headerCellStyle.setBorderLeft(BorderStyle.THIN);
			headerCellStyle.setBorderRight(BorderStyle.THIN);
			headerCellStyle.setBorderTop(BorderStyle.THIN);

			rowCount = 13;
			Row headerRow = sheet.createRow(rowCount);
			for (int i = 0; i < columns.length; i++) {
				int index = i + 3;
				Cell cell = headerRow.createCell(index);
				cell.setCellValue(columns[i]);
				cell.setCellStyle(headerCellStyle);
				// cell.setCellStyle(styleBold);
			}
			// Create Cell Style for formatting Date
			CreationHelper createHelper = workbook.getCreationHelper();
			CellStyle dateCellStyle = workbook.createCellStyle();
			dateCellStyle.setDataFormat(
					createHelper.createDataFormat().getFormat(CommonConstants.PATTREN_DATE));
			dateCellStyle.setAlignment(HorizontalAlignment.CENTER);
			dateCellStyle.setBorderBottom(BorderStyle.THIN);
			dateCellStyle.setBorderLeft(BorderStyle.THIN);
			dateCellStyle.setBorderRight(BorderStyle.THIN);
			dateCellStyle.setBorderTop(BorderStyle.THIN);
			// Cell Style
			CellStyle cellStyle = workbook.createCellStyle();
			cellStyle.setAlignment(HorizontalAlignment.CENTER);
			cellStyle.setBorderBottom(BorderStyle.THIN);
			cellStyle.setBorderLeft(BorderStyle.THIN);
			cellStyle.setBorderRight(BorderStyle.THIN);
			cellStyle.setBorderTop(BorderStyle.THIN);
			// load data
			int rowNum = 14;
			int index = 3;
			for (int i = 0; i < loanSchedule.getScheduleDTOs().size() - 1; i++) {
				Row row = sheet.createRow(rowNum++);

				row.createCell(index);
				// Period
				Cell cellPeriod = row.createCell(index++);
				cellPeriod.setCellValue(loanSchedule.getScheduleDTOs().get(i).getPeriod());
				cellPeriod.setCellStyle(cellStyle);
				// Repayment Date
				Cell cellRepaymentDate = row.createCell(index++);
				cellRepaymentDate
						.setCellValue(loanSchedule.getScheduleDTOs().get(i).getRepaymentDate());
				cellRepaymentDate.setCellStyle(dateCellStyle);
				// Total Repayment
				Cell cellTotalRepayment = row.createCell(index++);
				cellTotalRepayment.setCellValue(
						loanSchedule.getScheduleDTOs().get(i).getTotalRepayment().doubleValue()
								+ " " + currency);
				cellTotalRepayment.setCellStyle(cellStyle);
				// Interest Repayment
				Cell cellInterestRepayment = row.createCell(index++);
				cellInterestRepayment.setCellValue(
						loanSchedule.getScheduleDTOs().get(i).getInterestRepayment().doubleValue()
								+ " " + currency);
				cellInterestRepayment.setCellStyle(cellStyle);
				// Loan Repayment
				Cell cellLoanRepayment = row.createCell(index++);
				cellLoanRepayment.setCellValue(
						loanSchedule.getScheduleDTOs().get(i).getLoanRepayment().doubleValue() + " "
								+ currency);
				cellLoanRepayment.setCellStyle(cellStyle);
				// Balance
				Cell cellBalance = row.createCell(index++);
				cellBalance.setCellValue(
						loanSchedule.getScheduleDTOs().get(i).getBalance().doubleValue() + " "
								+ currency);
				cellBalance.setCellStyle(cellStyle);
				index = 3;
			}
			// Total
			ScheduleDTO totalSchedule = new ScheduleDTO();
			totalSchedule =
					loanSchedule.getScheduleDTOs().get(loanSchedule.getScheduleDTOs().size() - 1);
			Row rowTotal = sheet.createRow(rowNum++);
			Cell cellTotal = rowTotal.createCell(4);
			cellTotal.setCellValue(TOTAL);
			cellTotal.setCellStyle(styleBold);
			Cell cellTotalRepaymentTotal = rowTotal.createCell(5);
			cellTotalRepaymentTotal
					.setCellValue(totalSchedule.getTotalRepayment().doubleValue() + " " + currency);
			cellTotalRepaymentTotal.setCellStyle(styleBold);

			Cell cellInterestRepaymentTotal = rowTotal.createCell(6);
			cellInterestRepaymentTotal.setCellValue(
					totalSchedule.getInterestRepayment().doubleValue() + " " + currency);
			cellInterestRepaymentTotal.setCellStyle(styleBold);

			Cell cellLoanRepaymentTotal = rowTotal.createCell(7);
			cellLoanRepaymentTotal
					.setCellValue(totalSchedule.getLoanRepayment().doubleValue() + " " + currency);
			cellLoanRepaymentTotal.setCellStyle(styleBold);

			// auto size cells
			for (int i = 0; i < columns.length + 3; i++) {
				sheet.autoSizeColumn(i);
			}
			workbook.write(out);
			workbook.close();
			logger.info("Generating excel Schedule Report : {} :: DONE", SHEET);
			return out.toByteArray();
		}
		catch (IOException e) {
			throw new RuntimeException(
					"fail to import data to Excel Schedule Report: " + e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportExcelService#generateAMLReport(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public byte[] generateAMLReport(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws IOException {

		// load && parse response data
		List<AMLDataDTO> amlDataDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getResponseValue())) {
			// Creating a Gson Object
			Gson gson = new Gson();
			// Converting jsonArray to List
			Type responseListType = new TypeToken<List<AMLDataDTO>>() {

			}.getType();
			amlDataDTOs =
					gson.fromJson(thirdPartyHistoriqueDTO.getResponseValue(), responseListType);
		}
		logger.debug("{}", amlDataDTOs);

		// find customer data
		Long idCustomer = thirdPartyHistoriqueDTO.getIdCustomer();
		// check if customer is guarantor
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomerGuarantor())
				&& ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomer())) {
			idCustomer = thirdPartyHistoriqueDTO.getIdCustomerGuarantor();
		}
		CustomerDTO customerDTO = creditClient.findCustomerById(idCustomer);

		// The sheet name
		final String SHEET = "AML REPORT";
		final String[] columns =
				{"م", "الأسم", "الرقم القومي", "تاريخ الميلاد", "التحديثات", "قضية"};

		final String[] columnsCustomer = {"إسم العميل", "الرقم القومي", "تاريخ الميلاد"};

		try (XSSFWorkbook workbook = new XSSFWorkbook();
				ByteArrayOutputStream out = new ByteArrayOutputStream();) {
			Sheet sheet = workbook.createSheet(SHEET);
			// header tables styles
			Font headerFont = workbook.createFont();
			((XSSFSheet) sheet).getCTWorksheet().getSheetViews().getSheetViewArray(0)
					.setRightToLeft(true);

			headerFont.setBold(true);
			headerFont.setFontHeightInPoints((short) 12);
			headerFont.setColor(IndexedColors.GREEN.index);
			CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);
			CellStyle dateCellStyle = workbook.createCellStyle();
			// format date
			CreationHelper createHelper = workbook.getCreationHelper();
			dateCellStyle.setDataFormat(
					createHelper.createDataFormat().getFormat(CommonConstants.PATTREN_DATE));
			dateCellStyle.setAlignment(HorizontalAlignment.CENTER);

			// style first table title : SAMA
			CellStyle rowTitleCellStyle = workbook.createCellStyle();
			// Create font
			Font font = workbook.createFont();
			// Make font bold
			font.setBold(true);
			Row headerRowCustomer = sheet.createRow(1);
			for (int i = 0; i < columnsCustomer.length; i++) {
				int index = i + 1;
				Cell cell = headerRowCustomer.createCell(index);
				cell.setCellValue(columnsCustomer[i]);
				cell.setCellStyle(headerCellStyle);
			}
			Row rowCustomer = sheet.createRow(2);
			rowCustomer.createCell(0);
			rowCustomer.createCell(1).setCellValue(customerDTO.getCustomerName().replace("|", " "));
			rowCustomer.createCell(2).setCellValue(customerDTO.getIdentity());
			Cell cellCustomerDate = rowCustomer.createCell(3);
			cellCustomerDate.setCellStyle(dateCellStyle);
			cellCustomerDate.setCellValue(customerDTO.getDateOfBirth());
			// setting border
			for (int i = 1; i <= 2; i++) {
				for (int j = 1; j <= 3; j++) {
					CellRangeAddress regionTitleCustomer = new CellRangeAddress(i, i, j, j);
					RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
					RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
					RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
					RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);
				}
			}

			// set it to bold
			rowTitleCellStyle.setFont(font);
			rowTitleCellStyle.setAlignment(HorizontalAlignment.CENTER);
			Row rowTitle = sheet.createRow(5);
			Cell firstTable = rowTitle.createCell(1);
			firstTable.setCellValue(
					"أسماء الأشخاص المدرجين وفقا لقانون تنظيم قوائم الكيانات الإرهابية والإرهابيين");
			firstTable.setCellStyle(rowTitleCellStyle);
			CellRangeAddress regionTitle = new CellRangeAddress(5, 5, 1, 6);
			sheet.addMergedRegion(regionTitle);
			RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitle, sheet);
			RegionUtil.setBorderTop(BorderStyle.THIN, regionTitle, sheet);
			RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitle, sheet);
			RegionUtil.setBorderRight(BorderStyle.THIN, regionTitle, sheet);
			// header first table
			int rowNum = 8;
			Row headerRow = sheet.createRow(rowNum);
			// setting headers
			for (int i = 0; i < columns.length; i++) {
				int index = i + 1;
				CellRangeAddress regionTitleCustomer = null;
				Cell cell = headerRow.createCell(index);
				cell.setCellValue(columns[i]);
				cell.setCellStyle(headerCellStyle);

				regionTitleCustomer = new CellRangeAddress(8, 8, i + 1, i + 1);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);
			}
			rowNum++;
			// first table data : AML-DATA
			for (AMLDataDTO amlDataDTO : amlDataDTOs) {
				Row row = sheet.createRow(rowNum++);

				CellRangeAddress regionTitleCustomer = null;
				row.createCell(0);

				row.createCell(1).setCellValue(
						!ACMValidationUtils.isNullOrEmpty(amlDataDTO.getReferenceInFile())
								? amlDataDTO.getReferenceInFile()
								: 1);
				regionTitleCustomer = new CellRangeAddress(rowNum - 1, rowNum - 1, 1, 1);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);

				row.createCell(2)
						.setCellValue(!ACMValidationUtils.isNullOrEmpty(amlDataDTO.getName())
								? amlDataDTO.getName()
								: "");
				regionTitleCustomer = new CellRangeAddress(rowNum - 1, rowNum - 1, 2, 2);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);

				row.createCell(3).setCellValue(
						!ACMValidationUtils.isNullOrEmpty(amlDataDTO.getIdentityNumber())
								? amlDataDTO.getIdentityNumber()
								: "");
				regionTitleCustomer = new CellRangeAddress(rowNum - 1, rowNum - 1, 3, 3);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);

				row.createCell(4)
						.setCellValue(!ACMValidationUtils.isNullOrEmpty(amlDataDTO.getDateOfBirth())
								? amlDataDTO.getDateOfBirth()
								: "");
				regionTitleCustomer = new CellRangeAddress(rowNum - 1, rowNum - 1, 4, 4);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);

				row.createCell(5)
						.setCellValue(!ACMValidationUtils.isNullOrEmpty(amlDataDTO.getUpdatedData())
								? amlDataDTO.getUpdatedData()
								: "");
				regionTitleCustomer = new CellRangeAddress(rowNum - 1, rowNum - 1, 5, 5);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);

				row.createCell(6).setCellValue(
						!ACMValidationUtils.isNullOrEmpty(amlDataDTO.getReferenceCase())
								? amlDataDTO.getReferenceCase()
								: "");
				regionTitleCustomer = new CellRangeAddress(rowNum - 1, rowNum - 1, 6, 6);
				RegionUtil.setBorderTop(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderBottom(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderLeft(BorderStyle.THIN, regionTitleCustomer, sheet);
				RegionUtil.setBorderRight(BorderStyle.THIN, regionTitleCustomer, sheet);
			}

			// auto size cells
			for (int i = 0; i < columns.length + 1; i++) {
				sheet.autoSizeColumn(i);
			}
			// create excel file
			workbook.write(out);

			// // create && save file in disk
			// try (FileOutputStream outputStream =
			// new FileOutputStream("C:\\demo\\" + SHEET + ".xlsx")) {
			// workbook.write(outputStream);
			// }
			workbook.close();
			logger.info("Generating excel report AML : {} :: DONE", SHEET);
			return out.toByteArray();
		}
		catch (IOException e) {
			throw new RuntimeException(
					"fail to import data to Excel AML Report: " + e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportExcelService#calculateFinancialReport(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public Map<String, Object> calculateFinancialReport(LoanDTO loanDTO) {

		File myFile = fileSystemStorageServiceImpl.loadFile("AnalyseFinTest.xlsx");
		// FileInputStream fis = null;
		try (FileInputStream fis = new FileInputStream(myFile)) {

			// Finds the workbook instance for XLSX file
			XSSFWorkbook finWorkBook = new XSSFWorkbook(fis);

			XSSFSheet outSheet = finWorkBook.getSheet("Output");

			String outp = outSheet.getRow(1).getCell(1).getRawValue();
			System.out.println("Output 1 before set params : " + outp);

			String outp2 = outSheet.getRow(2).getCell(1).getRawValue();
			System.out.println("Output 2 before set params : " + outp2);

			// Go to Input Sheet
			XSSFSheet inSheet = finWorkBook.getSheet("Input");

			Map<String, Object> mapUdf = getUDfMapForLoan(loanDTO);

			setInputsToExcelSheet(loanDTO, inSheet, mapUdf);

			// inSheet.getRow(1).getCell(1).setCellValue(loanDTO.getProductId());
			// inSheet.getRow(2).getCell(1).setCellValue(loanDTO.getApprovelAmount().doubleValue());
			// inSheet.getRow(3).getCell(1).setCellValue(loanDTO.getTermPeriodNum());
			// inSheet.getRow(3).getCell(1).setCellValue(loanDTO.getTermPeriodNum());

			// Recalculate all Formulas
			finWorkBook.setForceFormulaRecalculation(true);
			XSSFFormulaEvaluator.evaluateAllFormulaCells(finWorkBook);

			outSheet = finWorkBook.getSheet("Output");
			Map<String, Object> mapResults = new HashMap<String, Object>();
			for (int rowIndex = 1; rowIndex <= outSheet.getLastRowNum(); rowIndex++) {
				String cellKey = outSheet.getRow(rowIndex).getCell(0).getStringCellValue();
				if (!ACMValidationUtils.isNullOrEmpty(cellKey)) {
					mapResults.put(cellKey, readCellValue(outSheet.getRow(rowIndex).getCell(1)));
				}
			}

			outp = outSheet.getRow(1).getCell(1).getRawValue();
			System.out.println("Output 1 after set params : " + outp);

			outp2 = outSheet.getRow(2).getCell(1).getRawValue();
			System.out.println("Output 2 after set params : " + outp2);

			System.out.println("Result Map : " + mapResults.toString());
			return mapResults;

		}
		catch (Exception e) {
			logger.error("Error Calculate Financial Report : {} ", e.fillInStackTrace());

		}

		return null;
	}

	/**
	 * Gets the u df map for loan.
	 *
	 * @param loanDTO the loan DTO
	 * @return the u df map for loan
	 */
	private Map<String, Object> getUDfMapForLoan(LoanDTO loanDTO) {

		Map<String, Object> mapUdf = new HashMap<String, Object>();

		// GET LOAN UDF
		UserDefinedFieldsLinksDTO param = new UserDefinedFieldsLinksDTO();
		param.setLoanId(loanDTO.getLoanId());
		List<UDFLinksGroupeFieldsDTO> udfLinkGroups = creditClient.findUDFGroupBy(param);
		if (!ACMValidationUtils.isNullOrEmpty(udfLinkGroups)) {

			for (UDFLinksGroupeFieldsDTO udfGrp : udfLinkGroups) {
				for (UDFLinksGroupeFieldsModelDTO udfModel : udfGrp.getUdfGroupeFieldsModels()) {
					mapUdf.put(udfModel.getFieldName(), udfModel.getValue());
				}
			}
		}

		// GET CUSTOMER UDF
		param = new UserDefinedFieldsLinksDTO();
		param.setCustomerId(loanDTO.getCustomerDTO().getId());
		udfLinkGroups = creditClient.findUDFGroupBy(param);
		if (!ACMValidationUtils.isNullOrEmpty(udfLinkGroups)) {

			for (UDFLinksGroupeFieldsDTO udfGrp : udfLinkGroups) {
				for (UDFLinksGroupeFieldsModelDTO udfModel : udfGrp.getUdfGroupeFieldsModels()) {
					mapUdf.put(udfModel.getFieldName(), udfModel.getValue());
				}
			}
		}
		return mapUdf;
	}

	/**
	 * Read cell value.
	 *
	 * @param cell the cell
	 * @return the object
	 */
	private Object readCellValue(XSSFCell cell) {

		if (!ACMValidationUtils.isNullOrEmpty(cell)) {

			try {
				BigDecimal val = new BigDecimal(cell.getRawValue());
				return val;
			}
			catch (Exception e) {
				return cell.getRawValue();
			}

		}
		return null;
	}

	/**
	 * Sets the inputs to excel sheet.
	 *
	 * @author ymezrani
	 * @param loanDTO the loan DTO
	 * @param inSheet the in sheet
	 * @param mapUDF the map UDF
	 * @throws Exception the exception
	 */
	private void setInputsToExcelSheet(LoanDTO loanDTO, XSSFSheet inSheet,
			Map<String, Object> mapUDF) throws Exception {

		for (int rowIndex = 1; rowIndex <= inSheet.getLastRowNum(); rowIndex++) {
			String cellKey = inSheet.getRow(rowIndex).getCell(0).getStringCellValue();
			if (!ACMValidationUtils.isNullOrEmpty(cellKey)) {

				Object value = getValueFromObject(cellKey, loanDTO, mapUDF);
				if (value == null) {
					inSheet.getRow(rowIndex).getCell(1).setCellValue("");
				}
				else if (value instanceof Long || value instanceof Double) {
					inSheet.getRow(rowIndex).getCell(1).setCellValue((Long) value);
				}
				else if (value instanceof BigDecimal) {
					inSheet.getRow(rowIndex).getCell(1)
							.setCellValue(((BigDecimal) value).doubleValue());
				}
				else if (value instanceof Date) {
					inSheet.getRow(rowIndex).getCell(1).setCellValue((Date) value);
				}
				else {
					inSheet.getRow(rowIndex).getCell(1).setCellValue(value.toString());
				}
			}
		}

	}

	/**
	 * Gets the value from object.
	 *
	 * @param fieldKey the field key
	 * @param loanElement the loan element
	 * @param udfMap the udf map
	 * @return the value from object
	 * @throws Exception the exception
	 */
	private Object getValueFromObject(String fieldKey, Object loanElement,
			Map<String, Object> udfMap) throws Exception {

		Object value = null;

		
		if (fieldKey.matches("^UDF\\[.*\\]")) { 	// UDF
			String udfKey = fieldKey.substring(4, fieldKey.length() - 1);
			value = udfMap.get(udfKey);			
		}
		else if (fieldKey.contains(".")) { 			// Embedded Element

			int pointIndex = fieldKey.indexOf(".");
			String elementName = fieldKey.substring(0, pointIndex);
			String embFieldKey = fieldKey.substring(pointIndex + 1);

			for (PropertyDescriptor pd : Introspector.getBeanInfo(loanElement.getClass())
					.getPropertyDescriptors()) {
				if (pd.getReadMethod() != null && !"class".equals(pd.getName())
						&& pd.getName().toUpperCase().endsWith(elementName.toUpperCase())) {
					Object embFieldObject = pd.getReadMethod().invoke(loanElement);

					value = getValueFromObject(embFieldKey, embFieldObject, udfMap);
					break;
				}
			}
		}
		else {									// Standard field
			for (PropertyDescriptor pd : Introspector.getBeanInfo(loanElement.getClass())
					.getPropertyDescriptors()) {
				if (pd.getReadMethod() != null && !"class".equals(pd.getName())
						&& pd.getName().toUpperCase().endsWith(fieldKey.toUpperCase())) {
					value = pd.getReadMethod().invoke(loanElement);
					break;
				}
			}
		}

		return value;

	}

}
