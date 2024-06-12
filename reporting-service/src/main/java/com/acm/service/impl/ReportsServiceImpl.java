/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.ApplicationProperties;
import com.acm.DataSourceConfigAbacus;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ReportingException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.FileSystemStorageService;
import com.acm.service.ReportService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDetailsReportsDTO;
import com.acm.utils.dtos.ReportDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.number.NumberToWordConverter;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperExportManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports.engine.util.JRLoader;
import net.sf.jasperreports.engine.util.JRSaver;

/**
 * {@link ReportsServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class ReportsServiceImpl implements ReportService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ReportsServiceImpl.class);

	/** The file system storage service. */
	@Autowired
	private FileSystemStorageService fileSystemStorageService;

	/** The data source abacus. */
	@Autowired
	private DataSourceConfigAbacus dataSourceAbacus;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	@Autowired
	private ParametrageClient parametrageClient;

	@Autowired
	private DataSource dataSource;

	/** The root location. */
	private final Path rootLocation;

	/** The root location jrxml. */
	private final Path rootLocationJrxml;

	@Autowired
	private CreditClient creditClient;

	/**
	 * Instantiates a new file system storage service.
	 *
	 * @param properties the properties
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public ReportsServiceImpl(ApplicationProperties properties) throws IOException {

		this.rootLocation = Paths.get(properties.getFileStorageLocation().getURL().getPath());
		this.rootLocationJrxml = Paths.get(properties.getStorageLocationJrxml().getURL().getPath());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportService#init()
	 */
	@Override
	public void init() {

		try {
			FileSystemStorageServiceImpl.initializeStorage(rootLocation);
		}
		catch (ResourcesNotFoundException e) {
			logger.error("failed to initialize File System Storage {}", e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportService#deleteAll()
	 */
	@Override
	public void deleteAll() {

		try {
			FileSystemStorageServiceImpl.delete(rootLocation);
		}
		catch (ResourcesNotFoundException e) {
			logger.error("failed to delete File System Storage {}", e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportService#generatePDF(com.acm.utils.dtos.reportDTO)
	 */
	@Override
	public byte[] generatePDF(ReportDTO reportDTO) throws ReportingException {

		byte[] bytes = null;
		try {
			checkAndLoadJasperFile(reportDTO.getInputFileName());
			JasperReport jasperReport = checkAndLoadJasperFile(reportDTO.getInputFileName());
			if (!ACMValidationUtils
					.isNullOrEmpty(reportDTO.getEntryList().get(0).getApprovelAmount())) {

				reportDTO.getEntryList().get(0)
						.setAmountWord(NumberToWordConverter.convertNumberToWords(
								reportDTO.getEntryList().get(0).getApprovelAmount().toString(),
								"AR"));
			}
			JasperPrint jasperPrint =
					JasperFillManager.fillReport(jasperReport, reportDTO.getParams(),
							new JRBeanCollectionDataSource(reportDTO.getEntryList()));
			bytes = JasperExportManager.exportReportToPdf(jasperPrint);
		}
		catch (JRException | IllegalAccessException | NoSuchFieldException e) {
			logger.error("generatePDF: Encountered error when loading jasper file : {}",
					e.getMessage());
			e.printStackTrace();
			throw new ReportingException(
					new ExceptionResponseMessage(CommonErrorCode.FAILED_GENERATE_REPORT,
							CommonExceptionsMessage.FAILED_GENERATE_REPORT,
							new TechnicalException()),
					CommonExceptionsMessage.FAILED_GENERATE_REPORT);
		}
		return bytes;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportService#generateReport(com.acm.utils.dtos.reportDTO)
	 */
	@Override
	public byte[] generateReport(ReportDTO reportDTO) throws SQLException, ReportingException {

		Preconditions.checkNotNull(reportDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		byte[] bytes = null;
		// init params
		Map<String, Object> params = new HashMap<>();
		// Init connection
		java.sql.Connection connection = null;
		try {
			if (reportDTO.getTypeReport().equals("AGREEMENT")
					&& !reportDTO.getInputFileName().contains("ACM_")) {

				connection = dataSourceAbacus.abacusDataSource().getConnection();

				if (!ACMValidationUtils.isNullOrEmpty(reportDTO.getEntryList())
						&& reportDTO.getEntryList().get(0).getIdAccountExtern() != null
						&& reportDTO.getInputFileName() != null) {

					// guarantor informations from ACM
					CustomerDetailsReportsDTO customerDetailsReportsDTO = creditClient
							.getGuarantorsDetails(reportDTO.getEntryList().get(0).getLoanId());
					
					
					if (!ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO) && !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO.getCustomerName())) {
						logger.info("guarantor details", customerDetailsReportsDTO);
						// guarantor name
						params.put("GUARANTOR_NAME",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getCustomerName())
												? customerDetailsReportsDTO.getCustomerName()
												: " "));
						// age
						params.put("GUARANTOR_AGE",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getAge())
												? customerDetailsReportsDTO.getAge()
												: 0));
						// guarantor id
						params.put("GUARANTOR_ID",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getIdentity())
												? customerDetailsReportsDTO.getIdentity()
												: " "));
						// ssue place id
						params.put("GUARANTOER_ID_ISSUE_PLACE",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getPlaceOfIssue())
												? customerDetailsReportsDTO.getPlaceOfIssue()
												: " "));
						// issue date udf
						params.put("GUARANTOR_ID_ISSUE_DATE",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getIssueDate())
												? customerDetailsReportsDTO.getIssueDate()
												: " "));
						// family situation udf
						params.put("GUARANTOR_FAMILY_SITUATION",
								(!ACMValidationUtils.isNullOrEmpty(
										customerDetailsReportsDTO.getFamilySituation())
												? customerDetailsReportsDTO.getFamilySituation()
												: " "));
						// profession udf
						params.put("GUARANTOR_PROFESSION",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getProfession())
												? customerDetailsReportsDTO.getProfession()
												: "................."));
						// address
						String street = " ";
						String street_county = "";
						String street_county_towncity = "";
						String street_county_state = "";
						String country = "";
						String address2 = "";
						String address3 = "";
						String codePostal = "";
						// list of address
						int size = customerDetailsReportsDTO.getAddressDTO().size();
						int j = 0;
						for (int i = 0; i < size; i++) {
							j++;
							street = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getAddress1())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getAddress1()
													: "...............");
							street_county = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getCounty())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getCounty()
													: "...............");
							street_county_towncity = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getTownCity())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getTownCity()
													: "...............");
							street_county_state = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getState())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getState()
													: "...............");
							country = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getCountry())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getCountry()
													: "...............");
							address2 = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getAddress2())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getAddress2()
													: "...............");
							address3 = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getAddress3())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getAddress3()
													: "...............");
							codePostal = (!ACMValidationUtils
									.isNullOrEmpty(customerDetailsReportsDTO.getAddressDTO().get(i))
									&& !ACMValidationUtils.isNullOrEmpty(customerDetailsReportsDTO
											.getAddressDTO().get(i).getPostalCode())
													? customerDetailsReportsDTO.getAddressDTO()
															.get(i).getPostalCode()
													: "...............");
							params.put("GUARANTOR_ADDRESS" + j + "_STREET", street);
							params.put("GUARANTOR_ADDRESS" + j + "_STREET_COUNTY", street_county);
							params.put("GUARANTOR_ADDRESS" + j + "_STREET_COUNTY_TOWNCITY",
									street_county_towncity);
							params.put("GUARANTOR_ADDRESS" + j + "_STREET_COUNTY_STATE",
									street_county_state);
							// GUARANTOR_PRIMARY_ADDRESS full address
							params.put("GUARANTOR_ADDRESS" + j,
									street + ' ' + address2 + ' ' + street_county + ' '
											+ street_county_towncity + ' ' + street_county_state
											+ ' ' + codePostal + ' ' + country);
							// GUARANTOR_PRIMARY_ADDRESS
							if (customerDetailsReportsDTO.getAddressDTO().get(i)
									.getAddressTypeId() == 6) {
								params.put("GUARANTOR_PRIMARY_ADDRESS", street + ' ' + street_county
										+ ' ' + street_county_towncity + ' ' + street_county_state);
								// Guarantor address for : know your customer report
								params.put("GUARANTOR_ADDRESS_KYC",
										address3 + ' ' + street + ' ' + street_county + ' '
												+ street_county_towncity + ' ' + address2 + ' '
												+ codePostal + ' ' + street_county_state + ' '
												+ country);
							}

						}
						// if the guarantor has only one address : set the second address null
						if (size < 2) {
							params.put("GUARANTOR_ADDRESS2_STREET", null);
							params.put("GUARANTOR_ADDRESS2_STREET_COUNTY", null);
							params.put("GUARANTOR_ADDRESS2_STREET_COUNTY_TOWNCITY", null);
							params.put("GUARANTOR_ADDRESS2_STREET_COUNTY_STATE", null);
						}
						// mobile number
						params.put("GUARANTOR_MOBILE_NUMBER",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getMobileNumber())
												? customerDetailsReportsDTO.getMobileNumber()
												: " "));
						// phone number
						params.put("GUARANTOR_PHONE_NUMBER",
								(!ACMValidationUtils
										.isNullOrEmpty(customerDetailsReportsDTO.getPhoneNumber())
												? customerDetailsReportsDTO.getPhoneNumber()
												: " "));
					}
					params.put("ACCOUNTID", reportDTO.getEntryList().get(0).getIdAccountExtern());
					// loan id extern
					params.put("CULOANID", reportDTO.getEntryList().get(0).getIdLoanExtern());
					params.put("BRANCH_OPERATION_NAME",
							reportDTO.getEntryList().get(0).getOwnerName());
					params.put("REPORT_CONNECTION", connection);
					params.put("SUBREPORT_DIR", rootLocationJrxml.toString());
					// get responsible name from portfolioLoanId , responsible name = supervisor
					// name
					Long portfolioId = reportDTO.getEntryList().get(0).getPortfolioId();
					if (portfolioId != 0) {
						UserDTO userDTOPortfolio = new UserDTO();
						userDTOPortfolio.setAccountPortfolioId(
								reportDTO.getEntryList().get(0).getPortfolioId());
						UserDTO userDTOResponsible =
								userClient.findResponsibleOfUser(userDTOPortfolio);
						params.put("SUPERVISOR_NAME", userDTOResponsible.getSimpleName());
					}
					else {
						params.put("SUPERVISOR_NAME", userClient.find().getSimpleName());
					}
					// setting head Branch Operation name
					String headBranchOperation = "...................";
					List<UserDTO> userDTOs = userClient.findByGroupeCodeAndBranchID(
							CommonConstants.USER_GROUPE_BRANCH_OPERATION,
							reportDTO.getEntryList().get(0).getBranchID());
					if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
						headBranchOperation = userDTOs.get(0).getSimpleName();
					}
					params.put("CURRENT_USER", headBranchOperation);
					// find seat address
					AcmEnvironnementDTO acmEnvironnementDTO =
							parametrageClient.find(CommonConstants.SEAT_ADDRESS);
					// check seat address if exist
					if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO)) {
						params.put("SEAT_ADDRESS", acmEnvironnementDTO.getValue());
					}
					else {
						params.put("SEAT_ADDRESS", "");
					}
				}

			}
			else if (reportDTO.getTypeReport().equals("COLLECTION")) {

				connection = dataSourceAbacus.acmDataSource().getConnection();

				if (!ACMValidationUtils.isNullOrEmpty(reportDTO.getEntryListAcmCollections())
						&& reportDTO.getEntryListAcmCollections().get(0).getId() != null
						&& reportDTO.getInputFileName() != null) {
					// init params
					params.put("ID_ACM_COLLECTION",
							reportDTO.getEntryListAcmCollections().get(0).getId());
					params.put("REPORT_CONNECTION", connection);
					params.put("SUBREPORT_DIR", rootLocationJrxml.toString());

				}
			}
			else if (reportDTO.getInputFileName().contains("ACM_")) {
				connection = dataSourceAbacus.acmDataSource().getConnection();
				params.put("ACM_LOAN_ID", reportDTO.getEntryList().get(0).getLoanId());
				params.put("SUBREPORT_DIR", rootLocationJrxml.toString());
			}
			// check And Load JasperFile
			checkAndLoadJasperFile(reportDTO.getInputFileName());
			JasperReport jasperReport = checkAndLoadJasperFile(reportDTO.getInputFileName());

			// generate file
			JasperPrint jasperPrint =
					JasperFillManager.fillReport(jasperReport, params, connection);

			// returning byte array
			bytes = JasperExportManager.exportReportToPdf(jasperPrint);

			// close connection after using
			if (connection != null) {
				connection.close();
			}
		}
		catch (JRException e) {
			logger.error("Encountered error when generating jasper file : {}", e.getMessage());
			e.printStackTrace();
			// close connection after using
			connection.close();
			throw new ReportingException(
					new ExceptionResponseMessage(CommonErrorCode.FAILED_GENERATE_REPORT,
							CommonExceptionsMessage.FAILED_GENERATE_REPORT,
							new TechnicalException()),
					CommonExceptionsMessage.FAILED_GENERATE_REPORT);
		}
		catch (Exception e) {
			logger.error("Failed to generate File: {}", e.getMessage());
			e.printStackTrace();
			// close connection after using
			connection.close();

			throw new ReportingException(
					new ExceptionResponseMessage(CommonErrorCode.FAILED_GENERATE_REPORT,
							CommonExceptionsMessage.FAILED_GENERATE_REPORT,
							new TechnicalException()),
					CommonExceptionsMessage.FAILED_GENERATE_REPORT);
		}
		return bytes;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportService#generatePDFReport(java.lang.String, java.util.Map,
	 * net.sf.jasperreports.engine.JRDataSource)
	 */
	@Override
	public byte[] generatePDFReport(String inputFileName, Map<String, Object> params,
			JRDataSource dataSource) throws ReportingException {

		byte[] bytes = null;
		try (ByteArrayOutputStream byteArray = new ByteArrayOutputStream()) {
			JasperReport jasperReport = checkAndLoadJasperFile(inputFileName);
			JasperPrint jasperPrint =
					JasperFillManager.fillReport(jasperReport, params, dataSource);
			bytes = JasperExportManager.exportReportToPdf(jasperPrint);
		}
		catch (JRException | IOException e) {
			logger.error("Encountered error when loading jasper file : {}", e.getMessage());
			throw new ReportingException(
					new ExceptionResponseMessage(CommonErrorCode.FAILED_GENERATE_REPORT,
							CommonExceptionsMessage.FAILED_GENERATE_REPORT,
							new TechnicalException()),
					CommonExceptionsMessage.FAILED_GENERATE_REPORT);
		}
		return bytes;
	}

	/**
	 * Check and load jasper file.
	 * 
	 * @author HaythemBenizid
	 * @param inputFileName the input file name
	 * @return the jasper report
	 * @throws ReportingException the reporting exception
	 */
	private JasperReport checkAndLoadJasperFile(String inputFileName) throws ReportingException {

		JasperReport jasperReport = null;
		try {
			// Check if a compiled report exists
			if (fileSystemStorageService.jasperFileExists(inputFileName)) {
				jasperReport = (JasperReport) JRLoader
						.loadObject(fileSystemStorageService.loadJasperFile(inputFileName));
			}
			else {
				// Compile report from source and save
				String jrxml = fileSystemStorageService.loadJrxmlFile(inputFileName);
				logger.debug("{} loaded. Compiling report", jrxml);
				jasperReport = JasperCompileManager.compileReport(jrxml);
				// Save compiled report. Compiled report is loaded next time
				JRSaver.saveObject(jasperReport,
						fileSystemStorageService.loadJasperFile(inputFileName));
			}
		}
		catch (JRException e) {
			logger.error("Encountered error when loading jasper file : {}", e.getMessage());
			e.printStackTrace();
			throw new ReportingException(
					new ExceptionResponseMessage(CommonErrorCode.FAILED_GENERATE_REPORT,
							CommonExceptionsMessage.FAILED_GENERATE_REPORT,
							new TechnicalException()),
					CommonExceptionsMessage.FAILED_GENERATE_REPORT);
		}
		return jasperReport;
	}

	/**
	 * ### FOR TEST PURPOSE ### Loading data source.
	 *
	 * @return the JR data source
	 */
	@SuppressWarnings("unused")
	private JRDataSource loadingDataSource() {

		Collection<BeanWithList> coll = new ArrayList<>();

		BeanWithList bean = new BeanWithList(Arrays.asList("London", "Paris"), 1);
		coll.add(bean);
		bean = new BeanWithList(Arrays.asList("London", "Madrid", "Moscow"), 2);
		coll.add(bean);
		bean = new BeanWithList(Arrays.asList("Rome"), 3);
		coll.add(bean);

		return new JRBeanCollectionDataSource(coll);
	}

	/**
	 * ### FOR TEST PURPOSE ### The Class BeanWithList.
	 */
	public class BeanWithList {

		/**
		 * The cities.
		 */
		private List<String> cities;

		/**
		 * The id.
		 */
		private Integer id;

		/**
		 * Instantiates a new bean with list.
		 *
		 * @param cities the cities
		 * @param id the id
		 */
		public BeanWithList(List<String> cities, Integer id) {

			this.cities = cities;
			this.id = id;
		}

		/**
		 * Gets the cities.
		 *
		 * @return the cities
		 */
		public List<String> getCities() {

			return cities;
		}

		/**
		 * Sets the cities.
		 *
		 * @param cities the cities to set
		 */
		public void setCities(List<String> cities) {

			this.cities = cities;
		}

		/**
		 * Gets the id.
		 *
		 * @return the id
		 */
		public Integer getId() {

			return id;
		}

		/**
		 * Sets the id.
		 *
		 * @param id the id to set
		 */
		public void setId(Integer id) {

			this.id = id;
		}
	}
}
