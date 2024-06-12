/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.service.impl;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.client.ParametrageClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.json.model.CompanyLiveRequest;
import com.acm.json.model.CompanyRequest;
import com.acm.json.model.CreditFacility;
import com.acm.json.model.CustomerLiveRequest;
import com.acm.json.model.CustomerRequest;
import com.acm.json.model.Datum;
import com.acm.json.model.IScoreResponse;
import com.acm.json.model.ModuleData;
import com.acm.json.model.ProfileSummary;
import com.acm.json.model.RequestApplicationDetails;
import com.acm.json.model.RequestCompanySearchParameters;
import com.acm.json.model.RequestHeader;
import com.acm.json.model.RequestPersonSearchParameters;
import com.acm.json.model.RequestReportParameters;
import com.acm.json.service.IScoreService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.ThirdPartyStatus;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link IScoreServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
@Service
public class IScoreServiceImpl implements IScoreService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(IScoreServiceImpl.class);

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The Constant FIXED_RESPONSE_TYPE. */
	private static final String FIXED_RESPONSE_TYPE = "JSONANDPDF";

	/** The Constant FIXED_RESPONSE_LANG. */
	private static final String FIXED_RESPONSE_LANG = "EN";

	/** The Constant FIXED_HEADER_INQUIRY_TYPE. */
	private static final String FIXED_HEADER_INQUIRY_TYPE = "NEW_INQUIRY";

	/** The Constant FIXED_HEADER_SUBJECT_TYPE_PERSON. */
	private static final String FIXED_HEADER_SUBJECT_TYPE_PERSON = "PERSON";

	/** The Constant FIXED_HEADER_SUBJECT_TYPE_COMPANY. */
	private static final String FIXED_HEADER_SUBJECT_TYPE_COMPANY = "COMPANY";

	/** The Constant FIXED_REPORT_PARAMETERS_SEARCH_TYPE. */
	private static final String FIXED_REPORT_PARAMETERS_SEARCH_TYPE = "NMIDSRCH";

	/** The Constant FIXED_APPLICATION_DETAILS_INQUIRY_REASON. */
	private static final String FIXED_APPLICATION_DETAILS_INQUIRY_REASON = "1";

	/** The Constant FIXED_APPLICATION_DETAILS_INQUIRY_LOAN_TYPE. */
	private static final String FIXED_APPLICATION_DETAILS_INQUIRY_LOAN_TYPE = "001";

	/** The Constant FIXED_APPLICATION_DETAILS_REFERENC_NO. */
	private static final String FIXED_APPLICATION_DETAILS_REFERENC_NO = "DISPCORR";

	/*
	 * (non-Javadoc)
	 * @see com.acm.json.service.IScoreService#requestAPIIScore(com.acm.utils.dtos.ScreeningDTO)
	 */
	@Override
	public ScreeningDTO requestAPIIScore(ScreeningDTO screeningDTO) {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Start method : requestAPIIScore() => {}", screeningDTO);
		// Load strUserID | strPassword | APIPath
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("TAMKEEN_API_REQUEST"));
		// SETTING default valus
		String userID = "mf001800010001heq";
		String pwd = "Pass@123";
		String pathAPI = "https://www.i-score.com.eg/SbxLiveRequest/api/LiveRequest/";
		String apiKey = "sxpUhKRCJc6t06h0GMznol1SSfJELnjffKgFJtMy_1";
		String productId = "140001";
		String response_type = FIXED_RESPONSE_TYPE;
		String response_lang = FIXED_RESPONSE_LANG;
		String fixed_application_details_inquiry_loan_type =
				FIXED_APPLICATION_DETAILS_INQUIRY_LOAN_TYPE;

		// GETTING config
		userID = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_USER", userID);
		pwd = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_PASS", pwd);
		pathAPI = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_PATH", pathAPI);
		apiKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_API_KEY", apiKey);
		productId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_PRODUCT_ID", productId);
		response_type = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_TYPE", response_type);
		response_lang = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_LANG", response_lang);
		fixed_application_details_inquiry_loan_type = CommonFunctions.findConfigurationFromList(
				environnementDTOs, "TAMKEEN_API_REQUEST_LOAN_TYPE",
				fixed_application_details_inquiry_loan_type);
		// init & setting request params
		// PRODICT_ID => fixed = 140001
		RequestReportParameters requestReportParameters = new RequestReportParameters(
				FIXED_REPORT_PARAMETERS_SEARCH_TYPE, response_lang, productId, response_type);
		RequestApplicationDetails requestApplicationDetails =
				new RequestApplicationDetails(FIXED_APPLICATION_DETAILS_INQUIRY_REASON,
						fixed_application_details_inquiry_loan_type,
						String.valueOf(screeningDTO.getLoanDTO().getApplyAmountTotal().intValue()),
						FIXED_APPLICATION_DETAILS_REFERENC_NO);
		CustomerLiveRequest customerLiveRequest = null;
		CompanyLiveRequest companyLiveRequest = null;
		String requestString = "";
		if (screeningDTO.getCustomerDTO().getCustomerType().equals(CustomerType.INDIV.name())) {
			RequestHeader requestHeader = new RequestHeader(userID, pwd,
					FIXED_HEADER_SUBJECT_TYPE_PERSON, FIXED_HEADER_INQUIRY_TYPE);
			String name = screeningDTO.getCustomerDTO().getFirstName() + " "
					+ screeningDTO.getCustomerDTO().getSecondName() + " "
					+ screeningDTO.getCustomerDTO().getMiddleName() + " "
					+ screeningDTO.getCustomerDTO().getLastName();
			// IDENTIFIER_TYPE = 003 Fixed
			// 003 = Civil Affairs Authority - National ID
			// GENDER: (Male:001 / Female:002)
			RequestPersonSearchParameters requestPersonSearchParameters =
					new RequestPersonSearchParameters(name, "003",
							screeningDTO.getCustomerDTO().getIdentity(),
							DateUtil.formatDate(screeningDTO.getCustomerDTO().getDateOfBirth(),
									"dd-MMM-yyyy"),
							screeningDTO.getCustomerDTO().getGender().equals("M") ? "001" : "002",
							"");
			customerLiveRequest = new CustomerLiveRequest(requestHeader, requestReportParameters,
					requestPersonSearchParameters, requestApplicationDetails);
			logger.info("### INIT I-SCORE Request For CUSTOMER = [{}] ### ", customerLiveRequest);
			CustomerRequest customerRequest = new CustomerRequest(customerLiveRequest);
			requestString = CommonFunctions.convertObjectToJSONString(customerRequest);
			logger.info("### INIT I-SCORE Request For CUSTOMER (JSON) = [{}] ### ", requestString);
		}
		else if (screeningDTO.getCustomerDTO().getCustomerType().equals(CustomerType.ORG.name())) {
			RequestHeader requestHeader = new RequestHeader(userID, pwd,
					FIXED_HEADER_SUBJECT_TYPE_COMPANY, FIXED_HEADER_INQUIRY_TYPE);
			// TODO : "DATE_OF_REGISTRATION" ??
			// IDENTIFIER_TYPE" = 901 Fixed
			RequestCompanySearchParameters requestCompanySearchParameters =
					new RequestCompanySearchParameters(
							DateUtil.formatDate(new Date(), "dd-MMM-yyyy"), "901",
							screeningDTO.getCustomerDTO().getRegisterNumber(),
							screeningDTO.getCustomerDTO().getOrganizationName());
			companyLiveRequest = new CompanyLiveRequest(requestHeader, requestReportParameters,
					requestCompanySearchParameters, requestApplicationDetails);
			logger.info("### INIT I-SCORE Request For COMPANY = [{}] ### ", companyLiveRequest);
			CompanyRequest companyRequest = new CompanyRequest(companyLiveRequest);
			requestString = CommonFunctions.convertObjectToJSONString(companyRequest);
			logger.info("### INIT I-SCORE Request For COMPANY (JSON) = [{}] ### ", requestString);
		}
		screeningDTO.setXmlRequest(requestString);
		try {
			logger.info("### Send I-SCORE Request using USER = [{}] && PASS = [{}] ### ", userID,
					pwd);
			// call API Service
			// INIT REST_TEMPLATE
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();
			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
			headers.add("api_key", apiKey);

			// init request
			HttpEntity<String> request = new HttpEntity<>(requestString, headers);
			// init URI
			URI uri = new URI(pathAPI);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<IScoreResponse> responseAPI =
					restTemplate.postForEntity(uri, request, IScoreResponse.class);
			// check response
			logger.debug("{}", responseAPI.getBody());
			if (responseAPI.getStatusCode() == HttpStatus.OK) {
				logger.info("Request Successful");
			}
			else {
				logger.error("Request Failed with Code = {}", responseAPI.getStatusCode());
				return screeningDTO;
			}
			// setting returned DATA
			IScoreResponse iScoreResponse = responseAPI.getBody();
			logger.debug("body Response I-SCORE = {} ",
					iScoreResponse != null ? iScoreResponse.response : "NONE");

			// PARSING RESPONSE & GET DATA
			if (iScoreResponse.response != null) {
				List<ModuleData> moduleDatas = iScoreResponse.response.moduleData;
				// SCORE
				ModuleData moduledata = null;
				if (screeningDTO.getCustomerDTO().getCustomerType()
						.equals(CustomerType.INDIV.name())) {
					moduledata = moduleDatas.stream().filter(
							module -> module.moduleId.equals("iScoreProfileSectionConsumer"))
							.findFirst().get();
				}
				if (screeningDTO.getCustomerDTO().getCustomerType()
						.equals(CustomerType.ORG.name())) {
					moduledata = moduleDatas.stream().filter(
							module -> module.moduleId.equals("iScoreProfileSectionCommercial"))
							.findFirst().get();
				}
				if (!ACMValidationUtils.isNullOrEmpty(moduledata)) {
					List<Datum> datumsSCORE = moduledata.content.dATA;
					screeningDTO.setScore("0");
					if (!ACMValidationUtils.isNullOrEmpty(datumsSCORE)) {
						List<ProfileSummary> profileSummariesSCORE =
								datumsSCORE.get(0).profileSummary;
						if (!ACMValidationUtils.isNullOrEmpty(profileSummariesSCORE)) {
							screeningDTO.setScore("" + profileSummariesSCORE.get(0).sCORE);
						}
					}
				}
				logger.info("SCORE = {}", screeningDTO.getScore());

				// ACTIVE_LOAN
				moduledata = null;
				if (screeningDTO.getCustomerDTO().getCustomerType()
						.equals(CustomerType.INDIV.name())) {
					moduledata = moduleDatas.stream().filter(
							module -> module.moduleId.equals("iScoreCreditProfileOverviewConsumer"))
							.findFirst().get();
				}
				if (screeningDTO.getCustomerDTO().getCustomerType()
						.equals(CustomerType.ORG.name())) {
					moduledata = moduleDatas.stream()
							.filter(module -> module.moduleId
									.equals("iScoreCreditProfileOverviewCommercial"))
							.findFirst().get();
				}
				if (!ACMValidationUtils.isNullOrEmpty(moduledata)) {
					List<Datum> datumsCompany = moduledata.content.dATA;
					List<CreditFacility> creditFacilities = datumsCompany.get(0).creditFacilities;
					screeningDTO.setActiveLoan(creditFacilities.get(0).noOfAccounts);
					screeningDTO.setTotalApprovalAmt(creditFacilities.get(0).totalApprovalAmt);
					screeningDTO.setTotalMonthlyInstallmentAmt(
							creditFacilities.get(0).totalMonthlyInstallmentAmt);
					screeningDTO.setTotalBalanceAmount(creditFacilities.get(0).totalBalanceAmount);
				}
				// Set pdf file
				if (iScoreResponse.response != null) {
					byte[] encoded =
							java.util.Base64.getEncoder().encode(iScoreResponse.response.pDFStream);
					logger.info("  : generateIScoreReport() :: DONE");
					screeningDTO.setIscoreReport(java.util.Base64.getDecoder().decode(encoded));

					// remove file from Api json response before save history
					iScoreResponse.response.pDFStream = new byte[0];
				}

				// setting returned DATA
				screeningDTO
						.setXmlResponse(CommonFunctions.convertObjectToJSONString(iScoreResponse));

				logger.info("ACTIVE_LOAN = {}", screeningDTO.getActiveLoan());
				logger.info("TOTAL_APPROVAL_AMT = {}", screeningDTO.getTotalApprovalAmt());
				logger.info("TOTAL_MONTHLY_INSTALLMENT_AMT = {}",
						screeningDTO.getTotalMonthlyInstallmentAmt());
				logger.info("TOTAL_BALANCE_AMOUNT = {}", screeningDTO.getTotalBalanceAmount());

				// MAX_NUM_DAYS_DUE
				try {
					List<Integer> valuesMaxNumDaysDue = getValuesByKeyInJSONArray(
							screeningDTO.getXmlResponse(), "MAX_NUM_DAYS_DUE");
					if (!ACMValidationUtils.isNullOrEmpty(valuesMaxNumDaysDue)) {
						// Get Min or Max Number
						Comparator<Integer> comparator = Comparator.comparing(Integer::valueOf);
						Integer maxNumber = valuesMaxNumDaysDue.stream().max(comparator).get();
						logger.error("MAX_NUMBER = {}", maxNumber);
						// calculate SUM of all founded MAX_DU_NUMBER
						Integer sum = valuesMaxNumDaysDue.stream().reduce(0, Integer::sum);
						logger.error("SUM = {}", sum);
						// TODO
						screeningDTO.setMaxNumDaysDue(String.valueOf(maxNumber));
					}
				}
				catch (Exception e) {
					screeningDTO.setMaxNumDaysDue("0");
				}
				logger.info("MAX_NUM_DAYS_DUE = {}", screeningDTO.getMaxNumDaysDue());
				// setting decision to ACCEPTED
				screeningDTO.setDecision(ThirdPartyStatus.ACCEPTED.name());
			}
			else {
				// setting decision to FAILED
				screeningDTO.setDecision(ThirdPartyStatus.FAILED.name());
			}
		}
		catch (Exception e) {
			logger.error("{}", e.getMessage());
			e.printStackTrace();
		}
		logger.info("Execution method : requestAPI REST IScore() :: DONE");
		return screeningDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.json.service.IScoreService#generateIScoreReport(com.acm.utils.dtos.ScreeningDTO)
	 */
	@Override
	public byte[] generateIScoreReport(ScreeningDTO screeningDTO) {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(screeningDTO.getXmlRequest(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Start method generateIScoreReport() : XmlRequest()() => {}",
				screeningDTO.getXmlRequest());
		// Load strUserID | strPassword | API RESTPath
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("TAMKEEN_API_REQUEST"));
		// SETTING default valus
		String userID = "mf001800010001heq";
		String pwd = "Pass@123";
		String pathAPI = "https://www.i-score.com.eg/SbxLiveRequest/api/LiveRequest/";
		String apiKey = "sxpUhKRCJc6t06h0GMznol1SSfJELnjffKgFJtMy_1";
		String productId = "140001";
		String response_type = FIXED_RESPONSE_TYPE;
		String response_lang = FIXED_RESPONSE_LANG;
		String fixed_application_details_inquiry_loan_type =
				FIXED_APPLICATION_DETAILS_INQUIRY_LOAN_TYPE;

		// GETTING config
		userID = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_USER", userID);
		pwd = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_PASS", pwd);
		pathAPI = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_PATH", pathAPI);
		apiKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_API_KEY", apiKey);
		productId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_PRODUCT_ID", productId);
		response_type = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_TYPE", response_type);
		response_lang = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"TAMKEEN_API_REQUEST_LANG", response_lang);
		fixed_application_details_inquiry_loan_type = CommonFunctions.findConfigurationFromList(
				environnementDTOs, "TAMKEEN_API_REQUEST_LOAN_TYPE",
				fixed_application_details_inquiry_loan_type);

		// init & setting request params
		// PRODICT_ID => fixed = 140001
		RequestReportParameters requestReportParameters = new RequestReportParameters(
				FIXED_REPORT_PARAMETERS_SEARCH_TYPE, response_lang, productId, response_type);
		RequestApplicationDetails requestApplicationDetails =
				new RequestApplicationDetails(FIXED_APPLICATION_DETAILS_INQUIRY_REASON,
						fixed_application_details_inquiry_loan_type,
						screeningDTO.getLoanDTO().getApplyAmountTotal().toString(),
						FIXED_APPLICATION_DETAILS_REFERENC_NO);
		CustomerLiveRequest customerLiveRequest = null;
		CompanyLiveRequest companyLiveRequest = null;
		String requestString = "";
		if (screeningDTO.getCustomerDTO().getCustomerType().equals(CustomerType.INDIV.name())) {
			RequestHeader requestHeader = new RequestHeader(userID, pwd,
					FIXED_HEADER_SUBJECT_TYPE_PERSON, FIXED_HEADER_INQUIRY_TYPE);
			String name = screeningDTO.getCustomerDTO().getFirstName() + " "
					+ screeningDTO.getCustomerDTO().getSecondName() + " "
					+ screeningDTO.getCustomerDTO().getMiddleName() + " "
					+ screeningDTO.getCustomerDTO().getLastName();
			// IDENTIFIER_TYPE = 003 Fixed
			// 003 = Civil Affairs Authority - National ID
			// GENDER: (Male:001 / Female:002)
			RequestPersonSearchParameters requestPersonSearchParameters =
					new RequestPersonSearchParameters(name, "003",
							screeningDTO.getCustomerDTO().getIdentity(),
							DateUtil.formatDate(screeningDTO.getCustomerDTO().getDateOfBirth(),
									"dd-MMM-yyyy"),
							screeningDTO.getCustomerDTO().getGender().equals("M") ? "001" : "002",
							"");
			customerLiveRequest = new CustomerLiveRequest(requestHeader, requestReportParameters,
					requestPersonSearchParameters, requestApplicationDetails);
			logger.info("### INIT I-SCORE Request For CUSTOMER = [{}] ### ", customerLiveRequest);
			CustomerRequest customerRequest = new CustomerRequest(customerLiveRequest);
			requestString = CommonFunctions.convertObjectToJSONString(customerRequest);
			logger.info("### INIT I-SCORE Request For CUSTOMER (JSON) = [{}] ### ", requestString);
		}
		else if (screeningDTO.getCustomerDTO().getCustomerType().equals(CustomerType.ORG.name())) {
			RequestHeader requestHeader = new RequestHeader(userID, pwd,
					FIXED_HEADER_SUBJECT_TYPE_COMPANY, FIXED_HEADER_INQUIRY_TYPE);
			// TODO : "DATE_OF_REGISTRATION" ??
			// IDENTIFIER_TYPE" = 901 Fixed
			RequestCompanySearchParameters requestCompanySearchParameters =
					new RequestCompanySearchParameters(
							DateUtil.formatDate(new Date(), "dd-MMM-yyyy"), "901",
							screeningDTO.getCustomerDTO().getRegisterNumber(),
							screeningDTO.getCustomerDTO().getOrganizationName());
			companyLiveRequest = new CompanyLiveRequest(requestHeader, requestReportParameters,
					requestCompanySearchParameters, requestApplicationDetails);
			logger.info("### INIT I-SCORE Request For COMPANY = [{}] ### ", companyLiveRequest);
			CompanyRequest companyRequest = new CompanyRequest(companyLiveRequest);
			requestString = CommonFunctions.convertObjectToJSONString(companyRequest);
			logger.info("### INIT I-SCORE Request For COMPANY (JSON) = [{}] ### ", requestString);
		}
		// init & setting request params
		try {
			logger.info(
					"### Send I-SCORE generate report Request using USER = [{}] && PASS = [{}] ### ",
					userID, pwd);
			// call API REST Service
			// INIT REST_TEMPLATE
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();
			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.getAccept().clear();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
			headers.add("api_key", apiKey);

			// init request
			HttpEntity<String> request = new HttpEntity<>(requestString, headers);
			// init URI
			URI uri = new URI(pathAPI);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<IScoreResponse> responseAPI =
					restTemplate.postForEntity(uri, request, IScoreResponse.class);
			logger.debug("{}", responseAPI.getBody());
			if (responseAPI.getStatusCode() == HttpStatus.OK) {
				logger.info("Request Successful");
			}
			else {
				logger.error("Request Failed with Code = {}", responseAPI.getStatusCode());
				return null;
			}
			// setting returned DATA
			IScoreResponse iScoreResponse = responseAPI.getBody();
			logger.info("Execution method : generateIScoreReport() :: DONE");
			if (iScoreResponse.response != null) {
				byte[] encoded =
						java.util.Base64.getEncoder().encode(iScoreResponse.response.pDFStream);
				logger.info("Execution method : generateIScoreReport() :: DONE");
				return java.util.Base64.getDecoder().decode(encoded);
			}
			logger.warn("Execution method : generateIScoreReport() :: RETURNED EMPTY REPORT");
		}
		catch (Exception e) {
			logger.error("error while generating iscore report = {}", e.getMessage());
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Gets the values by key in JSON array.
	 * 
	 * @author HaythemBenizid
	 * @param jsonString the json string
	 * @param key the key
	 * @return the values by key in JSON array
	 */
	private List<Integer> getValuesByKeyInJSONArray(String jsonString, String key) {

		// INIT returned list
		List<Integer> values = new ArrayList<>();
		// Start parsing & extract data by KEY
		JSONObject jsonObject = new JSONObject(jsonString);
		JSONArray jsonArrayLevel1 = jsonObject.getJSONObject("Response").getJSONArray("ModuleData");
		for (int index = 0; index < jsonArrayLevel1.length(); index++) {
			JSONObject jsonObjectLevel1 = jsonArrayLevel1.getJSONObject(index);
			JSONObject jsonObjectLevel11 = jsonObjectLevel1.getJSONObject("Content");
			if (isJSONValidJSONArrayOrJSONObject(jsonObjectLevel11.toString(), "DATA") == 2) {
				JSONArray jsonArrayLevel2 = jsonObjectLevel11.getJSONArray("DATA");
				if (!jsonArrayLevel2.isEmpty()) {
					for (int index2 = 0; index2 < jsonArrayLevel2.length(); index2++) {
						JSONObject jsonObjectLevel2 = jsonArrayLevel2.getJSONObject(index2);
						if (isJSONValidJSONArrayOrJSONObject(jsonObjectLevel2.toString(),
								"MaxNDPDHistoryGraph") == 2) {
							values = parseJsonEntryByLevel(jsonObjectLevel2, values, key,
									"MaxNDPDHistoryGraph", "");
						}
						if (isJSONValidJSONArrayOrJSONObject(jsonObjectLevel2.toString(),
								"ClosedAccountDetails") == 2) {
							values = parseJsonEntryByLevel(jsonObjectLevel2, values, key,
									"ClosedAccountDetails", "DPDHistory18Months");
						}
						if (isJSONValidJSONArrayOrJSONObject(jsonObjectLevel2.toString(),
								"WrittenOffAccountDetails") == 2) {
							values = parseJsonEntryByLevel(jsonObjectLevel2, values, key,
									"WrittenOffAccountDetails", "DPDHistory18Months");
						}
						if (isJSONValidJSONArrayOrJSONObject(jsonObjectLevel2.toString(),
								"OpenAccountDetails") == 2) {
							values = parseJsonEntryByLevel(jsonObjectLevel2, values, key,
									"OpenAccountDetails", "DPDHistory18Months");
						}
						else {
							if (!ACMValidationUtils
									.isNullOrEmpty(jsonObjectLevel2.optString(key))) {
								values.add(Integer.parseInt(jsonObjectLevel2.optString(key)));
							}
						}
					}
				}
			}
		}
		return values;
	}

	/**
	 * Parses the JSON entry by level.
	 * 
	 * @author HaythemBenizid
	 * @param jsonObject the json object
	 * @param values the values
	 * @param key the key
	 * @param level1 the level 1
	 * @param level2 the level 2
	 * @return the list
	 */
	private List<Integer> parseJsonEntryByLevel(JSONObject jsonObject, List<Integer> values,
			String key, String level1, String level2) {

		JSONArray jsonArray = jsonObject.getJSONArray(level1);
		for (int index = 0; index < jsonArray.length(); index++) {
			JSONObject jsonObj = jsonArray.getJSONObject(index);
			if (isJSONValidJSONArrayOrJSONObject(jsonObj.toString(), level2) == 2) {
				JSONArray jsonArray2 = jsonObj.getJSONArray(level2);
				for (int index2 = 0; index2 < jsonArray2.length(); index2++) {
					JSONObject jsonObject2 = jsonArray2.getJSONObject(index2);
					if (!ACMValidationUtils.isNullOrEmpty(jsonObject2.optString(key))) {
						values.add(Integer.parseInt(jsonObject2.optString(key)));
					}
				}
			}
			if (!ACMValidationUtils.isNullOrEmpty(jsonObj.optString(key))) {
				values.add(Integer.parseInt(jsonObj.optString(key)));
			}
		}
		return values;
	}

	/**
	 * Checks if is JSON valid JSON array or JSON object : 0 = JSON not Valid / 1 :is
	 * {@link JSONObject} / 2 : is {@link JSONArray}.
	 * 
	 * @author HaythemBenizid
	 * @param json the json
	 * @param key the key
	 * @return the integer
	 */
	public static Integer isJSONValidJSONArrayOrJSONObject(String json, String key) {

		// in case JSONObject is valid
		try {
			new JSONObject(json);
		}
		catch (JSONException ex) {
			// in case JSONArray is valid as well...
			try {
				new JSONObject(json).getJSONArray(key);
				return 2;
			}
			catch (JSONException ex1) {
				return 0;
			}
		}
		// in case JSONObject is valid AND has JSONArray is valid as well...
		try {
			new JSONObject(json).getJSONArray(key);
			return 2;
		}
		catch (JSONException ex1) {
			return 0;
		}
	}
}
