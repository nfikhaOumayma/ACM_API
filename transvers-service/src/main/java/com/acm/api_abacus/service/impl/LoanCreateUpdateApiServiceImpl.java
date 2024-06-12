/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service.impl;

import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.impl.json.JSONValue;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.acm.api_abacus.model.LoanAbacusAPIModel;
import com.acm.api_abacus.model.LoanAbacusAPIModelCommunityLoan;
import com.acm.api_abacus.model.LoanAbacusAPIModelLoanApp;
import com.acm.api_abacus.model.LoanAbacusAPIModelProductLoan;
import com.acm.api_abacus.model.LoanAbacusAPIModelSurvey;
import com.acm.api_abacus.model.LoanAbacusAPIModelUdfLink;
import com.acm.api_abacus.model.MetaDataAPIAbacus;
import com.acm.api_abacus.service.CustomerAbacusApiService;
import com.acm.api_abacus.service.LoanApprovalApiService;
import com.acm.api_abacus.service.LoanCreateUpdateApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.api_charge_off.dtos.ReadFileCsvDTO;
import com.acm.api_charge_off.dtos.ResponseInfoChargeOffDTO;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.service.LoanAbacusService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.JournalEnteriesToAbacusDTO;
import com.acm.utils.dtos.LoanApprovalProcessDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.PaymentApiAbacusDTO;
import com.acm.utils.dtos.RequestPaymentApiAbacusDTO;
import com.acm.utils.dtos.ResponseGetInfoPaymentAbacusDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsModelDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.models.UserDefinedFieldsLinks;
import com.acm.utils.number.NumberUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link LoanCreateUpdateApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Service
public class LoanCreateUpdateApiServiceImpl implements LoanCreateUpdateApiService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoanCreateUpdateApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The customer abacus api service. */
	@Autowired
	private CustomerAbacusApiService customerAbacusApiService;

	/** The loan abacus service. */
	@Autowired
	private LoanAbacusService loanAbacusService;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri add loan. */
	@Value("${rest.api.abacus.loan.add.uri}")
	private String uriAddLoan;

	/** The uri loan approval part 1. */
	@Value("${rest.api.abacus.loan.approval.uri.part1}")
	private String uriLoanApprovalPart1;

	/** The uri loan approval part 2. */
	@Value("${rest.api.abacus.loan.approval.uri.part2}")
	private String uriLoanApprovalPart2;

	/** The uri get loan data. */
	@Value("${rest.api.abacus.loan.get.uri}")
	private String uriGetLoanData;

	/** The uri get loan data. */
	@Value("${rest.api.abacus.journal.entry.post.uri}")
	private String uriPostJournalEntry;

	/** The uri post journal page. */
	@Value("${rest.api.abacus.journal.page.post.uri}")
	private String uriPostJournalPage;

	/** The uri post payment loan. */
	@Value("${rest.api.abacus.payment.loan.post.uri}")
	private String uriPostPaymentLoan;

	// @Value("${rest.api.abacus.loan.update.uri.part1}")
	// private String uriUpdateLoanPart1;
	//
	// @Value("${rest.api.abacus.loan.update.uri.part2}")
	// private String uriUpdateLoanPart2;

	/** The url server authentication. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/** The loan approval api service. */
	@Autowired
	private LoanApprovalApiService loanApprovalApiService;

	/**
	 * Save.
	 *
	 * @param loanDTO the loan DTO
	 * @param customerType the customer type
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#save(com.acm.utils.dtos.LoanDTO,
	 * com.acm.utils.enums.CustomerType)
	 */
	@Override
	public LoanDTO save(LoanDTO loanDTO, CustomerType customerType)
			throws IOException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			// send loan to ABACUS via API by Type
			if (CustomerType.INDIV.equals(customerType) || CustomerType.ORG.equals(customerType)) {
				return saveLoan(loanDTO, restTemplate, headers);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
			logger.error("Failed to add loan {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
		}
		return null;
	}

	/**
	 * Save.
	 *
	 * @param loanDTOs the loan DT os
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#save(java.util.List)
	 */
	@Override
	public List<LoanDTO> save(List<LoanDTO> loanDTOs) throws IOException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			// build body object (JSON)
			JSONObject loanJsonObject = generateLoanGRPJSONObject(loanDTOs, CommonConstants.ADD);
			logger.info("************ REQUEST : SAVE LOAN GROUP ************");
			logger.info("{}", loanJsonObject);
			logger.info("************* SAVE LOAN GROUP ********************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// init URI
			String accessUrl = urlServeurApi + uriAddLoan;
			URI uri = new URI(accessUrl);
			logger.info("uri create LOAN GRP = {}", uri);
			List<LoanDTO> responses = new ArrayList<>();
			// sending request to server using POST method
			ResponseEntity<LoanAbacusAPIModel> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, LoanAbacusAPIModel.class);
			logger.debug("{}", responseAPI.getBody());
			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(), uri.toString(),
					responseAPI.getStatusCode().toString(), loanJsonObject.toString(),
					responseAPI.getBody().toString());
			logger.info("transversHistorique add loan = {}", transversHistoriqueDTO.toString());
			// add history of request and response api create customer in transvers historique
			// table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
			// getGroupe Loans
			List<LoanDTO> loanDTOGrp =
					loanDTOs.stream().filter(loanDTO -> loanDTO.getCustomerDTO().getCustomerType()
							.equals(CustomerType.GRP.name())).collect(Collectors.toList());
			LoanDTO loanDTOResponseGrp =
					getDataToSaveInACM(responseAPI.getBody(), loanDTOGrp.get(0));
			responses.add(loanDTOResponseGrp);
			// getChild Loans
			List<LoanDTO> loanDTOChilds =
					loanDTOs.stream()
							.filter(loanDTO -> loanDTO.getCustomerDTO().getCustomerType()
									.equals(CustomerType.INDIV.name()))
							.collect(Collectors.toList());
			for (LoanAbacusAPIModelCommunityLoan abacusAPIModel : responseAPI.getBody()
					.getCommunityLoans()) {
				for (LoanDTO loanDTO : loanDTOChilds) {
					LoanDTO loanDTOResponse = getChildDataToSaveInACM(abacusAPIModel, loanDTO);
					logger.info(
							"Loan has been successfully created in ABACUS DB with ID = [{}] && Account number = [{}]",
							loanDTOResponse.getIdLoanExtern(), loanDTOResponse.getAccountNumber());

					responses.add(loanDTOResponse);
				}

			}
			return responses;
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" add Loan GRP RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" add Loan GRP ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			String accessUrl = urlServeurApi + uriAddLoan;

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(),
					accessUrl, String.valueOf(e.getRawStatusCode()), null,
					e.getResponseBodyAsString());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occurred : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * Update.
	 *
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#update(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void update(LoanDTO loanDTO)
			throws IOException, ApiAbacusException, CheckApprovelLevelException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			// build body object (JSON)
			JSONObject loanJsonObject = new JSONObject();
			loanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
					loanDTO.getProductDTO().getProductIdAbacus());
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
					loanDTO.getPortfolioId() != null ? loanDTO.getPortfolioId() : 0);
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_INDUSTRY_CODE_ID.fieldName(), 0);
			loanJsonObject.put(MetaDataAPIAbacus.IS_REJECT_UDF_AND_ANALYSIS.fieldName(),
					Boolean.TRUE);
			loanJsonObject.put(MetaDataAPIAbacus.CU_INSURANCE_ID.fieldName(),
					loanDTO.getProductDTO().getCuInsuranceID());
			loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTO.getApr() != null ? loanDTO.getApr() : 0);

			// init loanApp object
			JSONObject loanAppJsonObject = new JSONObject();
			// init loanPart object
			JSONObject loanPartJsonObject = new JSONObject();
			loanPartJsonObject.put(MetaDataAPIAbacus.USE_SCHEDULE_INTEREST.fieldName(),
					loanDTO.getProductDTO().getUseScheduleInterest() != null
							? loanDTO.getProductDTO().getUseScheduleInterest()
							: JSONObject.NULL);
			loanPartJsonObject.put(MetaDataAPIAbacus.INTEREST_RATE.fieldName(),
					loanDTO.getProductRate());
			loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_AMOUNT.fieldName(),
					loanDTO.getApprovelAmount());
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_AMOUNT.fieldName(),
					loanDTO.getApprovelAmount());
			// Default value => FALSE
			loanPartJsonObject.put(MetaDataAPIAbacus.REFINANCED.fieldName(), Boolean.FALSE);
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_DATE.fieldName(),
					loanDTO.getIssueDate() != null
							? java.sql.Date.valueOf(
									DateUtil.convertToLocalDateViaInstant(loanDTO.getIssueDate()))
							: java.sql.Date
									.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
			// default value => TRUE
			loanPartJsonObject.put(MetaDataAPIAbacus.LEVEL_PAYMENTS.fieldName(), Boolean.TRUE);
			loanPartJsonObject.put(MetaDataAPIAbacus.INITIAL_PAYMENT_DATE.fieldName(),
					loanDTO.getInitialPaymentDate() != null
							? java.sql.Date.valueOf(DateUtil
									.convertToLocalDateViaInstant(loanDTO.getInitialPaymentDate()))
							: java.sql.Date
									.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
			// default value => 1
			loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD_NUM.fieldName(),
					loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);
			loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.INTPAY_PERIOD_NUM.fieldName(),
					loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);
			loanPartJsonObject.put(MetaDataAPIAbacus.IGNORE_ODD_DAYS.fieldName(),
					loanDTO.getIgnoreOddDays() != null ? loanDTO.getIgnoreOddDays() : Boolean.TRUE);
			// default value = 5 for BRJMF
			loanPartJsonObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED_ID.fieldName(),
					loanDTO.getPeriodsDeferredType());
			loanPartJsonObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED.fieldName(),
					loanDTO.getPeriodsDeferred() != null ? loanDTO.getPeriodsDeferred() : 0);
			// default value = 0
			loanPartJsonObject.put(MetaDataAPIAbacus.FIRST_REPAYMENT_OFF_SET.fieldName(), 0);

			// check if exit any stored grace period in DB
			Integer existingGracePeriod =
					loanDTO.getGracePeriod() != null ? loanDTO.getGracePeriod() : 0;
			loanPartJsonObject.put(MetaDataAPIAbacus.GRACE_PERIOD.fieldName(),
					loanDTO.getPeriodsDeferred() != null && loanDTO.getPeriodsDeferred() != 0 ? 1
							: existingGracePeriod);

			loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD_NUM.fieldName(),
					loanDTO.getTermPeriodNum());
			loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID()
							: JSONObject.NULL);
			loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_CALCULATION_MODE.fieldName(),
					loanDTO.getLoanCalculationMode() != null ? loanDTO.getLoanCalculationMode()
							: 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.NORMAL_PAYMENT.fieldName(),
					loanDTO.getNormalPayment());
			loanPartJsonObject.put(
					MetaDataAPIAbacus.CAPITALISE_INTEREST_WHEN_REFINANCING.fieldName(),
					loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing() != null
							? loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing()
							: JSONObject.NULL);
			// default value = FALSE
			loanPartJsonObject.put(MetaDataAPIAbacus.IS_REVIEWED.fieldName(), Boolean.FALSE);
			// default value = 1
			loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_CALCULATOR_AMOUNT_TYPE.fieldName(), 1);
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE1.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null
							? loanDTO.getProductDTO().getIssueFeePercentage1()
							: 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE2.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage2() != null
							? loanDTO.getProductDTO().getIssueFeePercentage2()
							: 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD_ID.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.REPAYMENT_PERIOD_ID.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);

			loanPartJsonObject.put(MetaDataAPIAbacus.EFFECTIVE_INT_RATE.fieldName(),
					loanDTO.getEffectiveIntRate() != null ? loanDTO.getEffectiveIntRate() : 0);

			loanPartJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTO.getApr() != null ? loanDTO.getApr() : 0);
			BigDecimal vat1 = (loanDTO.getProductDTO().getInsuranceVat()
					.multiply(loanDTO.getApprovelAmount())).divide(new BigDecimal(100));

			BigDecimal amountVat1 = loanDTO.getApprovelAmount().add(vat1);
			// issue fee with % fee
			BigDecimal feeAmount1 =
					(loanDTO.getProductDTO().getIssueFeePercentage1().multiply(amountVat1))
							.divide(new BigDecimal(100));
			// issue fee with fix fee amount
			BigDecimal feeAmount1WithFixFee = feeAmount1
					.add(loanDTO.getFeeAmt1() != null ? loanDTO.getFeeAmt1() : BigDecimal.ZERO);

			loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_1.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null
							? NumberUtils.roundBigDecimal(feeAmount1WithFixFee, 2,
									BigDecimal.ROUND_HALF_EVEN)
							: 0);

			BigDecimal vat2 = (loanDTO.getProductDTO().getInsuranceVat()
					.multiply(loanDTO.getApprovelAmount())).divide(new BigDecimal(100));

			BigDecimal amountVat2 = loanDTO.getApprovelAmount().add(vat2);

			BigDecimal feeAmount2 =
					(loanDTO.getProductDTO().getIssueFeePercentage2().multiply(amountVat2))
							.divide(new BigDecimal(100));
			BigDecimal feeAmount2WithFixFee =
					feeAmount2.add(loanDTO.getProductDTO().getIssueFeeAmount2());
			loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_2.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage2() != null
							? NumberUtils.roundBigDecimal(feeAmount2WithFixFee, 2,
									BigDecimal.ROUND_HALF_EVEN)
							: 0);

			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null
							&& loanDTO.getProductDTO().getIssueFeePercentage2() != null
									? NumberUtils.roundBigDecimal(loanDTO.getIssueFeeAmount(), 2,
											BigDecimal.ROUND_HALF_EVEN)
									: 0);
			// Default value 1 for BRJMF
			loanPartJsonObject.put(MetaDataAPIAbacus.DAY_COUNT.fieldName(), 2);
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);

			// Creating a json array cuLoanProcesses
			JSONArray arrayCuLoanProcesses = new JSONArray();
			JSONObject cuLoanProcessesJSONObject = new JSONObject();
			// find approval process from ABACUS by id loan extern
			List<LoanApprovalProcessDTO> loanApprovalProcessDTOs =
					loanAbacusService.findApprovalProcess(loanDTO.getIdLoanExtern());
			// check if process is null or empty
			if (ACMValidationUtils.isNullOrEmpty(loanApprovalProcessDTOs)) {
				throw new CheckApprovelLevelException(new ExceptionResponseMessage(
						CommonErrorCode.CHECK_APP_L1_CODE_DATA_NOT_FOUND,
						CommonExceptionsMessage.APPROVAL_PROCESS_FAILED, new TechnicalException()),
						CommonExceptionsMessage.APPROVAL_PROCESS_FAILED);
			}
			// setting CuLoanID
			Long cuLoanProcessId = 0L;
			if (!loanApprovalProcessDTOs.isEmpty()) {
				cuLoanProcessId = loanApprovalProcessDTOs.get(0).getCuLoanProcessID();
			}
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESS_ID.fieldName(),
					cuLoanProcessId);
			// default Approval etape 1 =1 =>"loanApprovalGroup": 1,
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
					loanDTO.getLoanApprovalLevel());
			// default => "description": "Loan Approval"
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
					"Loan : " + loanDTO.getAccountNumber() + " Approved");
			// default value of menuKey = cuLoansApprove
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(), "cuLoansApprove");
			// default =>"isCompleted": true,
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_COMPLETED.fieldName(), Boolean.TRUE);
			// default =>"reference": "",
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.REFERENCE.fieldName(), "");
			// default =>"isChanged": true,
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_CHANGED.fieldName(), Boolean.TRUE);
			arrayCuLoanProcesses.put(cuLoanProcessesJSONObject);
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESSES.fieldName(),
					arrayCuLoanProcesses);
			// Default value of status = 2 => approvel in ABACUS
			loanAppJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 2);
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_REASON_ID.fieldName(),
					loanDTO.getLoanReasonId() != null ? loanDTO.getLoanReasonId() : 2);
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_SOURCE_OF_FUNDS_ID.fieldName(),
					loanDTO.getSourceOfFundsID() != null ? loanDTO.getSourceOfFundsID() : 1);
			// default value = 0
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_GUARANTOR_SOURCE_ID.fieldName(), 0);
			// default value = 0
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_DISTRICT_CODE_ID.fieldName(), 0);
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_REFINANCE_REASON_ID.fieldName(),
					loanDTO.getRefinanceReasonId() != null ? loanDTO.getRefinanceReasonId() : 0);

			loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);
			if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getUserDefinedFieldsLinksDTOs())) {
				// Creating a json array surveys
				JSONArray arraySurveys = buildSurveysJsonObject(loanDTO, CommonConstants.UPDATE);
				loanJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);
			}
			logger.info("************ REQUEST : UPDATE LOAN ORG / INDIV ************");
			logger.info("{}", loanJsonObject);
			logger.info("*************** UPDATE LOAN ORG / INDIV ******************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// TODO init URI : UPDATE_LOAN : /api/loan/15/details (CULoanID) (PUT)
			// String accessUrl = urlServeurApi + uriUpdateLoanPart1 + loanDTO.getIdLoanExtern()
			// + uriUpdateLoanPart2

			// setting ID loan in URI => /api/loan/12/processes (CULoanID)
			String accessUrl = urlServeurApi + uriLoanApprovalPart1 + loanDTO.getIdLoanExtern()
					+ uriLoanApprovalPart2;

			URI uri = new URI(accessUrl);
			logger.info("uri UPDATE LOAN ORG / INDIV = {}", uri);
			// sending request to server using PUT method
			ResponseEntity<String> responseEntity =
					restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);
			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.LOAN.name(), RequestMethode.PUT.name(), uri.toString(),
					responseEntity.getStatusCode().toString(), loanJsonObject.toString(),
					responseEntity.getBody());
			logger.info("transversHistorique update loan = {}", transversHistoriqueDTO);
			// add history of request and response api update customer in transvers historique table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" update loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" update loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl = urlServeurApi + uriLoanApprovalPart1 + loanDTO.getIdLoanExtern()
					+ uriLoanApprovalPart2;
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.PUT.name(),
					accessUrl, String.valueOf(e.getRawStatusCode()), null,
					e.getResponseBodyAsString());

			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occurred while consuming update LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * Update.
	 *
	 * @param loanDTOs the loan DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#update(java.util.List)
	 */
	@Override
	public void update(List<LoanDTO> loanDTOs) throws IOException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			// get id loan group
			Long idLoanGroup = null;
			for (LoanDTO loanDTO : loanDTOs) {
				if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
					idLoanGroup = loanDTO.getIdLoanExtern();
				}
			}
			// build body object (JSON)
			JSONObject loanJsonObject = generateLoanGRPJSONObject(loanDTOs, CommonConstants.UPDATE);
			logger.info("************ REQUEST : UPDATE LOAN GROUP ************");
			logger.info("{}", loanJsonObject);
			logger.info("*************** UPDATE LOAN GROUP ******************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// init URI
			// TODO bug API update loan in ABACUS => change to use API Process Approve TEMPORARILY
			// String accessUrl =
			// urlServeurApi + uriUpdateLoanPart1 + idLoanGroup + uriUpdateLoanPart2

			// setting ID loan in URI => /api/loan/12/processes (CULoanID)
			String accessUrl =
					urlServeurApi + uriLoanApprovalPart1 + idLoanGroup + uriLoanApprovalPart2;

			URI uri = new URI(accessUrl);
			logger.info("uri UPDATE LOAN GROUP = {}", uri);
			// sending request to server using PUT method
			restTemplate.put(uri, request);
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" update loan grp RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" update loan grp ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			Long idLoanGroup = null;
			for (LoanDTO loanDTO : loanDTOs) {
				if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
					idLoanGroup = loanDTO.getIdLoanExtern();
				}
			}
			String accessUrl =
					urlServeurApi + uriLoanApprovalPart1 + idLoanGroup + uriLoanApprovalPart2;

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.PUT.name(),
					accessUrl, String.valueOf(e.getRawStatusCode()), null,
					e.getResponseBodyAsString());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occurred while consuming update LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * Save loan with type INDIV / ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param restTemplate the rest template
	 * @param headers the headers
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	private LoanDTO saveLoan(LoanDTO loanDTO, RestTemplate restTemplate, HttpHeaders headers)
			throws IOException, ApiAbacusException {

		try {
			// build body object (JSON)
			JSONObject loanJsonObject = generateLoanJSONObject(loanDTO, CommonConstants.ADD);
			logger.info("************ REQUEST : SAVE LOAN ORG / INDIV ************");
			logger.info("{}", loanJsonObject);
			logger.info("**************** SAVE LOAN ORG / INDIV *****************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// init URI
			String accessUrl = urlServeurApi + uriAddLoan;
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<LoanAbacusAPIModel> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, LoanAbacusAPIModel.class);
			logger.debug("{}", responseAPI.getBody());
			TransversHistoriqueDTO transversHistoriqueDTO = null;
			// try {
			// // TODO: handle exception
			//
			// transversHistoriqueDTO = new TransversHistoriqueDTO(
			// TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(),
			// uri.toString(), responseAPI.getStatusCode().toString(),
			// loanJsonObject.toString(), responseAPI.getBody().toString());
			// }
			// catch (Exception e) {
			// e.printStackTrace();
			// }
			// logger.info("transversHistorique add loan = {}", transversHistoriqueDTO.toString());
			// mapping result
			LoanDTO loanDTOResponse = getDataToSaveInACM(responseAPI.getBody(), loanDTO);
			logger.info(
					"Loan has been successfully created in ABACUS DB with ID = [{}] && Account number = [{}]",
					loanDTOResponse.getIdLoanExtern(), loanDTOResponse.getAccountNumber());
			// add history of request and response api create customer in transvers historique table
			// if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
			// creditClient.create(transversHistoriqueDTO);
			// }
			return loanDTOResponse;
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" add loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" add loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl = urlServeurApi + uriAddLoan;

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(),
					accessUrl, String.valueOf(e.getRawStatusCode()), null,
					e.getResponseBodyAsString());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException e) {
			logger.error("Error has been occurred :: URISyntaxException : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * Gets the data to save in ACM.
	 * 
	 * @author MOEZ
	 * @param loanAbacusAPIModel the loan abacus API model
	 * @param loanDTO the loan DTO
	 * @return the data to save in ACM
	 */
	private LoanDTO getDataToSaveInACM(LoanAbacusAPIModel loanAbacusAPIModel, LoanDTO loanDTO) {

		LoanDTO loanDTOResponse = new LoanDTO();
		LoanAbacusAPIModelLoanApp loanAppAbacusObject = loanAbacusAPIModel.getLoanApp();
		LoanAbacusAPIModelProductLoan productLoanAbacusObject = loanAbacusAPIModel.getProductLoan();
		loanDTOResponse.setIdLoanExtern(Long.valueOf(loanAbacusAPIModel.getCuLoanID()));
		loanDTOResponse.setAccountNumber(loanAbacusAPIModel.getAccountNumber());
		loanDTOResponse.setIdAccountExtern(Long.valueOf(loanAbacusAPIModel.getCuAccountID()));
		loanDTOResponse.setPortfolioId(Long.valueOf(loanAbacusAPIModel.getCuAccountPortfolioID()));
		loanDTOResponse.setProductId(loanAbacusAPIModel.getProductId());
		loanDTOResponse.setCustomerId(Long.valueOf(loanAbacusAPIModel.getCustomerID()));
		loanDTOResponse.setStatut(loanAbacusAPIModel.getAccStatus());
		loanDTOResponse
				.setCommunityCULoanID(Long.valueOf(loanAbacusAPIModel.getCommunityCULoanID()));
		loanDTOResponse
				.setIndustryCode(String.valueOf(loanAbacusAPIModel.getCuAccountIndustryCodeID()));
		loanDTOResponse.setApr(NumberUtils.roundBigDecimal(
				BigDecimal.valueOf(loanAbacusAPIModel.getApr()), 2, BigDecimal.ROUND_HALF_EVEN));
		loanDTOResponse.setProductDescription(productLoanAbacusObject.getDescription());
		loanDTOResponse.setProductCode(productLoanAbacusObject.getCode());
		loanDTOResponse.setCustomerName(loanDTO.getCustomerDTO().getCustomerName());
		loanDTOResponse.setCurrencySymbol(loanDTO.getProductDTO().getCurrency());
		loanDTOResponse.setCurrencyDecimalPlaces(loanDTO.getProductDTO().getDecimal());
		loanDTOResponse.setPortfolioCode(loanDTO.getCustomerDTO().getAccountPortfolioCode());
		loanDTOResponse
				.setPortfolioDescription(loanDTO.getCustomerDTO().getAccountPortfolioDescription());
		loanDTOResponse.setLoanReasonCode(loanDTO.getLoanReasonCode());
		loanDTOResponse.setLoanReasonDescription(loanDTO.getLoanReasonDescription());
		loanDTOResponse.setApplyDate(loanAppAbacusObject.getApplyDate());
		loanDTOResponse
				.setApplyAmountTotal(BigDecimal.valueOf(loanAppAbacusObject.getApplyAmountTotal()));
		loanDTOResponse.setCreationDate(new Date());
		loanDTOResponse.setTermPeriodNum(loanAppAbacusObject.getLoanPart().getTermPeriodNum());
		loanDTOResponse.setPaymentFreq(loanAppAbacusObject.getLoanPart().getRepaymentPeriodID());
		loanDTOResponse.setIssueDate(loanAppAbacusObject.getLoanPart().getIssueDate());
		loanDTOResponse.setIssueFeeAmount(
				BigDecimal.valueOf(loanAppAbacusObject.getLoanPart().getIssueFee()));
		loanDTOResponse.setProductRate(NumberUtils.roundBigDecimal(
				BigDecimal.valueOf(loanAppAbacusObject.getLoanPart().getInterestRate()), 2,
				BigDecimal.ROUND_HALF_EVEN));
		loanDTOResponse.setGracePeriod(loanAppAbacusObject.getLoanPart().getPeriodsDeferred());

		loanDTOResponse.setIndustryCodeDescription("");

		loanDTOResponse
				.setInitialPaymentDate(loanAppAbacusObject.getLoanPart().getInitialPaymentDate());
		loanDTOResponse.setNormalPayment(
				(new Double(loanAppAbacusObject.getLoanPart().getNormalPayment())).longValue());
		loanDTOResponse.setIgnoreOddDays(loanAppAbacusObject.getLoanPart().isIgnoreOddDays());
		loanDTOResponse.setPeriodsDeferred(loanAppAbacusObject.getLoanPart().getPeriodsDeferred());
		loanDTOResponse.setCalculateInitialPaymentDate(
				loanAppAbacusObject.getLoanPart().isCalculateInitialPaymentDate());
		loanDTOResponse
				.setTermPeriodID(Long.valueOf(loanAppAbacusObject.getLoanPart().getTermPeriodID()));

		loanDTOResponse.setBranchID(loanDTO.getCustomerDTO().getBranchId());
		loanDTOResponse.setBranchName(loanDTO.getCustomerDTO().getBranchesName());
		loanDTOResponse.setBranchDescription(loanDTO.getCustomerDTO().getBranchesDescription());

		loanDTOResponse.setCustomerType(loanDTO.getCustomerDTO().getCustomerType());

		loanDTOResponse.setGuarantorSourceId(loanAppAbacusObject.getCuLoanGuarantorSourceID());
		loanDTOResponse.setSourceOfFundsID(loanAppAbacusObject.getLoanSourceOfFundsID());
		loanDTOResponse.setRefinanceReasonId(loanAppAbacusObject.getCuLoanRefinanceReasonID());
		loanDTOResponse.setDistrictCodeId(loanAppAbacusObject.getCuLoanDistrictCodeID());
		loanDTOResponse.setIntPayPeriodNum(loanAppAbacusObject.getLoanPart().getIntPayPeriodNum());

		loanDTOResponse
				.setLoanCalculationMode(loanAppAbacusObject.getLoanPart().getLoanCalculationMode());

		loanDTOResponse.setEffectiveIntRate(NumberUtils.roundBigDecimal(
				BigDecimal.valueOf(loanAppAbacusObject.getLoanPart().getEffectiveIntRate()), 2,
				BigDecimal.ROUND_HALF_EVEN));
		loanDTOResponse.setFeeAmt1(loanDTO.getFeeAmt1());
		return loanDTOResponse;

	}

	/**
	 * Gets the child data to save in ACM.
	 *
	 * @param loanAbacusAPIModel the loan abacus API model
	 * @param loanDTO the loan DTO
	 * @return the child data to save in ACM
	 */
	private LoanDTO getChildDataToSaveInACM(LoanAbacusAPIModelCommunityLoan loanAbacusAPIModel,
			LoanDTO loanDTO) {

		LoanDTO loanDTOResponse = new LoanDTO();
		LoanAbacusAPIModelLoanApp loanAppAbacusObject = loanAbacusAPIModel.getLoanApp();
		LoanAbacusAPIModelProductLoan productLoanAbacusObject = loanAbacusAPIModel.getProductLoan();
		loanDTOResponse.setIdLoanExtern(Long.valueOf(loanAbacusAPIModel.getCuLoanID()));
		loanDTOResponse.setAccountNumber(loanAbacusAPIModel.getAccountNumber());
		loanDTOResponse.setIdAccountExtern(Long.valueOf(loanAbacusAPIModel.getCuAccountID()));
		loanDTOResponse.setPortfolioId(Long.valueOf(loanAbacusAPIModel.getCuAccountPortfolioID()));
		loanDTOResponse.setProductId(loanAbacusAPIModel.getProductId());
		loanDTOResponse.setCustomerId(Long.valueOf(loanAbacusAPIModel.getCustomerID()));
		loanDTOResponse.setStatut(loanAbacusAPIModel.getAccStatus());
		loanDTOResponse
				.setCommunityCULoanID(Long.valueOf(loanAbacusAPIModel.getCommunityCULoanID()));
		loanDTOResponse
				.setIndustryCode(String.valueOf(loanAbacusAPIModel.getCuAccountIndustryCodeID()));
		loanDTOResponse.setApr(new BigDecimal(0));
		loanDTOResponse.setProductDescription(productLoanAbacusObject.getDescription());
		loanDTOResponse.setProductCode(productLoanAbacusObject.getCode());
		loanDTOResponse.setCustomerName(loanDTO.getCustomerDTO().getCustomerName());
		loanDTOResponse.setCurrencySymbol(loanDTO.getProductDTO().getCurrency());
		loanDTOResponse.setCurrencyDecimalPlaces(loanDTO.getProductDTO().getDecimal());
		loanDTOResponse.setPortfolioCode(loanDTO.getCustomerDTO().getAccountPortfolioCode());
		loanDTOResponse
				.setPortfolioDescription(loanDTO.getCustomerDTO().getAccountPortfolioDescription());
		loanDTOResponse.setLoanReasonCode(loanDTO.getLoanReasonCode());
		loanDTOResponse.setLoanReasonDescription(loanDTO.getLoanReasonDescription());
		loanDTOResponse.setApplyDate(loanAppAbacusObject.getApplyDate());
		loanDTOResponse
				.setApplyAmountTotal(BigDecimal.valueOf(loanAppAbacusObject.getApplyAmountTotal()));
		loanDTOResponse.setCreationDate(new Date());
		loanDTOResponse.setTermPeriodNum(loanAppAbacusObject.getLoanPart().getTermPeriodNum());
		loanDTOResponse.setPaymentFreq(loanAppAbacusObject.getLoanPart().getRepaymentPeriodID());
		loanDTOResponse.setIssueDate(loanAppAbacusObject.getLoanPart().getIssueDate());
		loanDTOResponse.setIssueFeeAmount(
				BigDecimal.valueOf(loanAppAbacusObject.getLoanPart().getIssueFee()));
		loanDTOResponse.setProductRate(NumberUtils.roundBigDecimal(
				BigDecimal.valueOf(loanAppAbacusObject.getLoanPart().getInterestRate()), 2,
				BigDecimal.ROUND_HALF_EVEN));
		loanDTOResponse.setGracePeriod(loanAppAbacusObject.getLoanPart().getPeriodsDeferred());

		loanDTOResponse.setIndustryCodeDescription("");

		loanDTOResponse
				.setInitialPaymentDate(loanAppAbacusObject.getLoanPart().getInitialPaymentDate());
		loanDTOResponse.setNormalPayment(
				(new Double(loanAppAbacusObject.getLoanPart().getNormalPayment())).longValue());
		loanDTOResponse.setIgnoreOddDays(loanAppAbacusObject.getLoanPart().isIgnoreOddDays());
		loanDTOResponse.setPeriodsDeferred(loanAppAbacusObject.getLoanPart().getPeriodsDeferred());
		loanDTOResponse.setCalculateInitialPaymentDate(
				loanAppAbacusObject.getLoanPart().isCalculateInitialPaymentDate());
		loanDTOResponse
				.setTermPeriodID(Long.valueOf(loanAppAbacusObject.getLoanPart().getTermPeriodID()));

		loanDTOResponse.setBranchID(loanDTO.getCustomerDTO().getBranchId());
		loanDTOResponse.setBranchName(loanDTO.getCustomerDTO().getBranchesName());
		loanDTOResponse.setBranchDescription(loanDTO.getCustomerDTO().getBranchesDescription());

		loanDTOResponse.setCustomerType(loanDTO.getCustomerDTO().getCustomerType());

		loanDTOResponse.setGuarantorSourceId(loanAppAbacusObject.getCuLoanGuarantorSourceID());
		loanDTOResponse.setSourceOfFundsID(loanAppAbacusObject.getLoanSourceOfFundsID());
		loanDTOResponse.setRefinanceReasonId(loanAppAbacusObject.getCuLoanRefinanceReasonID());
		loanDTOResponse.setDistrictCodeId(loanAppAbacusObject.getCuLoanDistrictCodeID());
		loanDTOResponse.setIntPayPeriodNum(loanAppAbacusObject.getLoanPart().getIntPayPeriodNum());

		loanDTOResponse
				.setLoanCalculationMode(loanAppAbacusObject.getLoanPart().getLoanCalculationMode());

		loanDTOResponse.setEffectiveIntRate(NumberUtils.roundBigDecimal(
				BigDecimal.valueOf(loanAppAbacusObject.getLoanPart().getEffectiveIntRate()), 2,
				BigDecimal.ROUND_HALF_EVEN));
		return loanDTOResponse;

	}

	/**
	 * Generate loan JSON object for INDIV and ORG.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param action the action
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject generateLoanJSONObject(LoanDTO loanDTO, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		JSONObject loanJsonObject = new JSONObject();
		// create user ABACUS if not exist
		if (loanDTO.getCustomerDTO() != null
				&& (loanDTO.getCustomerDTO().getCustomerIdExtern() == null
						|| loanDTO.getCustomerDTO().getCustomerIdExtern() == 0)) {
			// save new customer in ABACUS DB
			CustomerDTO customerABACUS = customerAbacusApiService.save(loanDTO.getCustomerDTO());
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					customerABACUS.getCustomerIdExtern());
		}
		else {
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					loanDTO.getCustomerDTO().getCustomerIdExtern());
		}

		loanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
				loanDTO.getProductDTO().getProductIdAbacus());
		// TODO a verifie avec soumya
		loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
				loanDTO.getPortfolioId() != null ? loanDTO.getPortfolioId() : 0);
		loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_INDUSTRY_CODE_ID.fieldName(), 0);
		loanJsonObject.put(MetaDataAPIAbacus.CU_INSURANCE_ID.fieldName(),
				loanDTO.getProductDTO().getCuInsuranceID());
		loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
				loanDTO.getApr() != null ? loanDTO.getApr() : 0);

		// init loanApp object
		JSONObject loanAppJsonObject = new JSONObject();
		// init loanPart object
		JSONObject loanPartJsonObject = new JSONObject();
		loanPartJsonObject.put(MetaDataAPIAbacus.USE_SCHEDULE_INTEREST.fieldName(),
				loanDTO.getProductDTO().getUseScheduleInterest() != null
						? loanDTO.getProductDTO().getUseScheduleInterest()
						: JSONObject.NULL);
		loanPartJsonObject.put(MetaDataAPIAbacus.INTEREST_RATE.fieldName(),
				loanDTO.getProductRate() != null ? loanDTO.getProductRate()
						: loanDTO.getProductDTO().getRate());
		loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_AMOUNT.fieldName(),
				loanDTO.getApprovelAmount());
		loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_AMOUNT.fieldName(),
				loanDTO.getApprovelAmount());
		// Default value => FALSE
		loanPartJsonObject.put(MetaDataAPIAbacus.REFINANCED.fieldName(), Boolean.FALSE);
		loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_DATE.fieldName(),
				loanDTO.getIssueDate() != null
						? java.sql.Date.valueOf(
								DateUtil.convertToLocalDateViaInstant(loanDTO.getIssueDate()))
						: java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
		// default value => TRUE
		loanPartJsonObject.put(MetaDataAPIAbacus.LEVEL_PAYMENTS.fieldName(), Boolean.TRUE);
		loanPartJsonObject.put(MetaDataAPIAbacus.INITIAL_PAYMENT_DATE.fieldName(),
				loanDTO.getInitialPaymentDate() != null
						? java.sql.Date.valueOf(DateUtil
								.convertToLocalDateViaInstant(loanDTO.getInitialPaymentDate()))
						: java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
		// default value => 1
		loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD_NUM.fieldName(),
				loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);
		loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 3);
		loanPartJsonObject.put(MetaDataAPIAbacus.INTPAY_PERIOD_NUM.fieldName(),
				loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);
		loanPartJsonObject.put(MetaDataAPIAbacus.IGNORE_ODD_DAYS.fieldName(),
				loanDTO.getIgnoreOddDays() != null ? loanDTO.getIgnoreOddDays() : Boolean.TRUE);
		// default value = 5 for BRJMF
		loanPartJsonObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED_ID.fieldName(),
				loanDTO.getPeriodsDeferredType());
		loanPartJsonObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED.fieldName(),
				loanDTO.getPeriodsDeferred() != null ? loanDTO.getPeriodsDeferred() : 0);
		// default value = 0
		loanPartJsonObject.put(MetaDataAPIAbacus.FIRST_REPAYMENT_OFF_SET.fieldName(), 0);

		// check if exit any stored grace period in DB
		Integer existingGracePeriod =
				loanDTO.getGracePeriod() != null ? loanDTO.getGracePeriod() : 0;
		loanPartJsonObject.put(MetaDataAPIAbacus.GRACE_PERIOD.fieldName(),
				loanDTO.getPeriodsDeferred() != null && loanDTO.getPeriodsDeferred() != 0 ? 1
						: existingGracePeriod);

		loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD_NUM.fieldName(),
				loanDTO.getTermPeriodNum());
		loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 3);
		loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_CALCULATION_MODE.fieldName(),
				loanDTO.getLoanCalculationMode() != null ? loanDTO.getLoanCalculationMode() : 0);
		loanPartJsonObject.put(MetaDataAPIAbacus.NORMAL_PAYMENT.fieldName(),
				loanDTO.getNormalPayment());
		loanPartJsonObject.put(MetaDataAPIAbacus.CAPITALISE_INTEREST_WHEN_REFINANCING.fieldName(),
				loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing() != null
						? loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing()
						: JSONObject.NULL);
		// default value = FALSE
		loanPartJsonObject.put(MetaDataAPIAbacus.IS_REVIEWED.fieldName(), Boolean.FALSE);
		// default value = 1
		loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_CALCULATOR_AMOUNT_TYPE.fieldName(), 1);
		loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE1.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage1() != null
						? loanDTO.getProductDTO().getIssueFeePercentage1()
						: 0);
		loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE2.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage2() != null
						? loanDTO.getProductDTO().getIssueFeePercentage2()
						: 0);
		loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD_ID.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);
		loanPartJsonObject.put(MetaDataAPIAbacus.REPAYMENT_PERIOD_ID.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);

		loanPartJsonObject.put(MetaDataAPIAbacus.EFFECTIVE_INT_RATE.fieldName(),
				loanDTO.getEffectiveIntRate() != null ? loanDTO.getEffectiveIntRate() : 0);

		loanPartJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
				loanDTO.getApr() != null ? loanDTO.getApr() : 0);
		BigDecimal vat1 =
				(loanDTO.getProductDTO().getInsuranceVat().multiply(loanDTO.getApprovelAmount()))
						.divide(new BigDecimal(100));

		BigDecimal amountVat1 = loanDTO.getApprovelAmount().add(vat1);

		// issue fee with % fee
		BigDecimal feeAmount1 =
				(loanDTO.getProductDTO().getIssueFeePercentage1().multiply(amountVat1))
						.divide(new BigDecimal(100));
		// issue fee with fix fee amount
		BigDecimal feeAmount1WithFixFee = feeAmount1
				.add(loanDTO.getFeeAmt1() != null ? loanDTO.getFeeAmt1() : BigDecimal.ZERO);

		loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_1.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage1() != null
						? NumberUtils.roundBigDecimal(feeAmount1WithFixFee, 2,
								BigDecimal.ROUND_HALF_EVEN)
						: 0);

		BigDecimal vat2 =
				(loanDTO.getProductDTO().getInsuranceVat().multiply(loanDTO.getApprovelAmount()))
						.divide(new BigDecimal(100));

		BigDecimal amountVat2 = loanDTO.getApprovelAmount().add(vat2);

		BigDecimal feeAmount2 =
				(loanDTO.getProductDTO().getIssueFeePercentage2().multiply(amountVat2))
						.divide(new BigDecimal(100));
		BigDecimal feeAmount2WithFixFee =
				feeAmount2.add(loanDTO.getProductDTO().getIssueFeeAmount2());
		loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_2.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage2() != null
						? NumberUtils.roundBigDecimal(feeAmount2WithFixFee, 2,
								BigDecimal.ROUND_HALF_EVEN)
						: 0);

		loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage1() != null
						&& loanDTO.getProductDTO().getIssueFeePercentage2() != null
								? NumberUtils.roundBigDecimal(loanDTO.getIssueFeeAmount(), 2,
										BigDecimal.ROUND_HALF_EVEN)
								: 0);
		// Default value 1 for BRJMF
		loanPartJsonObject.put(MetaDataAPIAbacus.DAY_COUNT.fieldName(), 2);
		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);

		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_REASON_ID.fieldName(),
				loanDTO.getLoanReasonId());
		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_SOURCE_OF_FUNDS_ID.fieldName(),
				loanDTO.getSourceOfFundsID());
		// default value = 0
		loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_GUARANTOR_SOURCE_ID.fieldName(), 0);
		// default value = 0
		loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_DISTRICT_CODE_ID.fieldName(), 0);
		loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_REFINANCE_REASON_ID.fieldName(),
				loanDTO.getRefinanceReasonId() != null ? loanDTO.getRefinanceReasonId() : 0);
		// TODO LAST payment date : last date in Schedule
		loanAppJsonObject.put(MetaDataAPIAbacus.FINAL_PART_DATE.fieldName(), java.sql.Date
				.valueOf(DateUtil.convertToLocalDateViaInstant(loanDTO.getInitialPaymentDate())));

		loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);

		// Creating a json array surveys
		JSONArray arraySurveys = buildSurveysJsonObject(loanDTO, action);
		loanJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);

		return loanJsonObject;
	}

	/**
	 * Generate loan GRP JSON object.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @param action the action
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject generateLoanGRPJSONObject(List<LoanDTO> loanDTOs, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		// filtring loan group & childs list
		LoanDTO loanDTOGroup = new LoanDTO();
		List<LoanDTO> childs = new ArrayList<>();
		for (LoanDTO loanDTO : loanDTOs) {
			if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
				loanDTOGroup = loanDTO;
				// setting product object by ID
				if (ACMValidationUtils.isNullOrEmpty(loanDTO.getProductDTO()) || ACMValidationUtils
						.isNullOrEmpty(loanDTO.getProductDTO().getProductIdAbacus())) {
					loanDTOGroup.setProductDTO(
							parametrageClient.findProductById(loanDTO.getProductId().longValue()));
				}
			}
			else {
				// setting product
				if (!ACMValidationUtils.isNullOrEmpty(loanDTOGroup.getProductDTO())
						&& !ACMValidationUtils
								.isNullOrEmpty(loanDTOGroup.getProductDTO().getProductIdAbacus())) {
					loanDTO.setProductDTO(loanDTOGroup.getProductDTO());
				}
				else if (ACMValidationUtils.isNullOrEmpty(loanDTO.getProductDTO())
						&& loanDTO.getProductId() != null) {
					loanDTO.setProductDTO(
							parametrageClient.findProductById(loanDTO.getProductId().longValue()));
				}
				// setting childs
				childs.add(loanDTO);
			}
		}

		JSONObject loanJsonObject = new JSONObject();
		// create user ABACUS if not exist
		if (loanDTOGroup.getCustomerDTO() != null
				&& (loanDTOGroup.getCustomerDTO().getCustomerIdExtern() == null
						|| loanDTOGroup.getCustomerDTO().getCustomerIdExtern() == 0)) {
			// save new customer in ABACUS DB
			CustomerDTO customerABACUS =
					customerAbacusApiService.save(loanDTOGroup.getCustomerDTO());
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					customerABACUS.getCustomerIdExtern());
		}
		else {
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					loanDTOGroup.getCustomerDTO().getCustomerIdExtern());
		}
		loanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
				loanDTOGroup.getProductDTO().getProductIdAbacus());
		loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
				loanDTOGroup.getPortfolioId() != null ? loanDTOGroup.getPortfolioId() : 0);
		loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_INDUSTRY_CODE_ID.fieldName(), 0);
		// init loanApp object
		JSONObject loanAppJsonObject = new JSONObject();
		// init loanPart object
		JSONObject loanPartJsonObject = new JSONObject();
		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);

		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_REASON_ID.fieldName(),
				loanDTOGroup.getLoanReasonId());
		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_SOURCE_OF_FUNDS_ID.fieldName(),
				loanDTOGroup.getSourceOfFundsID());
		// default value = 0
		loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_GUARANTOR_SOURCE_ID.fieldName(), 0);
		// default value = 0
		loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_DISTRICT_CODE_ID.fieldName(), 0);
		loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_REFINANCE_REASON_ID.fieldName(),
				loanDTOGroup.getRefinanceReasonId());
		loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);

		// Creating a json array communityLoans
		JSONArray arrayCommunityLoans = buildCommunityLoansJsonObject(loanDTOGroup, childs, action);
		loanJsonObject.put(MetaDataAPIAbacus.COMMUNITY_LOANS.fieldName(), arrayCommunityLoans);

		// Creating a json array loanGuarantor
		JSONArray arrayLoanGuarantor = new JSONArray();
		loanJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR.fieldName(), arrayLoanGuarantor);

		return loanJsonObject;
	}

	/**
	 * Builds the community loans json object.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOGroup the loan DTO group
	 * @param childs the loans
	 * @param action the action
	 * @return the JSON array
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONArray buildCommunityLoansJsonObject(LoanDTO loanDTOGroup, List<LoanDTO> childs,
			String action) throws ApiAbacusException, IOException, URISyntaxException {

		// build JSON schema
		JSONArray arrayCommunityLoans = new JSONArray();
		for (LoanDTO loanDTO : childs) {
			JSONObject loanJsonObject = new JSONObject();

			// find by id ACM
			CustomerDTO customerDTOACM = creditClient.findById(loanDTO.getCustomerDTO().getId());

			// create user ABACUS if not exist
			if (customerDTOACM.getCustomerIdExtern() == 0) {
				// setting default address
				customerDTOACM.setListAddress(loanDTOGroup.getCustomerDTO().getListAddress());
				// save new customer in ABACUS DB
				CustomerDTO customerABACUS = customerAbacusApiService.save(customerDTOACM);
				// setting member data for new entry
				// update id extern
				customerDTOACM.setCustomerIdExtern(customerABACUS.getCustomerIdExtern());
				// update number
				customerDTOACM.setCustomerNumber(customerABACUS.getCustomerNumber());
				// update personne ID
				customerDTOACM.setPersonIdExtern(customerABACUS.getPersonIdExtern());
				loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
						customerDTOACM.getCustomerIdExtern());
				loanDTO.setCustomerDTO(customerDTOACM);
			}
			else {
				loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
						customerDTOACM.getCustomerIdExtern());
			}
			// Creating a json array surveys
			JSONArray arraySurveys = buildSurveysJsonObject(loanDTO, action);
			loanJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);
			loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTO.getApr() != null ? loanDTO.getApr() : 0);

			// init loanApp object
			JSONObject loanAppJsonObject = new JSONObject();
			// init loanPart object
			JSONObject loanPartJsonObject = new JSONObject();
			loanPartJsonObject.put(MetaDataAPIAbacus.USE_SCHEDULE_INTEREST.fieldName(),
					loanDTO.getProductDTO().getUseScheduleInterest() != null
							? loanDTO.getProductDTO().getUseScheduleInterest()
							: JSONObject.NULL);
			loanPartJsonObject.put(MetaDataAPIAbacus.INTEREST_RATE.fieldName(),
					loanDTO.getProductRate() != null ? loanDTO.getProductRate() : JSONObject.NULL);
			loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_AMOUNT.fieldName(),
					loanDTO.getApprovelAmount());
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_AMOUNT.fieldName(),
					loanDTO.getApprovelAmount());
			// Default value => FALSE
			loanPartJsonObject.put(MetaDataAPIAbacus.REFINANCED.fieldName(), Boolean.FALSE);
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_DATE.fieldName(),
					loanDTO.getIssueDate() != null
							? java.sql.Date.valueOf(
									DateUtil.convertToLocalDateViaInstant(loanDTO.getIssueDate()))
							: java.sql.Date
									.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
			// default value => TRUE
			loanPartJsonObject.put(MetaDataAPIAbacus.LEVEL_PAYMENTS.fieldName(), Boolean.TRUE);
			loanPartJsonObject.put(MetaDataAPIAbacus.INITIAL_PAYMENT_DATE.fieldName(),
					loanDTO.getInitialPaymentDate() != null
							? java.sql.Date.valueOf(DateUtil
									.convertToLocalDateViaInstant(loanDTO.getInitialPaymentDate()))
							: java.sql.Date
									.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
			// default value => 1
			loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD_NUM.fieldName(),
					loanDTO.getPaymentFreq() != null ? loanDTO.getPaymentFreq() : 1);
			loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 3);
			loanPartJsonObject.put(MetaDataAPIAbacus.INTPAY_PERIOD_NUM.fieldName(), 1);
			loanPartJsonObject.put(MetaDataAPIAbacus.IGNORE_ODD_DAYS.fieldName(),
					loanDTO.getIgnoreOddDays() != null ? loanDTO.getIgnoreOddDays() : Boolean.TRUE);
			// default value = 5 for BRJMF
			loanPartJsonObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED_ID.fieldName(), 5);
			loanPartJsonObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED.fieldName(),
					loanDTO.getPeriodsDeferred() != null ? loanDTO.getPeriodsDeferred() : 0);
			// default value = 0
			loanPartJsonObject.put(MetaDataAPIAbacus.FIRST_REPAYMENT_OFF_SET.fieldName(), 0);

			// check if exit any stored grace period in DB
			Integer existingGracePeriod =
					loanDTO.getGracePeriod() != null ? loanDTO.getGracePeriod() : 0;
			loanPartJsonObject.put(MetaDataAPIAbacus.GRACE_PERIOD.fieldName(),
					loanDTO.getPeriodsDeferred() != null && loanDTO.getPeriodsDeferred() != 0 ? 1
							: existingGracePeriod);

			loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD_NUM.fieldName(),
					loanDTO.getTermPeriodNum());
			loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID()
							: JSONObject.NULL);
			loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_CALCULATION_MODE.fieldName(),
					loanDTO.getLoanCalculationMode() != null ? loanDTO.getLoanCalculationMode()
							: 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.NORMAL_PAYMENT.fieldName(),
					loanDTO.getNormalPayment());
			loanPartJsonObject.put(
					MetaDataAPIAbacus.CAPITALISE_INTEREST_WHEN_REFINANCING.fieldName(),
					loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing() != null
							? loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing()
							: JSONObject.NULL);
			// default value = FALSE
			loanPartJsonObject.put(MetaDataAPIAbacus.IS_REVIEWED.fieldName(), Boolean.FALSE);
			// default value = 1
			loanPartJsonObject.put(MetaDataAPIAbacus.LOAN_CALCULATOR_AMOUNT_TYPE.fieldName(), 1);
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE1.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null
							? loanDTO.getProductDTO().getIssueFeePercentage1()
							: 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE2.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage2() != null
							? loanDTO.getProductDTO().getIssueFeePercentage2()
							: 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.TERM_PERIOD_ID.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.REPAYMENT_PERIOD_ID.fieldName(),
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);

			loanPartJsonObject.put(MetaDataAPIAbacus.EFFECTIVE_INT_RATE.fieldName(),
					loanDTO.getEffectiveIntRate() != null ? loanDTO.getEffectiveIntRate() : 0);

			loanPartJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTO.getApr() != null ? loanDTO.getApr() : 0);

			BigDecimal vat1 = (loanDTO.getProductDTO().getInsuranceVat()
					.multiply(loanDTO.getApprovelAmount())).divide(new BigDecimal(100));

			BigDecimal amountVat1 = loanDTO.getApprovelAmount().add(vat1);

			// issue fee with % fee
			BigDecimal feeAmount1 =
					(loanDTO.getProductDTO().getIssueFeePercentage1().multiply(amountVat1))
							.divide(new BigDecimal(100));
			// issue fee with fix fee amount
			BigDecimal feeAmount1WithFixFee = feeAmount1
					.add(loanDTO.getFeeAmt1() != null ? loanDTO.getFeeAmt1() : BigDecimal.ZERO);

			loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_1.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null
							? NumberUtils.roundBigDecimal(feeAmount1WithFixFee, 2,
									BigDecimal.ROUND_HALF_EVEN)
							: 0);

			BigDecimal vat2 = (loanDTO.getProductDTO().getInsuranceVat()
					.multiply(loanDTO.getApprovelAmount())).divide(new BigDecimal(100));

			BigDecimal amountVat2 = loanDTO.getApprovelAmount().add(vat2);

			BigDecimal feeAmount2 =
					(loanDTO.getProductDTO().getIssueFeePercentage2().multiply(amountVat2))
							.divide(new BigDecimal(100));
			BigDecimal feeAmount2WithFixFee =
					feeAmount2.add(loanDTO.getProductDTO().getIssueFeeAmount2());
			loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_2.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage2() != null
							? NumberUtils.roundBigDecimal(feeAmount2WithFixFee, 2,
									BigDecimal.ROUND_HALF_EVEN)
							: 0);

			loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null
							&& loanDTO.getProductDTO().getIssueFeePercentage2() != null
									? NumberUtils.roundBigDecimal(loanDTO.getIssueFeeAmount(), 2,
											BigDecimal.ROUND_HALF_EVEN)
									: 0);
			// Default value 1 for BRJMF
			loanPartJsonObject.put(MetaDataAPIAbacus.DAY_COUNT.fieldName(), 2);
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);
			loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);

			arrayCommunityLoans.put(loanJsonObject);
		}
		return arrayCommunityLoans;
	}

	/**
	 * Builds the surveys {@link UserDefinedFieldsLinks} JSON object.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param action the action
	 * @return the JSON array
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONArray buildSurveysJsonObject(LoanDTO loanDTO, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		loanDTO.setUserDefinedFieldsLinksDTOs(loanDTO.getUserDefinedFieldsLinksDTOs().stream()
				.filter(udfLink -> (udfLink.getUserDefinedFieldsDTO().getIdUDFField() != 0))
				.collect(Collectors.toList()));
		JSONArray arraySurveys = new JSONArray();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getUserDefinedFieldsLinksDTOs())) {
			// get list of GROUP GROUP INDEX
			List<Long> groupIDindex = new ArrayList<>();
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : loanDTO
					.getUserDefinedFieldsLinksDTOs()) {
				groupIDindex.add(userDefinedFieldsLinksDTO.getIndexGroup());
			}
			// filter list group ID (Remove Duplicates)
			List<Long> groupIDindexWithoutDuplicates = new ArrayList<>(new HashSet<>(groupIDindex));

			// get list of GROUP ID ABACUS
			Map<Long, Long> groupIDList = new HashMap<>();
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : loanDTO
					.getUserDefinedFieldsLinksDTOs()) {
				if (groupIDindexWithoutDuplicates
						.indexOf(userDefinedFieldsLinksDTO.getIndexGroup()) != -1) {
					groupIDList.put(userDefinedFieldsLinksDTO.getIndexGroup(),
							userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO()
									.getUserDefinedFieldGroupDTO().getIdUDGroupAbacus());
					groupIDindexWithoutDuplicates.remove(groupIDindexWithoutDuplicates
							.indexOf(userDefinedFieldsLinksDTO.getIndexGroup()));
				}
			}

			List<UDFLinksGroupeFieldsDTO> udfGroupeFields = new ArrayList<>();
			for (Map.Entry<Long, Long> mapGroupIndex : groupIDList.entrySet()) {
				Long idUDFGroup = mapGroupIndex.getValue();
				Long indexGroup = mapGroupIndex.getKey();
				logger.debug("*** idUDFGroup = {} ***", idUDFGroup);
				// init object
				UDFLinksGroupeFieldsDTO udfGroupeField = new UDFLinksGroupeFieldsDTO();
				udfGroupeField.setDate(new Date());
				udfGroupeField.setTotalScore(0L);
				udfGroupeField.setUserDefinedFieldGroupID(idUDFGroup);
				List<UDFLinksGroupeFieldsModelDTO> udfGroupeFieldsModels = new ArrayList<>();
				for (UserDefinedFieldsLinksDTO dto : loanDTO.getUserDefinedFieldsLinksDTOs()) {
					if (dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO()
							.getIdUDGroupAbacus().equals(idUDFGroup)
							&& dto.getIndexGroup().equals(indexGroup)) {
						UDFLinksGroupeFieldsModelDTO udfGroupeFieldsModel =
								new UDFLinksGroupeFieldsModelDTO(
										dto.getUserDefinedFieldsDTO().getIdUDFField(),
										dto.getFieldValue(), dto.getUdfListValueId(), 1,
										dto.getUserDefinedFieldsDTO().getFieldMasc(),
										dto.getUserDefinedFieldsDTO().getMandatory(),
										dto.getUserDefinedFieldsDTO().getUniqueField(),
										Boolean.FALSE, dto.getUserDefinedFieldsDTO().getFieldType(),
										dto.getUserDefinedFieldsDTO().getName(), "1", null,
										dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO()
												.getCustomerType());
						// setting IDs
						udfGroupeFieldsModel.setId(dto.getId());
						udfGroupeFieldsModel.setIdAbacusUDFLink(dto.getIdAbacusUDFLink());
						udfGroupeFieldsModel.setSurveysId(dto.getSurveysId());
						if (dto.getSurveysId() != null) {
							udfGroupeField.setSurveysId(dto.getSurveysId());
						}

						udfGroupeFieldsModels.add(udfGroupeFieldsModel);
					}
				}
				udfGroupeField.setUdfGroupeFieldsModels(udfGroupeFieldsModels);
				udfGroupeFields.add(udfGroupeField);
			}
			// build json schema
			for (UDFLinksGroupeFieldsDTO udfFields : udfGroupeFields) {
				JSONObject surveysJSONObject = new JSONObject();
				surveysJSONObject.put(MetaDataAPIAbacus.USER_DEFINED_FIELD_GROUP_ID.fieldName(),
						udfFields.getUserDefinedFieldGroupID());
				surveysJSONObject.put(MetaDataAPIAbacus.DATE.fieldName(), java.sql.Date
						.valueOf(DateUtil.convertToLocalDateViaInstant(udfFields.getDate())));
				if (CommonConstants.UPDATE.equals(action)
						&& !ACMValidationUtils.isNullOrEmpty(udfFields.getSurveysId())) {
					// add surveys ID (cas UPDATE)
					surveysJSONObject.put(MetaDataAPIAbacus.SURVEY_ID.fieldName(),
							udfFields.getSurveysId());
				}

				// Default value = 0
				surveysJSONObject.put(MetaDataAPIAbacus.TOTAL_SCORE.fieldName(), 0);
				// build udfLinks
				JSONArray arrayUdfLinks = new JSONArray();
				for (UDFLinksGroupeFieldsModelDTO udfGroupeFieldsModel : udfFields
						.getUdfGroupeFieldsModels()) {
					JSONObject udfLinksJSONObject = new JSONObject();
					if (CommonConstants.UPDATE.equals(action) && !ACMValidationUtils
							.isNullOrEmpty(udfGroupeFieldsModel.getIdAbacusUDFLink())) {
						// add UDF links ID (cas UPDATE)
						udfLinksJSONObject.put(MetaDataAPIAbacus.UDFLINK_ID.fieldName(),
								udfGroupeFieldsModel.getIdAbacusUDFLink());
					}
					// setting ID field in ABACUS
					udfLinksJSONObject.put(MetaDataAPIAbacus.UDF_FIELD_ID.fieldName(),
							udfGroupeFieldsModel.getUdfFieldID());
					udfLinksJSONObject.put(MetaDataAPIAbacus.VALUE.fieldName(),
							udfGroupeFieldsModel.getValue() != null
									? udfGroupeFieldsModel.getValue()
									: "");
					udfLinksJSONObject.put(MetaDataAPIAbacus.USER_DEFINED_FIELD_LIST_ID.fieldName(),
							udfGroupeFieldsModel.getUserDefinedFieldListID());
					udfLinksJSONObject.put(MetaDataAPIAbacus.ORDER.fieldName(),
							udfGroupeFieldsModel.getOrder());
					udfLinksJSONObject.put(MetaDataAPIAbacus.MASK.fieldName(),
							udfGroupeFieldsModel.getMask());
					udfLinksJSONObject.put(MetaDataAPIAbacus.MANDATORY.fieldName(),
							udfGroupeFieldsModel.getMandatory());
					udfLinksJSONObject.put(MetaDataAPIAbacus.UNIQUE_FIELD.fieldName(),
							udfGroupeFieldsModel.getUniqueField());
					udfLinksJSONObject.put(MetaDataAPIAbacus.VALIDATE_ACTIVE_UDF.fieldName(),
							udfGroupeFieldsModel.getValidateActiveUDF());
					udfLinksJSONObject.put(MetaDataAPIAbacus.UDF_TYPE.fieldName(),
							udfGroupeFieldsModel.getUdfType());
					udfLinksJSONObject.put(MetaDataAPIAbacus.UDF_FIELD_NAME.fieldName(),
							udfGroupeFieldsModel.getFieldName());
					udfLinksJSONObject.put(MetaDataAPIAbacus.FIELD_CURRENCY.fieldName(),
							udfGroupeFieldsModel.getFieldCurrency());
					udfLinksJSONObject.put(MetaDataAPIAbacus.DATE_VALUE.fieldName(),
							udfGroupeFieldsModel.getDateValue() != null
									? java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
											udfGroupeFieldsModel.getDateValue()))
									: java.sql.Date.valueOf(
											DateUtil.convertToLocalDateViaInstant(new Date())));
					udfLinksJSONObject.put(MetaDataAPIAbacus.CUSTOMER_TYPES.fieldName(),
							udfGroupeFieldsModel.getCustomerTypes());
					udfLinksJSONObject.put(MetaDataAPIAbacus.SURVEY_ID.fieldName(),
							udfFields.getSurveysId());
					arrayUdfLinks.put(udfLinksJSONObject);
				}
				surveysJSONObject.put(MetaDataAPIAbacus.UDF_LINKS.fieldName(), arrayUdfLinks);
				arraySurveys.put(surveysJSONObject);
			}
		}

		if (CommonConstants.UPDATE.equals(action)) {
			// get loan DATA
			LoanAbacusAPIModel oldLoanAbacusAPIModel = getData(loanDTO.getIdAccountExtern());
			if (oldLoanAbacusAPIModel != null) {
				String intialJson =
						CommonFunctions.convertObjectToJSONString(oldLoanAbacusAPIModel);
				logger.info("Get LOAN ABACUS JSON : {}", intialJson);
				// builds && update JSON
				JSONObject loanJsonObject = new JSONObject(intialJson);
				JSONArray arraySurveysAbacus = loanJsonObject.getJSONArray("surveys");
				try {
					final ObjectMapper objectMapper = new ObjectMapper();
					// get & map JSON ABACUS
					LoanAbacusAPIModelSurvey[] abacusSurveys = objectMapper.readValue(
							arraySurveysAbacus.toString(), LoanAbacusAPIModelSurvey[].class);
					List<LoanAbacusAPIModelSurvey> abacusSurveyList =
							new ArrayList<>(Arrays.asList(abacusSurveys));
					// get & map JSON ACM
					LoanAbacusAPIModelSurvey[] acmSurvey = objectMapper
							.readValue(arraySurveys.toString(), LoanAbacusAPIModelSurvey[].class);
					List<LoanAbacusAPIModelSurvey> acmSurveyList =
							new ArrayList<>(Arrays.asList(acmSurvey));
					// Update existing UDF
					for (LoanAbacusAPIModelSurvey abacusSurveyObject : abacusSurveyList) {
						// Get surveyID && userDefinedFieldGroupID
						int surveyID = abacusSurveyObject.getSurveyID();
						int userDefinedFieldGroupID =
								abacusSurveyObject.getUserDefinedFieldGroupID();
						// Find surveys by surveyID && userDefinedFieldGroupID
						LoanAbacusAPIModelSurvey acmSurveyObject = acmSurveyList.stream()
								.filter(s -> s.getSurveyID() == surveyID && s
										.getUserDefinedFieldGroupID() == userDefinedFieldGroupID)
								.findFirst().orElse(null);
						// Check if surveys exist
						if (acmSurveyObject != null) {
							List<LoanAbacusAPIModelUdfLink> abacusUDFs =
									abacusSurveyObject.getUdfLinks();
							List<LoanAbacusAPIModelUdfLink> acmUDFs = acmSurveyObject.getUdfLinks();
							for (LoanAbacusAPIModelUdfLink abacusUDF : abacusUDFs) {
								// Get udfFieldID
								int udfFieldID = abacusUDF.getUdfFieldID();
								// Find UDF by udfFieldID
								LoanAbacusAPIModelUdfLink acmUDFObject = acmUDFs.stream()
										.filter(s -> s.getUdfFieldID() == udfFieldID).findFirst()
										.orElse(null);
								// Check if UDF exist
								if (acmUDFObject != null) {
									abacusUDF.setValue(acmUDFObject.getValue());
								}
								else {
									abacusUDF.setValue("");
								}
							}
						}
						else {
							for (LoanAbacusAPIModelUdfLink abacusUDF : abacusSurveyObject
									.getUdfLinks()) {
								abacusUDF.setValue("");
							}
						}
					}
					// Add new UDF if exist
					for (LoanAbacusAPIModelSurvey acmSurveyObject : acmSurveyList) {
						// check survey ID = 0 => new udf group to add
						if (acmSurveyObject.getSurveyID() == 0) {
							abacusSurveyList.add(acmSurveyObject);
						}
					}
					JSONArray arrayResultatSurveys = new JSONArray(abacusSurveyList);
					logger.debug("*** array final Surveys = {} ***", arrayResultatSurveys);
					return arrayResultatSurveys;
				}
				catch (JsonParseException | JsonMappingException e) {
					logger.error("Error : JsonParseException | JsonMappingException");
					e.printStackTrace();
				}
				catch (IOException e) {
					logger.error("Error : IOException");
					e.printStackTrace();
				}
			}
		}
		return arraySurveys;
	}

	/**
	 * Gets the data.
	 *
	 * @param accountId the account id
	 * @return the data
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanCreateUpdateApiService#getData(java.lang.Long)
	 */
	@Override
	public LoanAbacusAPIModel getData(Long accountId)
			throws IOException, ApiAbacusException, URISyntaxException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			logger.info("************ REQUEST : GET DATA API ************");
			logger.info("ACCOUNTID = {}", accountId);
			logger.info("*************** GET DATA API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			String accessUrl = urlServeurApi + uriGetLoanData + accountId;
			// sending request to server using POST method
			URI uri = new URI(accessUrl);
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<LoanAbacusAPIModel> responseAPI =
					restTemplate.exchange(uri, HttpMethod.GET, request, LoanAbacusAPIModel.class);
			// check if response is not NULL
			if (responseAPI.getBody() != null) {
				logger.info("responseAPI.getLoan = {}", responseAPI.getBody());
				return responseAPI.getBody();
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" get Loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" get Loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occured while consuming get LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return null;
	}

	/**
	 * Send tranverse historique.
	 * 
	 * @author hchaouachi
	 * @param objectValue the object value
	 * @param methode the methode
	 * @param uri the uri
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 */
	void sendTranverseHistorique(String objectValue, String methode, String uri, String status,
			String requestValue, String responseValue) {

		TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(objectValue,
				methode, uri, status, requestValue, requestValue);
		logger.info("transversHistorique add Customer = {}", transversHistoriqueDTO.toString());
		// add history of request and response api create customer in transvers historique
		// table
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
			creditClient.create(transversHistoriqueDTO);
		}

	}

	/**
	 * Send tranverse historique.
	 *
	 * @param objectValue the object value
	 * @param methode the methode
	 * @param uri the uri
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 * @param token the token
	 */
	void sendTranverseHistorique(String objectValue, String methode, String uri, String status,
			String requestValue, String responseValue, String token) {

		TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(objectValue,
				methode, uri, status, requestValue, requestValue);
		logger.info("transversHistorique add Customer = {}", transversHistoriqueDTO.toString());
		// add history of request and response api create customer in transvers historique
		// table
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
			creditClient.create(transversHistoriqueDTO, token);
		}

	}

	/**
	 * Convert dto to json object.
	 *
	 * @param object the object
	 * @return the string
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	String convertDtoToJsonObject(Object object)
			throws IllegalArgumentException, IllegalAccessException {

		// Create a new JSONObject
		JSONObject jsonObject = new JSONObject();

		// Iterate over the fields of the object and add them to the JSONObject
		Field[] fields = object.getClass().getDeclaredFields();
		for (Field field : fields) {
			field.setAccessible(true);
			Object value = field.get(object);
			jsonObject.put(field.getName(), value);
		}

		// Convert the JSONObject to a String
		return JSONValue.toJSONString(jsonObject);
	}

	/**
	 * Creates the journal entry.
	 *
	 * @param journalEnteriesToAbacus the journal enteries to abacus
	 * @return the string
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.LoanCreateUpdateApiService#createJournalEntry(com.acm.utils.dtos.
	 * JournalEnteriesToAbacusDTO)
	 */
	@Override
	public String createJournalEntry(JournalEnteriesToAbacusDTO journalEnteriesToAbacus)
			throws KeyManagementException, KeyStoreException, NoSuchAlgorithmException,
			URISyntaxException, IllegalArgumentException, IllegalAccessException {

		RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

		// init headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

		// setting token
		headers = loginApiService.settingHttpHeaders(headers);

		// build body object (JSON)
		logger.info("************ REQUEST : SAVE LOAN GROUP ************");
		// logger.info("{}", loanJsonObject);
		logger.info("************* SAVE LOAN GROUP ********************");
		// init request

		HttpEntity<String> request =
				new HttpEntity<>(convertDtoToJsonObject(journalEnteriesToAbacus), headers);

		// init URI
		String accessUrl = urlServeurApi + uriPostJournalEntry;
		URI uri = new URI(accessUrl);
		logger.info("uri create LOAN GRP = {}", uri);
		// sending request to server using POST method
		ResponseEntity<String> responseAPI =
				restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
		logger.debug("{}", responseAPI.getBody());
		JSONObject respJsonObj = new JSONObject(responseAPI.getBody());

		JSONObject jsonObject = new JSONObject();
		jsonObject.put("journalPageID", respJsonObj.get("journalPageID"));
		jsonObject.put("journalID", 3);
		jsonObject.put("isPostAndClose", true);
		request = new HttpEntity<>(jsonObject.toString(), headers);
		accessUrl = urlServeurApi + uriPostJournalPage;
		uri = new URI(accessUrl);
		ResponseEntity<String> responseAPIJournalPage =
				restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
		logger.debug("{}", responseAPIJournalPage.getBody());

		return convertDtoToJsonObject(journalEnteriesToAbacus);
	}

	/**
	 * Find informations charge off.
	 *
	 * @param accountId the account id
	 * @return the response info charge off DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.LoanCreateUpdateApiService#findInformationsChargeOff(java.lang.
	 * Long)
	 */
	@Override
	public ResponseInfoChargeOffDTO findInformationsChargeOff(Long accountId)
			throws IOException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			logger.info("************ REQUEST : GET DATA API ************");
			logger.info("ACCOUNTID = {}", accountId);
			logger.info("*************** GET DATA API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			logger.info("GET Request findInformationsChargeOff = {}", request);
			String accessUrl = urlServeurApi + "/api/receipt/initialize/" + accountId
					+ "?ReceiptTypeSearch=256";
			// sending request to server using POST method
			URI uri = new URI(accessUrl);
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<ResponseInfoChargeOffDTO> responseAPI = restTemplate.exchange(uri,
					HttpMethod.GET, request, ResponseInfoChargeOffDTO.class);

			if (responseAPI.getBody() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
						"findInformationsChargeOff", RequestMethode.GET.name(), uri.toString(),
						responseAPI.getStatusCode().toString(), request.toString(),
						responseAPI.getBody().toString());

				logger.info("responseAPI of findInformationsChargeOff = {}", responseAPI.getBody());
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}

				return responseAPI.getBody();
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" get Loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" get Loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occured while consuming get LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return null;

	}

	/**
	 * Charge off abacus.
	 *
	 * @param chargeOffDTO the charge off DTO
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.LoanCreateUpdateApiService#chargeOffAbacus(com.acm.utils.dtos.
	 * ResponseInfoChargeOffDTO)
	 */
	@Override
	public ResponseEntity<String> chargeOffAbacus(ResponseInfoChargeOffDTO chargeOffDTO)
			throws IOException, ApiAbacusException, URISyntaxException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();
			// setting token
			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			logger.info("************ REQUEST : POST DATA API ************");
			logger.info("*************** POST DATA API ******************");

			// create request body
			HttpEntity<ResponseInfoChargeOffDTO> request = new HttpEntity<>(chargeOffDTO, headers);

			String accessUrl = urlServeurApi + "/api/transaction/loan/chargedoff";

			// sending request to server using POST method
			URI uri = new URI(accessUrl);
			logger.info("uri POST DATA  = {}", uri);
			ResponseInfoChargeOffDTO requestChargeOff = request.getBody();
			ObjectMapper objectMapper = new ObjectMapper();

			logger.info("BODY chargeOffAbacus  = {}",
					objectMapper.writeValueAsString(requestChargeOff));

			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);

			// check if response is not NULL
			if (responseAPI.getBody() != null) {
				logger.info("responseAPI of ChargeOff = {}", responseAPI.getBody());
				TransversHistoriqueDTO transversHistoriqueDTO =
						new TransversHistoriqueDTO("ChargeOff", RequestMethode.POST.name(),
								uri.toString(), responseAPI.getStatusCode().toString(),
								objectMapper.writeValueAsString(requestChargeOff),
								responseAPI.getBody().toString());

				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}

				return ResponseEntity.status(responseAPI.getStatusCode())
						.contentType(MediaType.APPLICATION_JSON).body(responseAPI.getBody());
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" post CHARGE OFF RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" post CHARGE OFF ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
		}

		return null;

	}

	/**
	 * Read list data csv file charge off abacus.
	 *
	 * @param listDataCsvDTO the list data csv DTO
	 * @return the response entity
	 * @throws KeyManagementException the key management exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingWorkFlowService#readListDataCsvFileChargeOffAbacus(java.util.List)
	 */
	@Override
	public ResponseEntity<String> readListDataCsvFileChargeOffAbacus(
			List<ReadFileCsvDTO> listDataCsvDTO) throws KeyManagementException, ApiAbacusException,
			KeyStoreException, NoSuchAlgorithmException, IOException, URISyntaxException {

		ResponseEntity<String> repChargeOff = null;
		for (ReadFileCsvDTO dataCsvDTO : listDataCsvDTO) {

			// call 1st api to get informations charge off
			ResponseInfoChargeOffDTO response =
					findInformationsChargeOff(Long.parseLong(dataCsvDTO.getCuAccountId()));
			response.setChargeOffProductID(Integer.parseInt(dataCsvDTO.getChargeOffProductId()));
			response.setOriginalProductID(Integer.parseInt(dataCsvDTO.getOrginalProductId()));
			response.setFees(new ArrayList<>());

			// call 2nd api post to do transaction charged off
			repChargeOff = chargeOffAbacus(response);

		}
		return repChargeOff;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanCreateUpdateApiService#paymentLoan(com.acm.utils.dtos.
	 * PaymentApiAbacusDTO, java.lang.String, java.lang.String)
	 */
	@Override
	public Boolean paymentLoan(PaymentApiAbacusDTO paymentApiAbacusDTO, String username,
			String paymentFrom) throws IOException, ApiAbacusException {

		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		HttpEntity<String> request = null;
		try {

			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers, username, paymentFrom);

			// init request
			// build body object (JSON)
			request = new HttpEntity<>(convertDtoToJsonObject(paymentApiAbacusDTO), headers);

			logger.info("************ REQUEST : POST PAYMENT API ************");
			logger.info("HTTP REQUEST Body : {}", request.getBody());
			logger.info("************* POST PAYMENT API ********************");

			// init URI
			String accessUrl = urlServeurApi + uriPostPaymentLoan;
			URI uri = new URI(accessUrl);
			logger.info("uri post payment loan = {}", uri);
			// sending request to server using POST method
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.debug("{}", responseAPI.getBody());

			JSONObject jsonObject = new JSONObject();
			jsonObject.put("amount", paymentApiAbacusDTO.getAmount());
			jsonObject.put("notes", paymentApiAbacusDTO.getNotes());
			jsonObject.put("accountNumber", paymentApiAbacusDTO.getAccountNumber());
			jsonObject.put("payfee", true);
			request = new HttpEntity<>(jsonObject.toString(), headers);

			logger.info("************ REQUEST : POST PAYMENT LOAN ************");
			logger.info("HTTP REQUEST Body : {}", request.getBody());
			logger.info("************* POST PAYMENT LOAN ********************");

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(),
					accessUrl, String.valueOf(responseAPI.getStatusCodeValue()), request.toString(),
					responseAPI.getBody(), token);
			return responseAPI.getStatusCodeValue() == 201;

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" payment Loan  RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" payment Loan  ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			String accessUrl = urlServeurApi + uriPostPaymentLoan;

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(),
					accessUrl, String.valueOf(e.getRawStatusCode()), request.toString(),
					e.getResponseBodyAsString(), token);
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException
				| IllegalArgumentException | IllegalAccessException | URISyntaxException e) {
			logger.error("Error has been occurred while consuming PAYMENT LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.LoanCreateUpdateApiService#findInformationsPayment(java.lang.Long)
	 */
	@Override
	public ResponseGetInfoPaymentAbacusDTO findInformationsPayment(Long accountId)
			throws IOException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			logger.info("************ REQUEST : GET DATA API ************");
			logger.info("ACCOUNTID = {}", accountId);
			logger.info("*************** GET DATA API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			logger.info("GET Request find Informations Payment = {}", request);
			String accessUrl = urlServeurApi + "/api/loan/payments/" + accountId;
			// sending request to server using GET method
			URI uri = new URI(accessUrl);
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<ResponseGetInfoPaymentAbacusDTO> responseAPI = restTemplate.exchange(uri,
					HttpMethod.GET, request, ResponseGetInfoPaymentAbacusDTO.class);

			if (responseAPI.getBody() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
						"find Informations Payment", RequestMethode.GET.name(), uri.toString(),
						responseAPI.getStatusCode().toString(), request.toString(),
						responseAPI.getBody().toString());

				logger.info("responseAPI of findInformationsPayment = {}", responseAPI.getBody());
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}

				return responseAPI.getBody();
			}
		}
		catch (RestClientResponseException e) {
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occured while consuming get LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return null;

	}

	/**
	 * Receipt initialize by cu account id.
	 *
	 * @param requestPayment the request payment
	 * @return true, if successful
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@Override
	public boolean receiptInitializeByCuAccountId(RequestPaymentApiAbacusDTO requestPayment)
			throws IOException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			logger.info("************ REQUEST : GET DATA API ************");
			logger.info("ACCOUNTID = {}", requestPayment.getCuAccountId());
			logger.info("*************** GET DATA API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			logger.info("GET Request find recepit Informations = {}", request);
			String accessUrl = urlServeurApi + "/api/receipt/initialize/"
					+ requestPayment.getCuAccountId() + "?ReceiptTypeSearch=2";
			// sending request to server using GET method
			URI uri = new URI(accessUrl);
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.GET, request, String.class);

			if (responseAPI.getBody() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
						"find recepit Informations", RequestMethode.GET.name(), uri.toString(),
						responseAPI.getStatusCode().toString(), request.toString(),
						responseAPI.getBody().toString());

				logger.info("responseAPI of find recepit Informations = {}", responseAPI.getBody());
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}

				// call second api payment
				return paymentLoanUsingPaymentAbacusApi(responseAPI.getBody(), requestPayment);
			}
		}
		catch (RestClientResponseException e) {
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occured while consuming get LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return false;

	}

	/**
	 * Payment loan using payment abacus api.
	 *
	 * @param responseReceiptInformation the response receipt information
	 * @param requestPayment the request payment
	 * @return the boolean
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	public Boolean paymentLoanUsingPaymentAbacusApi(String responseReceiptInformation,
			RequestPaymentApiAbacusDTO requestPayment) throws IOException, ApiAbacusException {

		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		HttpEntity<String> request = null;
		try {

			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers, requestPayment.getUsername(),
					requestPayment.getPaymentFrom());

			// Parse the JSON string into a map
			ObjectMapper objectMapper = new ObjectMapper();
			@SuppressWarnings("unchecked")
			Map<String, Object> requestMap =
					objectMapper.readValue(responseReceiptInformation, Map.class);

			// Override the value of the "notes" attribute
			requestMap.put("notes", requestPayment.getNotes());

			// Convert the modified map back to a JSON string
			String modifiedJson = objectMapper.writeValueAsString(requestMap);

			logger.info("************* modifiedJson************* = {}", modifiedJson);

			// init request
			request = new HttpEntity<>(modifiedJson, headers);

			logger.info("************ REQUEST : POST PAYMENT2 API ************");
			logger.info("HTTP REQUEST Body : {}", request.getBody());
			logger.info("************* POST PAYMENT2 API ********************");

			// init URI
			String accessUrl = urlServeurApi + "/api/transaction/loan/payment";
			URI uri = new URI(accessUrl);
			logger.info("uri post payment2 loan = {}", uri);
			// sending request to server using POST method
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.debug("{}", responseAPI.getBody());

			logger.info("************ responseAPI : POST PAYMENT2 LOAN ************");
			logger.info("HTTP responseAPI Body : {}", responseAPI.getBody());

			sendTranverseHistorique("PAYMENT API", RequestMethode.POST.name(), accessUrl,
					String.valueOf(responseAPI.getStatusCodeValue()), request.toString(),
					responseAPI.getBody(), token);
			return responseAPI.getStatusCodeValue() == 201;

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" payment2 Loan  RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" payment2 Loan  ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			String accessUrl = urlServeurApi + uriPostPaymentLoan;

			sendTranverseHistorique(TransversHistoryObject.LOAN.name(), RequestMethode.POST.name(),
					accessUrl, String.valueOf(e.getRawStatusCode()), request.toString(),
					e.getResponseBodyAsString(), token);
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				if (arrayJsonNode.isNull()) {
					errorMsgAbacus = object.get("message").asText();
				}
				else {
					for (final JsonNode objNode : arrayJsonNode) {
						errorMsgAbacus = objNode.get("message").asText();
					}
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException
				| IllegalArgumentException | URISyntaxException e) {
			logger.error(
					"Error has been occurred while consuming PAYMENT2 LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

}
