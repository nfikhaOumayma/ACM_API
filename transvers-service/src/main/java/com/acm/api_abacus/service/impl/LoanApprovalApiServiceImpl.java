/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service.impl;

import java.io.IOException;
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

import com.acm.api_abacus.model.MetaDataAPIAbacus;
import com.acm.api_abacus.service.LoanApprovalApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.service.LoanAbacusService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.LoanApprovalProcessDTO;
import com.acm.utils.dtos.LoanDTO;
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
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link LoanApprovalApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Service
public class LoanApprovalApiServiceImpl implements LoanApprovalApiService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanApprovalApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The loan abacus service. */
	@Autowired
	private LoanAbacusService loanAbacusService;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;
	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri loan approval part 1. */
	@Value("${rest.api.abacus.loan.approval.uri.part1}")
	private String uriLoanApprovalPart1;

	/** The uri loan approval part 2. */
	@Value("${rest.api.abacus.loan.approval.uri.part2}")
	private String uriLoanApprovalPart2;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#approvel(com.acm.utils.dtos. LoanDTO)
	 */
	@Override
	public void approvel(LoanDTO loanDTO)
			throws IOException, CheckApprovelLevelException, ApiAbacusException {

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
					loanDTO.getPaymentFreq() != null ? loanDTO.getPaymentFreq() : 1);
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
			// Round fees 
			loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_1.fieldName(),
					loanDTO.getProductDTO().getIssueFeePercentage1() != null ? NumberUtils.roundBigDecimal(feeAmount1WithFixFee, 2,
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
					loanDTO.getProductDTO().getIssueFeePercentage2() != null ? NumberUtils.roundBigDecimal(feeAmount2WithFixFee, 2,
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
			if (loanDTO.getLoanApprovalLevel() == 1 && loanApprovalProcessDTOs.size() > 1) {
				cuLoanProcessId = loanApprovalProcessDTOs.get(1).getCuLoanProcessID();
			}
			else if (loanDTO.getLoanApprovalLevel() == 2 && loanApprovalProcessDTOs.size() > 2) {
				cuLoanProcessId = loanApprovalProcessDTOs.get(2).getCuLoanProcessID();
			}
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESS_ID.fieldName(),
					cuLoanProcessId);
			// default Approval etape 1 =1 =>"loanApprovalGroup": 1,
			// default Approval etape 2 =2 =>"loanApprovalGroup": 2,
			if (loanDTO.getLoanApprovalLevel() != null && (loanDTO.getLoanApprovalLevel() == 1)) {
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
						loanDTO.getLoanApprovalLevel());
				// default => "description": "Loan Approval"
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
						"Loan : " + loanDTO.getAccountNumber() + " Approved");
				// default value of menuKey = cuLoansApprove
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(),
						"cuLoansApprove");
			}
			else if (loanDTO.getLoanApprovalLevel() != null
					&& (loanDTO.getLoanApprovalLevel() == 2)) {
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
						JSONObject.NULL);
				// default => "description": "Ready for Disbursement"
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
						"Ready for Disbursement");
				// default value of menuKey = null
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(),
						JSONObject.NULL);
			}

			// default =>"isCompleted": true,
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_COMPLETED.fieldName(), Boolean.TRUE);
			// default =>"reference": "",
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.REFERENCE.fieldName(), "");
			// default =>"isChanged": true,
			cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_CHANGED.fieldName(), Boolean.TRUE);
			arrayCuLoanProcesses.put(cuLoanProcessesJSONObject);
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESSES.fieldName(),
					arrayCuLoanProcesses);

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
			// Default value of status = 2 => approvel in ABACUS
			loanAppJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 2);
			loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);

			// Creating a json array surveys
			JSONArray arraySurveys = buildSurveysJsonObject(loanDTO);
			loanJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);

			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					loanDTO.getCustomerDTO().getCustomerIdExtern());
			loanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
					loanDTO.getProductDTO().getProductIdAbacus());
			// TODO a verifie avec soumya
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
					loanDTO.getPortfolioId() != null ? loanDTO.getPortfolioId() : 0);
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_INDUSTRY_CODE_ID.fieldName(), 0);
			loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTO.getApr() != null ? loanDTO.getApr() : 0);

			logger.info("************ REQUEST : APPROVAL API ************");
			logger.info("{}", loanJsonObject);
			logger.info("************** APPROVAL API INDIV / ORG *******************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// init URI
			// setting ID loan in URI => /api/loan/12/processes (CULoanID)
			String accessUrl = urlServeurApi + uriLoanApprovalPart1 + loanDTO.getIdLoanExtern()
					+ uriLoanApprovalPart2;
			URI uri = new URI(accessUrl);
			logger.info("approvel uri = {}", uri);
			// sending request to server using PUT method
			restTemplate.put(uri, request);
			ResponseEntity<String> responseEntity =
					restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);
			if (loanDTO.getLoanApprovalLevel() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO();
				if (loanDTO.getLoanApprovalLevel() == 1) {
					// set approval level 1

					transversHistoriqueDTO = new TransversHistoriqueDTO(
							TransversHistoryObject.APPROVAL_LEVEL_1.name(),
							RequestMethode.PUT.name(), uri.toString(),
							responseEntity.getStatusCode().toString(), loanJsonObject.toString(),
							responseEntity.getBody());
					logger.info("transversHistorique approval level 1 = {}",
							transversHistoriqueDTO);

				}
				else if (loanDTO.getLoanApprovalLevel() == 2) {
					// set approval level 2
					transversHistoriqueDTO = new TransversHistoriqueDTO(
							TransversHistoryObject.APPROVAL_LEVEL_2.name(),
							RequestMethode.PUT.name(), uri.toString(),
							responseEntity.getStatusCode().toString(), loanJsonObject.toString(),
							responseEntity.getBody());
					logger.info("transversHistorique approval level 2 = {}",
							transversHistoriqueDTO);
				}
				// add history of request and response api update customer in transvers
				// historique table
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}
			}

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" Approval Loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" Approval Loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl = urlServeurApi + uriLoanApprovalPart1 + loanDTO.getIdLoanExtern()
					+ uriLoanApprovalPart2;
			if (loanDTO.getLoanApprovalLevel() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO();
				if (loanDTO.getLoanApprovalLevel() == 1) {
					// set approval level 1
					sendTranverseHistorique(TransversHistoryObject.APPROVAL_LEVEL_1.name(),
							RequestMethode.PUT.name(), accessUrl,
							String.valueOf(e.getRawStatusCode()), null,
							e.getResponseBodyAsString());
					logger.info("transversHistorique approval level 1 = {}",
							transversHistoriqueDTO);

				}
				else if (loanDTO.getLoanApprovalLevel() == 2) {
					// set approval level 2
					sendTranverseHistorique(TransversHistoryObject.APPROVAL_LEVEL_2.name(),
							RequestMethode.PUT.name(), accessUrl,
							String.valueOf(e.getRawStatusCode()), null,
							e.getResponseBodyAsString());
					logger.info("transversHistorique approval level 2 = {}",
							transversHistoriqueDTO);
				}
			}

			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				for (final JsonNode objNode : arrayJsonNode) {
					errorMsgAbacus = objNode.get("message").asText();
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occurred :: URISyntaxException : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#approvelGroup(java.util.List)
	 */
	@Override
	public void approvelGroup(List<LoanDTO> loanDTOs)
			throws IOException, CheckApprovelLevelException, ApiAbacusException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// filtering loan group & child list
			LoanDTO loanDTOGroup = new LoanDTO();
			List<LoanDTO> childs = new ArrayList<>();
			for (LoanDTO loanDTO : loanDTOs) {
				if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
					loanDTOGroup = loanDTO;
				}
				else {
					// setting child
					childs.add(loanDTO);
				}
			}

			JSONObject loanJsonObject = new JSONObject();
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					loanDTOGroup.getCustomerDTO().getCustomerIdExtern());
			loanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
					loanDTOGroup.getProductDTO().getProductIdAbacus());
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
					loanDTOGroup.getPortfolioId() != null ? loanDTOGroup.getPortfolioId() : 0);
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_INDUSTRY_CODE_ID.fieldName(), 0);
			loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTOGroup.getApr() != null ? loanDTOGroup.getApr() : 0);
			// init loanApp object
			JSONObject loanAppJsonObject = new JSONObject();
			// init loanPart object
			JSONObject loanPartJsonObject = new JSONObject();
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);

			// Creating a json array cuLoanProcesses
			JSONArray arrayCuLoanProcesses = new JSONArray();
			for (LoanDTO loanDTO : childs) {

				JSONObject cuLoanProcessesJSONObject = new JSONObject();

				// find approval process from ABACUS by id loan extern
				List<LoanApprovalProcessDTO> loanApprovalProcessDTOs =
						loanAbacusService.findApprovalProcess(loanDTO.getIdLoanExtern());
				// check if process is null or empty
				if (ACMValidationUtils.isNullOrEmpty(loanApprovalProcessDTOs)) {
					throw new CheckApprovelLevelException(
							new ExceptionResponseMessage(
									CommonErrorCode.CHECK_APP_L1_CODE_DATA_NOT_FOUND,
									CommonExceptionsMessage.APPROVAL_PROCESS_FAILED,
									new TechnicalException()),
							CommonExceptionsMessage.APPROVAL_PROCESS_FAILED);
				}
				// setting CuLoanID
				Long cuLoanProcessId = 0L;
				if (loanDTO.getLoanApprovalLevel() == 1 && !loanApprovalProcessDTOs.isEmpty()) {
					cuLoanProcessId = loanApprovalProcessDTOs.get(0).getCuLoanProcessID();
				}
				else if (loanDTO.getLoanApprovalLevel() == 2
						&& loanApprovalProcessDTOs.size() >= 2) {
					cuLoanProcessId = loanApprovalProcessDTOs.get(1).getCuLoanProcessID();
				}
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESS_ID.fieldName(),
						cuLoanProcessId);

				// default Approval etape 1 =1 =>"loanApprovalGroup": 1,
				// default Approval etape 2 =2 =>"loanApprovalGroup": null,
				if (loanDTO.getLoanApprovalLevel() != null
						&& (loanDTO.getLoanApprovalLevel() == 1)) {
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
							loanDTO.getLoanApprovalLevel());
					// default => "description": "Loan Approval"
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
							"Loan : " + loanDTO.getAccountNumber() + " Approved");
					// default value of menuKey = cuLoansApprove
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(),
							"cuLoansApprove");
				}
				else if (loanDTO.getLoanApprovalLevel() != null
						&& (loanDTO.getLoanApprovalLevel() == 2)) {
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
							JSONObject.NULL);
					// default => "description": "Ready for Disbursement"
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
							"Ready for Disbursement");
					// default value of menuKey = null
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(),
							JSONObject.NULL);
				}

				// default =>"isCompleted": true,
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_COMPLETED.fieldName(),
						Boolean.TRUE);
				// default =>"reference": "",
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.REFERENCE.fieldName(), "");
				// default =>"isChanged": true,
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_CHANGED.fieldName(),
						Boolean.TRUE);

				arrayCuLoanProcesses.put(cuLoanProcessesJSONObject);
			}
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESSES.fieldName(),
					arrayCuLoanProcesses);

			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_REASON_ID.fieldName(),
					loanDTOGroup.getLoanReasonId());
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_SOURCE_OF_FUNDS_ID.fieldName(),
					loanDTOGroup.getSourceOfFundsID());
			// default value = 0
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_GUARANTOR_SOURCE_ID.fieldName(), 0);
			// default value = 0
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_DISTRICT_CODE_ID.fieldName(), 0);
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_REFINANCE_REASON_ID.fieldName(),
					loanDTOGroup.getRefinanceReasonId() != null
							? loanDTOGroup.getRefinanceReasonId()
							: 0);
			// Default value of status = 2 => approvel in ABACUS
			loanAppJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 2);
			loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);

			// Creating a json array communityLoans
			JSONArray arrayCommunityLoans = buildCommunityLoansJsonObject(childs);
			loanJsonObject.put(MetaDataAPIAbacus.COMMUNITY_LOANS.fieldName(), arrayCommunityLoans);

			// Creating a json array loanGuarantor
			JSONArray arrayLoanGuarantor = new JSONArray();
			loanJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR.fieldName(), arrayLoanGuarantor);

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			logger.info("************ REQUEST : APPROVAL API GRP ************");
			logger.info("{}", loanJsonObject);
			logger.info("************** APPROVAL API GRP *******************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// init URI
			// setting ID loan in URI => /api/loan/12/processes (CULoanID)
			String accessUrl = urlServeurApi + uriLoanApprovalPart1 + loanDTOGroup.getIdLoanExtern()
					+ uriLoanApprovalPart2;
			URI uri = new URI(accessUrl);
			logger.info("approvel GROUP uri = {}", uri);
			// sending request to server using PUT method
			restTemplate.put(uri, request);

			// at level 2 of approval => approve Group parent
			if (loanDTOGroup.getLoanApprovalLevel() == 2) {
				approvelGroup(loanDTOGroup, childs);
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" Approval Loan GRP RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" Approval Loan GRP ResponseBodyAsString = {}",
					e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				for (final JsonNode objNode : arrayJsonNode) {
					errorMsgAbacus = objNode.get("message").asText();
				}
			}
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, errorMsgAbacus);
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("An Error has been occurred : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * method called at approval level = 2 to Approve group parent after approving his childs.
	 *
	 * @author HaythemBenizid
	 * @param loanDTOGroup the loan DTO group
	 * @param childs the childs
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private void approvelGroup(LoanDTO loanDTOGroup, List<LoanDTO> childs)
			throws ApiAbacusException, IOException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			JSONObject loanJsonObject = new JSONObject();
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					loanDTOGroup.getCustomerDTO().getCustomerIdExtern());
			loanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
					loanDTOGroup.getProductDTO().getProductIdAbacus());
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
					loanDTOGroup.getPortfolioId() != null ? loanDTOGroup.getPortfolioId() : 0);
			loanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_INDUSTRY_CODE_ID.fieldName(), 0);
			loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTOGroup.getApr() != null ? loanDTOGroup.getApr() : 0);
			// init loanApp object
			JSONObject loanAppJsonObject = new JSONObject();
			// init loanPart object
			JSONObject loanPartJsonObject = new JSONObject();
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);

			// find approval process from ABACUS by id loan extern
			List<LoanApprovalProcessDTO> loanApprovalProcessDTOs =
					loanAbacusService.findApprovalProcess(loanDTOGroup.getIdLoanExtern());

			// Creating a json array cuLoanProcesses
			JSONArray arrayCuLoanProcesses = new JSONArray();
			Integer groupLoanApprovalLevel = 1;
			for (LoanApprovalProcessDTO loanApprovalProcessDTO : loanApprovalProcessDTOs) {

				JSONObject cuLoanProcessesJSONObject = new JSONObject();

				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESS_ID.fieldName(),
						loanApprovalProcessDTO.getCuLoanProcessID());
				// default Approval etape 1 =1 =>"loanApprovalGroup": 1,
				// default Approval etape 2 =2 =>"loanApprovalGroup": 2,
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
						groupLoanApprovalLevel);
				if (groupLoanApprovalLevel == 1) {
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
							groupLoanApprovalLevel);
					// default => "description": "Loan Approval"
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
							"Loan : " + loanDTOGroup.getAccountNumber() + " Approved");
					// default value of menuKey = cuLoansApprove
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(),
							"cuLoansApprove");
				}
				else if (groupLoanApprovalLevel == 2) {
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.LOAN_APPROVAL_GROUP.fieldName(),
							JSONObject.NULL);
					// default => "description": "Loan Approval"
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
							// default => "description": "Ready for Disbursement"
							"Ready for Disbursement");
					// default value of menuKey = null
					cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.MENU_KEY.fieldName(),
							JSONObject.NULL);
				}
				// default =>"isCompleted": true,
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_COMPLETED.fieldName(),
						Boolean.TRUE);
				// default =>"reference": "",
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.REFERENCE.fieldName(), "");
				// default =>"isChanged": true,
				cuLoanProcessesJSONObject.put(MetaDataAPIAbacus.IS_CHANGED.fieldName(),
						Boolean.TRUE);
				arrayCuLoanProcesses.put(cuLoanProcessesJSONObject);
				groupLoanApprovalLevel++;
			}
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_PROCESSES.fieldName(),
					arrayCuLoanProcesses);

			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_REASON_ID.fieldName(),
					loanDTOGroup.getLoanReasonId());
			loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_SOURCE_OF_FUNDS_ID.fieldName(),
					loanDTOGroup.getSourceOfFundsID());
			// default value = 0
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_GUARANTOR_SOURCE_ID.fieldName(), 0);
			// default value = 0
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_DISTRICT_CODE_ID.fieldName(), 0);
			loanAppJsonObject.put(MetaDataAPIAbacus.CU_LOAN_REFINANCE_REASON_ID.fieldName(),
					loanDTOGroup.getRefinanceReasonId() != null
							? loanDTOGroup.getRefinanceReasonId()
							: 0);
			// Default value of status = 2 => approvel in ABACUS
			loanAppJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 2);
			loanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);

			// Creating a json array communityLoans
			JSONArray arrayCommunityLoans = buildCommunityLoansJsonObject(childs);
			loanJsonObject.put(MetaDataAPIAbacus.COMMUNITY_LOANS.fieldName(), arrayCommunityLoans);

			// Creating a json array loanGuarantor
			JSONArray arrayLoanGuarantor = new JSONArray();
			loanJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR.fieldName(), arrayLoanGuarantor);

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			logger.info("************ REQUEST : APPROVAL 2 API GRP ************");
			logger.info("{}", loanJsonObject);
			logger.info("**************** APPROVAL 2 API GRP *****************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(loanJsonObject.toString(), headers);

			// init URI
			// setting ID loan in URI => /api/loan/12/processes (CULoanID)
			String accessUrl = urlServeurApi + uriLoanApprovalPart1 + loanDTOGroup.getIdLoanExtern()
					+ uriLoanApprovalPart2;
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using PUT method
			restTemplate.put(uri, request);
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" Approval Loan GRP RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" Approval Loan GRP ResponseBodyAsString = {}",
					e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());
			final JsonNode jsonNode = new ObjectMapper().readTree(e.getResponseBodyAsString());
			String errorMsgAbacus =
					"{\"errorMessage\":\" " + CommonExceptionsMessage.ERROR_API_ABACUS + "\"}";
			if (jsonNode.isArray() && jsonNode.size() > 0) {
				JsonNode object = jsonNode.get(1);
				final JsonNode arrayJsonNode = object.get("validationMessages");
				for (final JsonNode objNode : arrayJsonNode) {
					errorMsgAbacus = objNode.get("message").asText();
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
	 * Builds the community loans json object.
	 * 
	 * @author HaythemBenizid
	 * @param childs the loans
	 * @return the JSON array
	 */
	private JSONArray buildCommunityLoansJsonObject(List<LoanDTO> childs) {

		// build JSON schema
		JSONArray arrayCommunityLoans = new JSONArray();
		for (LoanDTO loanDTO : childs) {
			JSONObject loanJsonObject = new JSONObject();
			loanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					loanDTO.getCustomerDTO().getCustomerIdExtern());
			loanJsonObject.put(MetaDataAPIAbacus.APR.fieldName(),
					loanDTO.getApr() != null ? loanDTO.getApr() : 0);
			// Creating a json array surveys
			JSONArray arraySurveys = buildSurveysJsonObject(loanDTO);
			loanJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);

			// init loanApp object
			JSONObject loanAppJsonObject = new JSONObject();
			// init loanPart object
			JSONObject loanPartJsonObject = new JSONObject();
			loanPartJsonObject.put(MetaDataAPIAbacus.USE_SCHEDULE_INTEREST.fieldName(),
					loanDTO.getProductDTO().getUseScheduleInterest() != null
							? loanDTO.getProductDTO().getUseScheduleInterest()
							: JSONObject.NULL);
			loanPartJsonObject.put(MetaDataAPIAbacus.INTEREST_RATE.fieldName(),
					loanDTO.getProductDTO().getRate() != null ? loanDTO.getProductDTO().getRate()
							: JSONObject.NULL);
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
					loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);
			loanPartJsonObject.put(MetaDataAPIAbacus.INTPAY_PERIOD_NUM.fieldName(),
					loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);
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
					loanDTO.getProductDTO().getIssueFeePercentage1() != null ? NumberUtils.roundBigDecimal(feeAmount1WithFixFee, 2,
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
					loanDTO.getProductDTO().getIssueFeePercentage2() != null ? NumberUtils.roundBigDecimal(feeAmount2WithFixFee, 2,
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
	 * @return the JSON array
	 */
	private JSONArray buildSurveysJsonObject(LoanDTO loanDTO) {

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

				if (!ACMValidationUtils.isNullOrEmpty(udfFields.getSurveysId())) {
					// add surveys ID (cas UPDATE)
					surveysJSONObject.put(MetaDataAPIAbacus.SURVEY_ID.fieldName(),
							udfFields.getSurveysId());
				}

				// Default value = 0
				surveysJSONObject.put(MetaDataAPIAbacus.TOTAL_SCORE.fieldName(), 0);
				// build udfLinks
				JSONArray arrayUdfLinks = new JSONArray();
				Integer count = 1;
				for (UDFLinksGroupeFieldsModelDTO udfGroupeFieldsModel : udfFields
						.getUdfGroupeFieldsModels()) {
					JSONObject udfLinksJSONObject = new JSONObject();

					if (!ACMValidationUtils
							.isNullOrEmpty(udfGroupeFieldsModel.getIdAbacusUDFLink())) {
						// add UDF links ID (cas UPDATE)
						udfLinksJSONObject.put(MetaDataAPIAbacus.UDFLINK_ID.fieldName(),
								udfGroupeFieldsModel.getIdAbacusUDFLink());
					}
					else if (ACMValidationUtils
							.isNullOrEmpty(udfGroupeFieldsModel.getIdAbacusUDFLink())
							&& !ACMValidationUtils.isNullOrEmpty(udfGroupeFieldsModel.getValue())
							&& udfFields.getSurveysId() != null) {
						// add default UDF links ID = (-1 * count)
						udfLinksJSONObject.put(MetaDataAPIAbacus.UDFLINK_ID.fieldName(),
								count * -1);
						count++;
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
		return arraySurveys;
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
		// add history of request and response api create customer in transvers
		// historique
		// table
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
			creditClient.create(transversHistoriqueDTO);
		}

	}
}
