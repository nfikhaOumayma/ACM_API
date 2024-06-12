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
import java.util.List;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.acm.api_abacus.model.MetaDataAPIAbacus;
import com.acm.api_abacus.service.LoanCalculateApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.models.transvers.LoanScheduleAPI;
import com.acm.utils.number.NumberUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link LoanCalculateApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
@Service
public class LoanCalculateApiServiceImpl implements LoanCalculateApiService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanCalculateApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri calculate schedules. */
	@Value("${rest.api.abacus.loan.calculate.uri}")
	private String uriCalculateSchedules;

	/** The uri cal culate eir. */
	@Value("${rest.api.abacus.loan.calculate.effectiveinterestrate.uri}")
	private String uriCalCulateEir;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#calculate(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanScheduleAPI calculate(LoanDTO loanDTO) throws IOException, ApiAbacusException {

		LoanScheduleAPI loanScheduleAPI = new LoanScheduleAPI();
		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			// init EffectiveInterestRateStandard using API Calcultate EIR
			if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getProductDTO().getFlatInterestRate())
					&& loanDTO.getProductDTO().getFlatInterestRate()
							.compareTo(BigDecimal.ZERO) != 0) {
				// Get Effective_Interest_Rate_Standard value using calculate API EIR
				LoanScheduleAPI eir = calculateEffectiveInterestRateStandard(loanDTO, restTemplate);
				// Setting value to be used in calculate API
				loanDTO.getProductDTO().setRate(eir.getEffectiveInterestRateStandard());
			}
			else if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomRate())) {
				loanDTO.getProductDTO().setRate(loanDTO.getCustomRate());

			}

			// build body object (JSON)
			JSONObject calculateJsonObject = initCalculateBodyParams(loanDTO, Boolean.FALSE);
			logger.info("************ REQUEST : CALCULATE API ************");
			logger.info("{}", calculateJsonObject);
			logger.info("*************** CALCULATE API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(calculateJsonObject.toString(), headers);
			String accessUrl = urlServeurApi + uriCalculateSchedules;
			// sending request to server using POST method
			URI uri = new URI(accessUrl);
			logger.info("uri CALCULATE  = {}", uri);
			ResponseEntity<LoanScheduleAPI> responseAPI =
					restTemplate.postForEntity(uri, request, LoanScheduleAPI.class);
			logger.debug("{}", responseAPI.getBody());
			// check if response is not NULL
			if (responseAPI != null && responseAPI.getBody() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO =
						new TransversHistoriqueDTO(TransversHistoryObject.CALCULATE_SCHEDULE.name(),
								RequestMethode.POST.name(), uri.toString(),
								responseAPI.getStatusCode().toString(), responseAPI.toString(),
								responseAPI.getBody().toString());
				logger.info("transversHistorique Calculate Schedule = {}",
						transversHistoriqueDTO.toString());
				// add history of request and response api Calculate Schedule in transvers
				// historique
				// table
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}
				// mapping result
				logger.debug("{}", responseAPI.getBody().getLoanSchedule());
				// setting Schedule list
				List<ScheduleDTO> scheduleDTOs =
						ACMValidationUtils.isNullOrEmpty(responseAPI.getBody().getLoanSchedule())
								? new ArrayList<>()
								: responseAPI.getBody().getLoanSchedule();
				// calculate total
				BigDecimal totalRepayment = BigDecimal.ZERO;
				BigDecimal totalLoanRepayment = BigDecimal.ZERO;
				BigDecimal totalInterestRepayment = BigDecimal.ZERO;
				for (ScheduleDTO scheduleDTO : scheduleDTOs) {
					totalRepayment = totalRepayment.add(scheduleDTO.getTotalRepayment());
					totalLoanRepayment = totalLoanRepayment.add(scheduleDTO.getLoanRepayment());
					totalInterestRepayment =
							totalInterestRepayment.add(scheduleDTO.getInterestRepayment());
				}
				// setting Total
				scheduleDTOs.add(new ScheduleDTO(totalRepayment, totalLoanRepayment,
						totalInterestRepayment));
				logger.info("returned list of scheduleDTOs = {}", scheduleDTOs.size());
				return new LoanScheduleAPI(scheduleDTOs,
						NumberUtils.roundBigDecimal(responseAPI.getBody().getApr(), 2,
								BigDecimal.ROUND_HALF_EVEN),
						responseAPI.getBody().getMaturityDate(),
						responseAPI.getBody().getInitialPaymentDate(),
						responseAPI.getBody().getNormalPayment(),
						NumberUtils.roundBigDecimal(
								responseAPI.getBody().getEffectiveInterestRate(), 2,
								BigDecimal.ROUND_HALF_EVEN),
						responseAPI.getBody().getInsurancePremium(),
						responseAPI.getBody().getIssueAmount(),
						responseAPI.getBody().getEffectiveInterestRateStandard(),
						NumberUtils.roundBigDecimal(responseAPI.getBody().getInterestRate(), 2,
								BigDecimal.ROUND_HALF_EVEN),
						responseAPI.getBody().getIssueFee(), responseAPI.getBody().getFeeAmt1());
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error("calculate : RawStatusCode =  {}", e.getRawStatusCode());
			logger.error("calculate : ResponseBodyAsString = {}", e.getResponseBodyAsString());
			String responseBody = e.getResponseBodyAsString().substring(1);
			responseBody = responseBody.substring(0, responseBody.length() - 1);
			// mapping result
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(responseBody);
			logger.error("Error message = {}", node.path("message").asText());
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, node.path("message").asText());
		}
		catch (URISyntaxException | KeyManagementException | KeyStoreException
				| NoSuchAlgorithmException e) {
			logger.error("Error has been occurred while consuming calculate API from ABACUS. {}",
					e.getMessage());
			loanScheduleAPI = new LoanScheduleAPI(new ArrayList<>(), BigDecimal.ZERO, null, null,
					BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO,
					BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO);
		}
		return loanScheduleAPI;
	}

	/**
	 * Calculate effective interest rate standard.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param restTemplate the rest template
	 * @return the loan schedule API
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	private LoanScheduleAPI calculateEffectiveInterestRateStandard(LoanDTO loanDTO,
			RestTemplate restTemplate) throws IOException, ApiAbacusException, URISyntaxException {

		LoanScheduleAPI loanScheduleAPI = new LoanScheduleAPI();
		// init headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
		// setting token
		headers = loginApiService.settingHttpHeaders(headers);

		// build body object (JSON)
		JSONObject calculateEIRJsonObject = initCalculateBodyParams(loanDTO, Boolean.TRUE);
		logger.info("************ REQUEST : CALCULATE EIR API ************");
		logger.info("{}", calculateEIRJsonObject);
		logger.info("*************** CALCULATE EIR API ******************");
		// init request
		HttpEntity<String> request = new HttpEntity<>(calculateEIRJsonObject.toString(), headers);

		// init URI dynamic rate with uriCalculate EIR or calculte api with interest rate fix
		String accessUrl = urlServeurApi + uriCalCulateEir;
		URI uri = new URI(accessUrl);
		logger.info("uri  CALCULATE EIR  = {}", uri);
		// sending request to server using POST method
		ResponseEntity<LoanScheduleAPI> responseCalculateEIRAPI =
				restTemplate.postForEntity(uri, request, LoanScheduleAPI.class);
		logger.debug("{}", responseCalculateEIRAPI.getBody());
		// check if response is not NULL
		if (responseCalculateEIRAPI.getBody() != null) {
			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.CALCULATE_EIR.name(), RequestMethode.POST.name(),
					uri.toString(), responseCalculateEIRAPI.getStatusCode().toString(),
					responseCalculateEIRAPI.toString(),
					responseCalculateEIRAPI.getBody().toString());
			logger.info("transversHistorique Calculate EIR = {}",
					transversHistoriqueDTO.toString());
			// add history of request and response api Calculate EIR in transvers historique
			// table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
			return new LoanScheduleAPI(new ArrayList<>(),
					responseCalculateEIRAPI.getBody().getApr(),
					responseCalculateEIRAPI.getBody().getMaturityDate(),
					responseCalculateEIRAPI.getBody().getInitialPaymentDate(),
					responseCalculateEIRAPI.getBody().getNormalPayment(),
					responseCalculateEIRAPI.getBody().getEffectiveInterestRate(),
					responseCalculateEIRAPI.getBody().getInsurancePremium(),
					responseCalculateEIRAPI.getBody().getIssueAmount(),
					responseCalculateEIRAPI.getBody().getEffectiveInterestRateStandard(),
					responseCalculateEIRAPI.getBody().getInterestRate(),
					responseCalculateEIRAPI.getBody().getIssueFee(),
					responseCalculateEIRAPI.getBody().getFeeAmt1());
		}
		return loanScheduleAPI;
	}

	/**
	 * Inits the calculate body params.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param calculateEIR the calculate EIR
	 * @return the JSON object
	 */
	private JSONObject initCalculateBodyParams(LoanDTO loanDTO, Boolean calculateEIR) {

		JSONObject calculateJsonObject = new JSONObject();
		calculateJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
				loanDTO.getProductDTO().getProductIdAbacus() != null
						? loanDTO.getProductDTO().getProductIdAbacus()
						: JSONObject.NULL);
		// default customer = 1
		calculateJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
				loanDTO.getCustomerDTO().getCustomerIdExtern() != null
						&& loanDTO.getCustomerDTO().getCustomerIdExtern() != 0
								? loanDTO.getCustomerDTO().getCustomerIdExtern()
								: 1);
		calculateJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
				loanDTO.getCustomerDTO().getAccountPortfolioID() != null
						? loanDTO.getCustomerDTO().getAccountPortfolioID()
						: 1);
		calculateJsonObject.put(MetaDataAPIAbacus.CU_INSURANCE_ID.fieldName(),
				loanDTO.getProductDTO().getCuInsuranceID());

		// init loanApp object
		JSONObject loanAppJsonObject = new JSONObject();
		// init loanPart object
		JSONObject loanPartJsonObject = new JSONObject();
		loanPartJsonObject.put(MetaDataAPIAbacus.USE_SCHEDULE_INTEREST.fieldName(),
				loanDTO.getProductDTO().getUseScheduleInterest() != null
						? loanDTO.getProductDTO().getUseScheduleInterest()
						: JSONObject.NULL);
		loanPartJsonObject.put(MetaDataAPIAbacus.INTEREST_RATE.fieldName(),
				loanDTO.getProductDTO().getRate());
		// used only in calculate EIR
		if (Boolean.TRUE.equals(calculateEIR)) {
			loanPartJsonObject.put(MetaDataAPIAbacus.FLAT_RATE_INTEREST_RATE.fieldName(),
					loanDTO.getProductDTO().getFlatInterestRate());
		}
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
		loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD_NUM.fieldName(),
				loanDTO.getPaymentFreq() != null ? loanDTO.getPaymentFreq() : 1);
		loanPartJsonObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);

		loanPartJsonObject.put(MetaDataAPIAbacus.INTPAY_PERIOD_NUM.fieldName(),
				loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);

		loanPartJsonObject.put(MetaDataAPIAbacus.IGNORE_ODD_DAYS.fieldName(),
				loanDTO.getIgnoreOddDays() != null ? loanDTO.getIgnoreOddDays() : Boolean.TRUE);
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
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : JSONObject.NULL);
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
		// default value = 0
		loanPartJsonObject.put(MetaDataAPIAbacus.EFFECTIVE_INT_RATE.fieldName(), 0);
		// BigDecimal vat1 =
		// (loanDTO.getProductDTO().getInsuranceVat().multiply(loanDTO.getApprovelAmount()))
		// .divide(new BigDecimal(100));
		// BigDecimal amountVat1 = loanDTO.getApprovelAmount().add(vat1);
		// // issue fee with % fee
		// BigDecimal feeAmount1 =
		// (loanDTO.getProductDTO().getIssueFeePercentage1().multiply(amountVat1))
		// .divide(new BigDecimal(100));
		// // issue fee with fix fee amount
		// BigDecimal feeAmount1WithFixFee =
		// feeAmount1.add(loanDTO.getProductDTO().getIssueFeeAmount1());
		// loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_1.fieldName(),
		// loanDTO.getProductDTO().getIssueFeePercentage1() != null ? feeAmount1WithFixFee
		// : 0);
		// BigDecimal vat2 =
		// (loanDTO.getProductDTO().getInsuranceVat().multiply(loanDTO.getApprovelAmount()))
		// .divide(new BigDecimal(100));
		//
		// BigDecimal amountVat2 = loanDTO.getApprovelAmount().add(vat2);
		// BigDecimal feeAmount2 =
		// (loanDTO.getProductDTO().getIssueFeePercentage2().multiply(amountVat2))
		// .divide(new BigDecimal(100));
		// BigDecimal feeAmount2WithFixFee =
		// feeAmount2.add(loanDTO.getProductDTO().getIssueFeeAmount2());
		// loanPartJsonObject.put(MetaDataAPIAbacus.FEE_AMOUNT_2.fieldName(),
		// loanDTO.getProductDTO().getIssueFeePercentage2() != null ? feeAmount2WithFixFee
		// : 0);
		//
		// loanPartJsonObject.put(MetaDataAPIAbacus.ISSUE_FEE.fieldName(),
		// loanDTO.getProductDTO().getIssueFeePercentage1() != null
		// && loanDTO.getProductDTO().getIssueFeePercentage2() != null
		// ? loanDTO.getIssueFeeAmount()
		// : 0);
		// Default value 1 for BRJMF
		loanPartJsonObject.put(MetaDataAPIAbacus.DAY_COUNT.fieldName(), 2);

		loanAppJsonObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPartJsonObject);
		calculateJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanAppJsonObject);
		return calculateJsonObject;
	}
}
