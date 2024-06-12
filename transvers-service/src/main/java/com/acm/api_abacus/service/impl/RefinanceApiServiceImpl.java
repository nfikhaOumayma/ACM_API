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
import java.util.Arrays;
import java.util.Date;

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
import com.acm.api_abacus.service.LoginApiService;
import com.acm.api_abacus.service.RefinanceApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.service.LoanAbacusService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link RefinanceApiServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class RefinanceApiServiceImpl implements RefinanceApiService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(RefinanceApiServiceImpl.class);
	/** The credit client. */
	@Autowired
	private CreditClient creditClient;
	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri add customer. */
	@Value("${rest.api.abacus.refinance.add.uri}")
	private String uriAddRefinance;

	/** The loan abacus service. */
	@Autowired
	private LoanAbacusService loanAbacusService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.RefinanceApiService#save(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void save(LoanDTO loanDTO) throws IOException, ApiAbacusException {

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
			JSONObject refinanceLoanJsonObject = initJSONObjectRefinanceLoan(loanDTO);

			logger.info("************ REQUEST : ADD LOAN REFINANCE ************");
			logger.info("{}", refinanceLoanJsonObject);
			logger.info("**************** ADD LOAN REFINANCE *****************");
			// init request
			HttpEntity<String> request =
					new HttpEntity<>(refinanceLoanJsonObject.toString(), headers);

			// init URI
			String accessUrl = urlServeurApi + uriAddRefinance + loanDTO.getIdAccountExtern();
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using PUT method
			ResponseEntity<String> responseEntity =
					restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);

			// create transverse historique
			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.LOAN.name(), RequestMethode.PUT.name(), uri.toString(),
					responseEntity.getStatusCode().toString(), refinanceLoanJsonObject.toString(),
					responseEntity.getBody());
			logger.info("transversHistorique loan refinance saved");
			// add history of request put response api refinance loan in transvers historique table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" refianance/topup loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" refianance/topup loan ResponseBodyAsString = {}",
					e.getResponseBodyAsString());
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
			logger.error(
					"Error has been occured while consuming refianance/topup loan API in ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * Inits the JSON object refinance loan.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject initJSONObjectRefinanceLoan(LoanDTO loanDTO)
			throws ApiAbacusException, IOException, URISyntaxException {

		JSONObject refinanceLoanJsonObject = new JSONObject();
		// Customer Type INDIV = 1
		refinanceLoanJsonObject.put(MetaDataAPIAbacus.CU_LOAN_ID.fieldName(),
				loanDTO.getIdLoanExtern());

		// Creating a json array
		JSONObject loanApp = buildLoanAppJsonObject(loanDTO);
		refinanceLoanJsonObject.put(MetaDataAPIAbacus.LOAN_APP.fieldName(), loanApp);
		refinanceLoanJsonObject.put(MetaDataAPIAbacus.CU_ACCOUNT_ID.fieldName(),
				loanDTO.getIdAccountExtern());
		refinanceLoanJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
				loanDTO.getCustomerDTO().getCustomerIdExtern());
		refinanceLoanJsonObject.put(MetaDataAPIAbacus.PRODUCT_ID.fieldName(),
				loanDTO.getProductId());
		return refinanceLoanJsonObject;
	}

	/**
	 * Builds the loan app json object.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject buildLoanAppJsonObject(LoanDTO loanDTO)
			throws ApiAbacusException, IOException, URISyntaxException {

		// build json schema
		JSONObject loanAppJSONObject = new JSONObject();

		loanAppJSONObject.put(MetaDataAPIAbacus.CU_LOAN_ID.fieldName(), loanDTO.getIdLoanExtern());
		JSONObject loanPart = buildLoanPartJsonObject(loanDTO);
		loanAppJSONObject.put(MetaDataAPIAbacus.LOAN_PART.fieldName(), loanPart);
		loanAppJSONObject.put(MetaDataAPIAbacus.APPLY_AMOUNT_TOTAL.fieldName(),
				loanDTO.getApplyAmountTotal());
		loanAppJSONObject.put(MetaDataAPIAbacus.LOAN_REASON_ID.fieldName(),
				loanDTO.getLoanReasonId());
		loanAppJSONObject.put(MetaDataAPIAbacus.LOAN_SOURCE_OF_FUNDS_ID.fieldName(),
				loanDTO.getSourceOfFundsID());
		loanAppJSONObject.put(MetaDataAPIAbacus.APPLY_DATE.fieldName(), loanDTO.getApplyDate());

		return loanAppJSONObject;
	}

	/**
	 * Builds the loan part json object.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject buildLoanPartJsonObject(LoanDTO loanDTO)
			throws ApiAbacusException, IOException, URISyntaxException {

		// build json schema
		JSONObject loanPartJSONObject = new JSONObject();

		loanPartJSONObject.put(MetaDataAPIAbacus.USE_SCHEDULE_INTEREST.fieldName(),
				loanDTO.getProductDTO() != null
						&& loanDTO.getProductDTO().getUseScheduleInterest() != null
								? loanDTO.getProductDTO().getUseScheduleInterest()
								: JSONObject.NULL);
		loanPartJSONObject.put(MetaDataAPIAbacus.INTEREST_RATE.fieldName(),
				loanDTO.getProductRate());
		loanPartJSONObject.put(MetaDataAPIAbacus.LOAN_AMOUNT.fieldName(),
				loanDTO.getApprovelAmount());
		loanPartJSONObject.put(MetaDataAPIAbacus.REFINANCED.fieldName(), Boolean.TRUE);
		loanPartJSONObject.put(MetaDataAPIAbacus.ISSUE_DATE.fieldName(),
				loanDTO.getIssueDate() != null
						? java.sql.Date.valueOf(
								DateUtil.convertToLocalDateViaInstant(loanDTO.getIssueDate()))
						: java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
		loanPartJSONObject.put(MetaDataAPIAbacus.LEVEL_PAYMENTS.fieldName(), Boolean.TRUE);
		loanPartJSONObject.put(MetaDataAPIAbacus.INITIAL_PAYMENT_DATE.fieldName(),
				loanDTO.getInitialPaymentDate() != null
						? java.sql.Date.valueOf(DateUtil
								.convertToLocalDateViaInstant(loanDTO.getInitialPaymentDate()))
						: java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));
		loanPartJSONObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD_NUM.fieldName(),
				loanDTO.getPaymentFreq() != null ? loanDTO.getPaymentFreq() : 1);
		loanPartJSONObject.put(MetaDataAPIAbacus.RE_PAYMENT_PERIOD.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : 0);
		loanPartJSONObject.put(MetaDataAPIAbacus.INTPAY_PERIOD_NUM.fieldName(),
				loanDTO.getInterestFreq() != null ? loanDTO.getInterestFreq() : 1);
		loanPartJSONObject.put(MetaDataAPIAbacus.PERIODS_DEFERRED_ID.fieldName(),
				loanDTO.getPeriodsDeferredType());
		loanPartJSONObject.put(MetaDataAPIAbacus.TERM_PERIOD_NUM.fieldName(),
				loanDTO.getTermPeriodNum());
		loanPartJSONObject.put(MetaDataAPIAbacus.TERM_PERIOD.fieldName(),
				loanDTO.getTermPeriodID() != null ? loanDTO.getTermPeriodID() : JSONObject.NULL);
		// TODO : defalut value : null or boolean.true ?
		loanPartJSONObject.put(MetaDataAPIAbacus.CAPITALISE_INTEREST_WHEN_REFINANCING.fieldName(),
				loanDTO.getProductDTO() != null
						&& loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing() != null
								? loanDTO.getProductDTO().getCapitaliseInterestWhenRefinancing()
								: JSONObject.NULL);

		loanPartJSONObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE1.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage1() != null
						? loanDTO.getProductDTO().getIssueFeePercentage1()
						: 0);
		loanPartJSONObject.put(MetaDataAPIAbacus.ISSUE_FEE_PERCENTAGE2.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage2() != null
						? loanDTO.getProductDTO().getIssueFeePercentage2()
						: 0);

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

		loanPartJSONObject.put(MetaDataAPIAbacus.FEE_AMOUNT_1.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage1() != null ? feeAmount1WithFixFee
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
		loanPartJSONObject.put(MetaDataAPIAbacus.FEE_AMOUNT_2.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage2() != null ? feeAmount2WithFixFee
						: 0);

		loanPartJSONObject.put(MetaDataAPIAbacus.ISSUE_FEE.fieldName(),
				loanDTO.getProductDTO().getIssueFeePercentage1() != null
						&& loanDTO.getProductDTO().getIssueFeePercentage2() != null
								? loanDTO.getIssueFeeAmount()
								: 0);

		return loanPartJSONObject;
	}
}
