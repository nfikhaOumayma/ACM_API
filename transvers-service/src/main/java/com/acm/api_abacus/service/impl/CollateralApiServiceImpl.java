/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service.impl;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;
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
import com.acm.api_abacus.service.CollateralApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link CollateralApiServiceImpl } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class CollateralApiServiceImpl implements CollateralApiService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(CollateralApiServiceImpl.class);
	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri add guarantor. */
	@Value("${rest.api.abacus.loan.collateral.add.uri}")
	private String uriAddCollateral;
	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.CollateralApiService#save(com.acm.utils.dtos.AcmCollateralDTO)
	 */
	@Override
	public List<AcmCollateralDTO> save(LoanDTO loanDTO)
			throws IOException, ApiAbacusException, URISyntaxException {

		// init URI
		String accessUrl = urlServeurApi + uriAddCollateral;
		URI uri = new URI(accessUrl);
		logger.info("uri = {}", uri);
		try {
			for (AcmCollateralDTO acmCollateralDTO : loanDTO.getCollaterals()) {

				// init resttemplate
				RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

				// init headers
				HttpHeaders headers = new HttpHeaders();
				headers.setContentType(MediaType.APPLICATION_JSON);
				headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

				// setting token
				headers = loginApiService.settingHttpHeaders(headers);

				// build body object (JSON)
				JSONObject collateralJsonObject = initJSONObjectAddCollateral(acmCollateralDTO);
				logger.info("************ REQUEST : ADD COLLATERAL ************");
				logger.info("{}", collateralJsonObject);
				logger.info("*************** ADD COLLATERAL ******************");
				// init request
				HttpEntity<String> request =
						new HttpEntity<>(collateralJsonObject.toString(), headers);

				// sending request to server using POST method
				ResponseEntity<String> responseAPI =
						restTemplate.postForEntity(uri, request, String.class);
				// mapping result
				logger.debug("{}", responseAPI.getBody());
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
						TransversHistoryObject.COLLATERAL.name(), RequestMethode.POST.name(),
						uri.toString(), responseAPI.getStatusCode().toString(),
						collateralJsonObject.toString(), responseAPI.getBody());
				logger.info("transversHistorique add collateral = {}", transversHistoriqueDTO);
				// add history of request and response api create customer in transvers historique
				// table
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}
			}
			return loanDTO.getCollaterals();
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" add guarantor RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" add guarantor ResponseBodyAsString = {}", e.getResponseBodyAsString());
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
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException e) {
			logger.error(
					"Error has been occurred while consuming add collateral API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}

	}

	/**
	 * Inits the JSON object add collateral.
	 *
	 * @author mlamloum
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the JSON object
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject initJSONObjectAddCollateral(AcmCollateralDTO acmCollateralDTO)
			throws IOException, ApiAbacusException, URISyntaxException {

		JSONObject collateralJsonObject = new JSONObject();
		JSONArray arrayLoanCollateral = new JSONArray();

		JSONObject loanCollateralJsonObject = new JSONObject();
		// default value = ""

		loanCollateralJsonObject.put(MetaDataAPIAbacus.CULOAN_ID.fieldName(),
				(acmCollateralDTO.getLoan() != null)
						&& (acmCollateralDTO.getLoan().getIdLoanExtern() != null)
								? acmCollateralDTO.getLoan().getIdLoanExtern()
								: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.REFERENCE.fieldName(),
				acmCollateralDTO.getReference() != null ? acmCollateralDTO.getReference() : "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
				acmCollateralDTO.getDescription() != null ? acmCollateralDTO.getDescription() : "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.CULOAN_COLLATERAL_TYPE_ID.fieldName(),
				acmCollateralDTO.getCollateralTypeIdExtern() != null
						? acmCollateralDTO.getCollateralTypeIdExtern()
						: 0);

		loanCollateralJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
				(acmCollateralDTO.getCustomer() != null)
						&& (acmCollateralDTO.getCustomer().getCustomerIdExtern() != null)
								? acmCollateralDTO.getCustomer().getCustomerIdExtern()
								: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.CUACCOUNT_ID.fieldName(),
				acmCollateralDTO.getIdAccountExtern() != null
						? acmCollateralDTO.getIdAccountExtern()
						: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.ORIGINAL_GROSS_VALUE.fieldName(),
				acmCollateralDTO.getOriginalGrossValue() != null
						? acmCollateralDTO.getOriginalGrossValue()
						: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.GROSS_VALUE.fieldName(),
				acmCollateralDTO.getGrossValue() != null ? acmCollateralDTO.getGrossValue() : "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.REALISED_VALUE.fieldName(),
				acmCollateralDTO.getRealisedValue() != null ? acmCollateralDTO.getRealisedValue()
						: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.FIXED_COST.fieldName(),
				acmCollateralDTO.getFixedCost() != null ? acmCollateralDTO.getFixedCost() : "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.NET_VALUE.fieldName(),
				acmCollateralDTO.getNetValue() != null ? acmCollateralDTO.getNetValue() : "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.VALUE_DATE.fieldName(),
				acmCollateralDTO.getValueDate() != null
						? DateUtil.formatDate(acmCollateralDTO.getValueDate(),
								"dd/MM/yyyy hh:mm:ss")
						: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.EXPIRY_DATE.fieldName(),
				acmCollateralDTO.getExpiryDate() != null
						? DateUtil.formatDate(acmCollateralDTO.getExpiryDate(),
								"dd/MM/yyyy hh:mm:ss")
						: "");

		loanCollateralJsonObject.put(MetaDataAPIAbacus.WITH_HOLDING_RATE.fieldName(),
				acmCollateralDTO.getWithHoldingRate() != null
						? acmCollateralDTO.getWithHoldingRate()
						: "");
		loanCollateralJsonObject.put(MetaDataAPIAbacus.VALUER_ID.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.VALUER_NAME.fieldName(), "");
		loanCollateralJsonObject.put(MetaDataAPIAbacus.WITH_HOLDING_RATE_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.GROSS_VALUE_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.FIXED_VALUE_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.REALISED_VALUE_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.COLLATERAL_TYPE_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.ACCOUNT_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.VALUER_CHANGED.fieldName(), 0);
		loanCollateralJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), new ArrayList<>());
		loanCollateralJsonObject.put(MetaDataAPIAbacus.COLLATERAL_ACTIVE.fieldName(), 1);
		arrayLoanCollateral.put(loanCollateralJsonObject);
		collateralJsonObject.put("", arrayLoanCollateral);

		return loanCollateralJsonObject;
	}

}
