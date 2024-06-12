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
import com.acm.api_abacus.service.GuarantorAbacusApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link GuarantorAbacusApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class GuarantorAbacusApiServiceImpl implements GuarantorAbacusApiService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(GuarantorAbacusApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri add guarantor. */
	@Value("${rest.api.abacus.loan.guarantor.add.uri}")
	private String uriAddGuarantor;
	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.GuarantorAbacusApiService#save(java.util.List)
	 */
	@Override
	public void save(List<GuarantorDTO> guarantorDTOs) throws IOException, IllegalArgumentException,
			IllegalAccessException, ApiAbacusException {

		GuarantorDTO guarantorDTORespoense = new GuarantorDTO();
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
			JSONObject guarantorJsonObject = initJSONObjectAddGuarantors(guarantorDTOs);
			logger.info("************ REQUEST : ADD GUARANTOR ************");
			logger.info("{}", guarantorJsonObject);
			logger.info("*************** ADD GUARANTOR ******************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(guarantorJsonObject.toString(), headers);

			// init URI
			String accessUrl =
					urlServeurApi + uriAddGuarantor + guarantorDTOs.get(0).getAccountId();
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<String> responseAPI =
					restTemplate.postForEntity(uri, request, String.class);
			// mapping result
			logger.debug("{}", responseAPI.getBody());
			guarantorDTORespoense.setLoanId(guarantorDTOs.get(0).getLoanId());
			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.GUARANTOR.name(), RequestMethode.POST.name(),
					uri.toString(), responseAPI.getStatusCode().toString(),
					guarantorJsonObject.toString(), responseAPI.getBody());
			logger.info("transversHistorique add guarantor = {}", transversHistoriqueDTO);
			// add history of request and response api create customer in transvers historique table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" add guarantor RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" add guarantor ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl =
					urlServeurApi + uriAddGuarantor + guarantorDTOs.get(0).getAccountId();

			sendTranverseHistorique(TransversHistoryObject.GUARANTOR.name(),
					RequestMethode.POST.name(), accessUrl, String.valueOf(e.getRawStatusCode()),
					null, e.getResponseBodyAsString());

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
			logger.error(
					"Error has been occurred while consuming add guarantor API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#save(com.acm.utils.dtos.GuarantorDTO)
	 */
	@Override
	public GuarantorDTO save(GuarantorDTO guarantorDTO) throws IOException, ApiAbacusException {

		GuarantorDTO guarantorDTORespoense = new GuarantorDTO();
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
			JSONObject guarantorJsonObject = initJSONObjectAddGuarantor(guarantorDTO);
			logger.info("************ REQUEST : ADD GUARANTOR ************");
			logger.info("{}", guarantorJsonObject);
			logger.info("*************** ADD GUARANTOR ******************");
			// init request
			HttpEntity<String> request = new HttpEntity<>(guarantorJsonObject.toString(), headers);

			// init URI
			String accessUrl = urlServeurApi + uriAddGuarantor + guarantorDTO.getAccountId();
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<String> responseAPI =
					restTemplate.postForEntity(uri, request, String.class);
			// mapping result
			logger.debug("{}", responseAPI.getBody());
			guarantorDTORespoense.setLoanId(guarantorDTO.getLoanId());
			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.GUARANTOR.name(), RequestMethode.POST.name(),
					uri.toString(), responseAPI.getStatusCode().toString(),
					guarantorJsonObject.toString(), responseAPI.getBody());
			logger.info("transversHistorique add guarantor = {}", transversHistoriqueDTO);
			// add history of request and response api create customer in transvers historique table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
			return guarantorDTORespoense;
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" add guarantor RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" add guarantor ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl = urlServeurApi + uriAddGuarantor + guarantorDTO.getAccountId();

			sendTranverseHistorique(TransversHistoryObject.GUARANTOR.name(),
					RequestMethode.POST.name(), accessUrl, String.valueOf(e.getRawStatusCode()),
					null, e.getResponseBodyAsString());

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
			logger.error(
					"Error has been occurred while consuming add guarantor API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}

	/**
	 * Inits the JSON object add guarantor.
	 * 
	 * @author HaythemBenizid
	 * @param guarantorDTO the guarantor DTO
	 * @return the JSON object
	 */
	private JSONObject initJSONObjectAddGuarantor(GuarantorDTO guarantorDTO) {

		JSONObject guarantorJsonObject = new JSONObject();
		JSONArray arrayLoanGuarantor = new JSONArray();

		JSONObject loanGuarantorJsonObject = new JSONObject();
		// default value = ""
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.TYPE.fieldName(),
				guarantorDTO.getGuarantorType() != null ? guarantorDTO.getGuarantorType() : "");
		// get customer ID
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
				guarantorDTO.getCustomerId());
		// default value = ""
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
				guarantorDTO.getDescription() != null ? guarantorDTO.getDescription() : "");
		// pour BRJMF currencyID=1 => pas de configuration multi-currency
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.CURRENCY_ID.fieldName(),
				guarantorDTO.getCurrencyId() != null ? guarantorDTO.getCurrencyId() : 1);
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.AMOUNT.fieldName(), guarantorDTO.getAmount());
		// default value = 0 => pas de parametrage cote ABACUS
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR_TYPE_ID.fieldName(),
				guarantorDTO.getLoanGuarantorTypeId() != null
						? guarantorDTO.getLoanGuarantorTypeId()
						: 0);
		loanGuarantorJsonObject.put(MetaDataAPIAbacus.CHECK_ACTIVE.fieldName(), Boolean.TRUE);
		arrayLoanGuarantor.put(loanGuarantorJsonObject);
		guarantorJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR.fieldName(), arrayLoanGuarantor);

		return guarantorJsonObject;
	}

	/**
	 * Inits the JSON object add guarantors.
	 *
	 * @param guarantorDTOs the guarantor DT os
	 * @return the JSON object
	 */
	private JSONObject initJSONObjectAddGuarantors(List<GuarantorDTO> guarantorDTOs) {

		JSONObject guarantorJsonObject = new JSONObject();
		JSONArray arrayLoanGuarantor = new JSONArray();

		for (GuarantorDTO guarantorDTO : guarantorDTOs) {
			JSONObject loanGuarantorJsonObject = new JSONObject();
			// default value = ""
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.TYPE.fieldName(),
					guarantorDTO.getGuarantorType() != null ? guarantorDTO.getGuarantorType() : "");
			// get customer ID
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
					guarantorDTO.getCustomerId());
			// default value = ""
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.DESCRIPTION.fieldName(),
					guarantorDTO.getDescription() != null ? guarantorDTO.getDescription() : "");
			// pour BRJMF currencyID=1 => pas de configuration multi-currency
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.CURRENCY_ID.fieldName(),
					guarantorDTO.getCurrencyId() != null ? guarantorDTO.getCurrencyId() : 1);
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.AMOUNT.fieldName(),
					guarantorDTO.getAmount());
			// default value = 0 => pas de parametrage cote ABACUS
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR_TYPE_ID.fieldName(),
					guarantorDTO.getLoanGuarantorTypeId() != null
							? guarantorDTO.getLoanGuarantorTypeId()
							: 0);
			loanGuarantorJsonObject.put(MetaDataAPIAbacus.CHECK_ACTIVE.fieldName(), Boolean.TRUE);
			arrayLoanGuarantor.put(loanGuarantorJsonObject);
		}

		guarantorJsonObject.put(MetaDataAPIAbacus.LOAN_GUARANTOR.fieldName(), arrayLoanGuarantor);

		return guarantorJsonObject;
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
}
