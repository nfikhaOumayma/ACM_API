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
import com.acm.api_abacus.service.LoanCancelApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link LoanCancelApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.10
 */
@Service
public class LoanCancelApiServiceImpl implements LoanCancelApiService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanCancelApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri cancel loan. */
	@Value("${rest.api.abacus.loan.cancel.uri}")
	private String uriCancelLoan;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#cancel(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void cancel(LoanDTO loanDTO) throws IOException, ApiAbacusException {

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
			JSONObject cancelJsonObject = new JSONObject();
			cancelJsonObject.put(MetaDataAPIAbacus.CANCEL_REASON_ID.fieldName(),
					loanDTO.getCodeExternMotifRejet());

			/** If reason motif is null of ibLoan => set motif id 1 as default */
			if (loanDTO.getCodeExternMotifRejet().equals(0)
					|| loanDTO.getCodeExternMotifRejet().equals(null)
							&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getIdIbLoan())) {
				loanDTO.setCodeExternMotifRejet(1);
			}

			// init Notes object
			JSONObject notesJsonObject = new JSONObject();
			notesJsonObject.put(MetaDataAPIAbacus.NOTE.fieldName(), loanDTO.getNote());
			cancelJsonObject.put(MetaDataAPIAbacus.NOTES.fieldName(), notesJsonObject);

			logger.info("************ REQUEST : CANCEL API ************");
			logger.info("{}", cancelJsonObject);
			logger.info("************** CANCEL API *******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(cancelJsonObject.toString(), headers);

			// init URI
			String accessUrl = urlServeurApi + uriCancelLoan + loanDTO.getIdLoanExtern();
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using PUT method
			restTemplate.put(uri, request);
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error("cancel : RawStatusCode =  {}", e.getRawStatusCode());
			logger.error("cancel : ResponseBodyAsString = {}", e.getResponseBodyAsString());
			String responseBody = e.getResponseBodyAsString().substring(1);
			responseBody = responseBody.substring(0, responseBody.length() - 1);
			// mapping result
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(responseBody);
			String exceptionID = node.path("exceptionID").asText();
			String message = node.path("message").asText();
			logger.error("Error exceptionID = {}", exceptionID);
			logger.error("Error message = {}", message);
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, message);
		}
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException
				| URISyntaxException e) {
			logger.error("Error has been occurred while cancelling Loan using ABACUS API. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
	}
}
