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

import com.acm.api_abacus.service.LoanScheduleAPIService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanSchedulesApiDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The Class LoanScheduleAPIServiceImpl.
 */
@Service
public class LoanScheduleAPIServiceImpl implements LoanScheduleAPIService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(RefinanceApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri get loan schedules. */
	@Value("${rest.api.abacus.loan.schedules.uri}")
	private String uriGetLoanSchedules;

	/** The object mapper. */
	private ObjectMapper objectMapper;

	/**
	 * Instantiates a new loan schedule API service impl.
	 */
	public LoanScheduleAPIServiceImpl() {

		objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanScheduleAPIService#getAllSchedules(java.lang.String)
	 */
	@Override
	public List<LoanSchedulesApiDTO> getAllSchedules(String customerId)
			throws IOException, ApiAbacusException {

		List<LoanSchedulesApiDTO> loanSchedulesApiDTOs = new ArrayList<>();
		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			String accessUrl = urlServeurApi + uriGetLoanSchedules + customerId;
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);

			// init request
			HttpEntity<String> request = new HttpEntity<>("", headers);

			ResponseEntity<String> responseEntity =
					restTemplate.exchange(uri, HttpMethod.GET, request, String.class);

			logger.info("transversHistorique fetch loan schedules by customerId");

			if (!ACMValidationUtils.isNullOrEmpty(responseEntity.getBody())) {

				JsonNode jsonNode = objectMapper.readTree(responseEntity.getBody());

				loanSchedulesApiDTOs = objectMapper.readValue(responseEntity.getBody(),
						new TypeReference<List<LoanSchedulesApiDTO>>() {

						});
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

		return loanSchedulesApiDTOs;
	}

}
