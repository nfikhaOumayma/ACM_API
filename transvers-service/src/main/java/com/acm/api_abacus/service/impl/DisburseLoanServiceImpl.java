/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service.impl;

import java.net.URI;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

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

import com.acm.api_abacus.service.DisburseLoanService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.DisburseDTO;
import com.acm.utils.dtos.DisburseResponse;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link DisburseLoanServiceImpl} class.
 *
 * @author kouali
 * @since 0.1.0
 */
@Service
public class DisburseLoanServiceImpl implements DisburseLoanService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(CustomerAbacusApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri disburse loan. */
	@Value("${rest.api.abacus.loan.diburse.uri}")
	private String uriDisburseLoan;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.DisburseLoanService#disburseLoan(com.acm.utils.dtos.DisburseDto)
	 */
	@Override
	public DisburseResponse disburseLoan(DisburseDTO disburseDto)
			throws URISyntaxException, ApiAbacusException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			// init request

			HttpEntity<String> request = new HttpEntity<>(
					CommonFunctions.convertObjectToJSONString(disburseDto), headers);

			logger.info("disburseDto = {}", CommonFunctions.convertObjectToJSONString(disburseDto));

			// init URI
			String accessUrl = urlServeurApi + uriDisburseLoan;
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			ResponseEntity<DisburseResponse> responseEntity =
					restTemplate.exchange(uri, HttpMethod.POST, request, DisburseResponse.class);

			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.LOAN_DISBURSE.name(), RequestMethode.POST.name(),
					uri.toString(), responseEntity.getStatusCode().toString(),
					CommonFunctions.convertObjectToJSONString(disburseDto),
					CommonFunctions.convertObjectToJSONString(responseEntity.getBody()));
			logger.info("transversHistorique add Customer = {}", transversHistoriqueDTO.toString());
			// // add history of request and response api create customer in transvers historique
			// table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
			return responseEntity.getBody();

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" Update customer RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" disburse loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

		}
		return null;

	}

}
