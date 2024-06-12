/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_payment.serviceImpl;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.acm.api_payment.service.PaymentApiService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.PaymentApiSanadDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The Class PaymentApiServiceImpl.
 */
@Service
public class PaymentApiServiceImpl implements PaymentApiService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(PaymentApiServiceImpl.class);

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_payment.service.PaymentApiService#findTargetUrlAndPaymentId(com.acm.utils.dtos.
	 * PaymentApiSanadDTO)
	 */
	@Override
	public ResponseEntity<String> findTargetUrlAndPaymentId(PaymentApiSanadDTO paymentApiSanadDTO)
			throws IOException, KeyManagementException, KeyStoreException, NoSuchAlgorithmException,
			URISyntaxException, RestClientException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
			List<AcmEnvironnementDTO> environnementDTOs = parametrageClient
					.findLikeKey(new AcmEnvironnementDTO("PAYMENT_SANAD_API_REQUEST"));

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			logger.info("************ REQUEST : GET DATA API ************");
			logger.info("*************** GET DATA API ******************");

			// create request body
			HttpEntity<PaymentApiSanadDTO> request = new HttpEntity<>(paymentApiSanadDTO, headers);

			String apiUrl = "https://3323f6a5-9a9b-4531-aa61-1592dcfa1e34.mock.pstmn.io";
			apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
					"PAYMENT_SANAD_API_REQUEST_URL", apiUrl);

			// sending request to server using POST method
			URI uri = new URI(apiUrl + "/v1/payments/");
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);

			// check if response is not NULL
			if (responseAPI.getBody() != null) {
				logger.info("responseAPI.getBody = {}", responseAPI.getBody());

				thirdPartyHistoDTO.setCategory("PaymentApi/targetUrl");
				thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
				thirdPartyHistoDTO.setResponseValue(responseAPI.getBody());
				thirdPartyHistoDTO.setStatus(String.valueOf(responseAPI.getStatusCodeValue()));
				creditClient.create(thirdPartyHistoDTO);
				return ResponseEntity.status(responseAPI.getStatusCode())
						.contentType(MediaType.APPLICATION_JSON).body(responseAPI.getBody());
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" get TargetUrlAndPaymentId RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" get TargetUrlAndPaymentId ResponseBodyAsString = {}",
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
		}

		return null;

	}

}
