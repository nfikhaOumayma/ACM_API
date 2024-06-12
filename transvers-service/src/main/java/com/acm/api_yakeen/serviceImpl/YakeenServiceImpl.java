/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_yakeen.serviceImpl;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.api_yakeen.service.YakeenService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The Class YakeenServiceImpl.
 */
@Service
public class YakeenServiceImpl implements YakeenService {

	/** The rest template. */
	private final RestTemplate restTemplate = new RestTemplate();

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(YakeenServiceImpl.class);

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_yakeen.service.YakeenService#Authentication(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public ResponseEntity<String> authentication(String username, String password) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("YAKEEN_API_REQUEST"));
		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");

		JSONObject requestBody = new JSONObject();
		requestBody.put("Username", username);
		requestBody.put("Password", password);

		HttpEntity<String> requestEntity = new HttpEntity<>(requestBody.toString(), headers);
		String apiUrl = "https://783c9b54-b9fa-487c-9982-dc4f2a93c36f.mock.pstmn.io";
		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_URL", apiUrl);
		try {
			ResponseEntity<String> response = restTemplate.exchange(apiUrl + "/api/v1/yakeen/login",
					HttpMethod.GET, requestEntity, String.class);
			thirdPartyHistoDTO.setCategory("Yakeen/Authentication");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue(response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);
			return response;

		}
		catch (Exception e) {
			// Handle any exceptions or errors here
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_yakeen.service.YakeenService#saudiByPassportOrNin(java.lang.String,
	 * java.lang.String, java.lang.String, java.util.Date)
	 */
	@Override
	public ResponseEntity<String> saudiByPassportOrNin(String passportNo, String passportExpiryDate,
			String nin, Date birthDateG) throws IOException {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("YAKEEN_API_REQUEST"));
		String username = "abc";
		String password = "abcd";

		username = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_USERNAME", username);
		password = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_PASSWORD", password);
		HttpHeaders headers = new HttpHeaders();
		ResponseEntity<String> responseAuthentication = this.authentication(username, password);

		ObjectMapper objectMapper = new ObjectMapper();
		JsonNode jsonNode = objectMapper.readTree(responseAuthentication.getBody());

		String authorization = jsonNode.get("Authorization").asText();

		// SETTING default valus
		headers.set("Authorization", authorization);
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");

		String serviceIdentifier = "390cda69-046c-4251-a89c-b9a4edb8e335";
		String usageCode = "USC-00002";
		String operatorId = "100001010";
		String appId = "app-id";
		String appKey = "app-key";

		// GETTING config
		serviceIdentifier = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_SERVICE_IDENTIFIER", serviceIdentifier);
		usageCode = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_USAGE_CODE", usageCode);
		operatorId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_OPERATOR_ID", operatorId);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_APP_ID", appId);
		appKey = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_APP_KEY", appKey);

		headers.add("app-id", appId);
		headers.add("app-key", appKey);
		headers.add("Operator-Id", operatorId);
		headers.add("Usage-usagecode", usageCode);
		headers.add("Service-Identifier", serviceIdentifier);

		JSONObject requestBody = new JSONObject();
		requestBody.put("passportNo", passportNo);
		requestBody.put("passportExpiryDate", passportExpiryDate);
		requestBody.put("nin", nin);
		requestBody.put("birthDateG", birthDateG);

		HttpEntity<String> requestEntity = new HttpEntity<>(requestBody.toString(), headers);
		String apiUrl = "https://783c9b54-b9fa-487c-9982-dc4f2a93c36f.mock.pstmn.io";
		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"YAKEEN_API_REQUEST_URL", apiUrl);
		try {
			ResponseEntity<String> response = restTemplate.exchange(

					apiUrl + "/api/v1/yakeen/data", HttpMethod.GET, requestEntity, String.class);
			thirdPartyHistoDTO.setCategory("Yakeen/saudiByPassportOrNin");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue(response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);
			return response;

		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}
}
