/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_simah.serviceImpl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.api_simah.service.SimahService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AuthResponseSimahApiDTO;
import com.acm.utils.dtos.LoginRequestSimahApiDTO;
import com.acm.utils.dtos.RequestEnquiryNewCustomerSimahApiDTO;
import com.acm.utils.dtos.RequestGetScoreDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;

/**
 * The Class SimahServiceImpl.
 */
@Service
public class SimahServiceImpl implements SimahService {

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The rest template. */
	private final RestTemplate restTemplate = new RestTemplate();

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SimahServiceImpl.class);

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_simah.service.SimahService#getToken()
	 */
	@Override
	public ResponseEntity<AuthResponseSimahApiDTO> getToken() {

		String apiUrl = "https://f61019d6-01cf-4c14-9c0e-e1851d54ab6b.mock.pstmn.io";
		String username = "tgffadmin";
		String password = "tgffadmin_123";

		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("SIMAH_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"SIMAH_API_REQUEST_URL", apiUrl);
		username = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"SIMAH_API_REQUEST_USERNAME", username);
		password = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"SIMAH_API_REQUEST_PASSWORD", password);

		LoginRequestSimahApiDTO loginRequest = new LoginRequestSimahApiDTO();
		loginRequest.setUsername(username);
		loginRequest.setPassword(password);

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.set("Accept", "application/json");

		String finalUrl = apiUrl + "/api/v1/Identity/login";

		// Create the HTTP entity with the request body and headers
		HttpEntity<LoginRequestSimahApiDTO> request = new HttpEntity<>(loginRequest, headers);

		try {
			logger.info(
					"INIT SIMAH Request For GET_TOKEN API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP POST request
			ResponseEntity<AuthResponseSimahApiDTO> response = restTemplate.exchange(finalUrl,
					HttpMethod.POST, request, AuthResponseSimahApiDTO.class);

			thirdPartyHistoDTO.setCategory("SIMAH/TOKEN");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT SIMAH Response For GET_TOKEN API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			logger.info("### Exception Message For GET_TOKEN API = [{}] ### ", e.getMessage());
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_simah.service.SimahService#getScore(com.acm.api_simah.dtos.RequestGetScoreDTO)
	 */
	@Override
	public ResponseEntity<String> getScore(RequestGetScoreDTO requestBody) {

		String apiUrl = "https://spapiuat.simah.com";

		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("SIMAH_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"SIMAH_API_REQUEST_URL", apiUrl);

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();

		// get token
		String token = getToken().getBody().getData().getToken();

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.set("Accept", "application/json");

		headers.set("Authorization", token);

		String finalUrl = apiUrl + "/api/v2/enquiry/consumer/score";

		// Create the HTTP entity with the request body and headers
		HttpEntity<RequestGetScoreDTO> request = new HttpEntity<>(requestBody, headers);

		try {
			logger.info(
					"INIT SIMAH Request For GET_SCORE API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP POST request
			ResponseEntity<String> response =
					restTemplate.exchange(finalUrl, HttpMethod.POST, request, String.class);

			thirdPartyHistoDTO.setCategory("SIMAH/SCORE");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT SIMAH Response For GET_SCORE API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			logger.info("### Exception Message For GET_SCORE API = [{}] ### ", e.getMessage());
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_simah.service.SimahService#enquiryCustomer(com.acm.utils.dtos.
	 * RequestEnquiryNewCustomerSimahApiDTO)
	 */
	@Override
	public ResponseEntity<String> enquiryCustomer(RequestEnquiryNewCustomerSimahApiDTO requestBody,
			Long loanId) {

		// String apiUrl = "https://spapiuat.simah.com";
		String apiUrl = "https://f61019d6-01cf-4c14-9c0e-e1851d54ab6b.mock.pstmn.io";

		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("SIMAH_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"SIMAH_API_REQUEST_URL", apiUrl);

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();

		// get token
		String token = getToken().getBody().getData().getToken();

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.set("Accept", "application/json");
		headers.set("Authorization", token);

		String finalUrl = apiUrl + "/api/v2/enquiry/consumer/new";

		// Create the HTTP entity with the request body and headers
		HttpEntity<RequestEnquiryNewCustomerSimahApiDTO> request =
				new HttpEntity<>(requestBody, headers);

		try {
			logger.info(
					"INIT SIMAH Request For ENQUIRY NEW CUSTOMER API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP POST request
			ResponseEntity<String> response =
					restTemplate.exchange(finalUrl, HttpMethod.POST, request, String.class);

			thirdPartyHistoDTO.setCategory("SIMAH/EnquiryNewCustomer");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT SIMAH Response For ENQUIRY NEW CUSTOMER API = [{}] ### ",
					response);

			return response;
		}
		catch (Exception e) {
			logger.info("### Exception Message For ENQUIRY NEW CUSTOMER API = [{}] ### ",
					e.getMessage());
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

}
