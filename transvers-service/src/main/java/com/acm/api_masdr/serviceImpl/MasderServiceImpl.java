/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_masdr.serviceImpl;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;

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

import com.acm.api_masdr.service.MasdrService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.EmploymentStatusInfoMasdrAPIDTO;
import com.acm.utils.dtos.ResponseMasderApiTokenDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;

/**
 * The Class MasdrApiServiceImpl.
 */
@Service
public class MasderServiceImpl implements MasdrService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(MasderServiceImpl.class);

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The rest template. */
	private RestTemplate restTemplate = new RestTemplate();

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_masdr.service.MasdrService#getToken()
	 */
	@Override
	public String getToken() {

		String apiUrl = "https://sandbox.b2b.masdr.sa/";
		String password = "mA2cMOvSCj4jP1zU";
		String userName = "zaA0qnZXH48e3IwRrKolADA3WzgonmSw";

		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MASDR_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MASDR_API_REQUEST_URL", apiUrl);
		password = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MASDR_API_REQUEST_PASSWORD", password);
		userName = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MASDR_API_REQUEST_USERNAME", userName);

		String finalUrl = apiUrl + "token/v1/accesstoken?grant_type=client_credentials";
		// Encode username and password in Base64
		String credentials = userName + ":" + password;
		String encodedCredentials =
				Base64.getEncoder().encodeToString(credentials.getBytes(StandardCharsets.UTF_8));

		HttpHeaders headers = new HttpHeaders();
		headers.set("Authorization", "Basic " + encodedCredentials);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		try {
			logger.info(
					"INIT MASDR Request For GET_TOKEN API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, requestEntity);

			ResponseEntity<ResponseMasderApiTokenDTO> responseEntity = restTemplate.exchange(
					finalUrl, HttpMethod.POST, requestEntity, ResponseMasderApiTokenDTO.class);

			// check if response is not NULL
			if (responseEntity.getBody() != null) {
				thirdPartyHistoDTO.setCategory("MASDR/TOKEN");
				thirdPartyHistoDTO
						.setRequestValue("username: " + userName + "password: " + password);
				thirdPartyHistoDTO.setResponseValue(responseEntity.getBody().toString());
				thirdPartyHistoDTO.setStatus(String.valueOf(responseEntity.getStatusCodeValue()));
				creditClient.create(thirdPartyHistoDTO);
			}

			logger.info("### INIT MASDR Response For GET_TOKEN API = [{}] ### ", responseEntity);
			return responseEntity.getBody().getAccessToken();
		}

		catch (Exception e) {
			logger.info("### Exception  For MASDR API GET_TOKEN API = [{}] ### ", e.getMessage());
			return null;
		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_masdr.service.MasdrService#mofeedApi(java.lang.String, java.lang.Long)
	 */
	@Override
	public ResponseEntity<EmploymentStatusInfoMasdrAPIDTO> mofeedApi(String identity, Long loanId) {

		String apiUrl = "https://sandbox.b2b.masdr.sa/";

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MASDR_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MASDR_API_REQUEST_URL", apiUrl);
		String finalUrl = apiUrl + "mofeed/employment/v1/employee/employment-status/" + identity;

		HttpHeaders headers = new HttpHeaders();
		headers.set("Accept", "application/json");
		headers.set("Authorization", "Bearer " + getToken());

		HttpEntity<?> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT MASDR Request For MOFEED API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.GET, requestEntity);
			ResponseEntity<EmploymentStatusInfoMasdrAPIDTO> response = restTemplate.exchange(
					finalUrl, HttpMethod.GET, requestEntity, EmploymentStatusInfoMasdrAPIDTO.class);
			thirdPartyHistoDTO.setCategory("MASDR/MOFEED");
			thirdPartyHistoDTO.setRequestValue("identity: " + identity);
			thirdPartyHistoDTO.setResponseValue(response.toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);
			logger.info("### INIT MASDR Response For MOFEED API = [{}] ### ", response);

			return ResponseEntity.ok(response.getBody());
		}
		catch (Exception e) {
			logger.info("### Exception For MASDR API MOFEED API = [{}] ### ", e.getMessage());
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

}
