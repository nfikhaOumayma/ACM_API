/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_Mobishastra.serviceImpl;

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

import com.acm.api_Mobishastra.service.MobishastraService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;

/**
 * The Class MobishastraServiceImpl.
 */
@Service
public class MobishastraServiceImpl implements MobishastraService {

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(MobishastraServiceImpl.class);

	/** The rest template. */
	private final RestTemplate restTemplate = new RestTemplate();

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	

	/**
	 * Send sms otp.
	 *
	 * @param number the number
	 * @param msg the msg
	 * @return the response entity
	 */
	@Override
	public ResponseEntity<String> sendSmsOtp(String number, String msg) {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MOBISHATRA_API_REQUEST"));
		HttpHeaders headers = new HttpHeaders();

		// SETTING default valus
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		String user = "user";
		String pwd = "pwd";
		String sender = "sender";
		String language = "language";

		user = CommonFunctions.findConfigurationFromList(environnementDTOs, "MOBISHASTRA_API_REQUEST_USER", user);
		sender = CommonFunctions.findConfigurationFromList(environnementDTOs, "MOBISHASTRA_API_REQUEST_SENDER",
				sender);
		pwd = CommonFunctions.findConfigurationFromList(environnementDTOs, "MOBISHASTRA_API_REQUEST_PWD", pwd);
		language = CommonFunctions.findConfigurationFromList(environnementDTOs, "MOBISHASTRA_API_REQUEST_LANGUAGE",
				language);

		JSONObject requestBody = new JSONObject();
		requestBody.put("user", user);
		requestBody.put("pwd", pwd);
		requestBody.put("number", number);
		requestBody.put("sender", sender);
		requestBody.put("msg", msg);
		requestBody.put("language", language);

		HttpEntity<String> requestEntity = new HttpEntity<>(requestBody.toString(), headers);
		String apiUrl = "https://28847415-5334-4999-93dc-04f788533132.mock.pstmn.io";
		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs, "MOBISHASTRA_API_REQUEST_URL",
				apiUrl);
		try {
			ResponseEntity<String> response = restTemplate.exchange(

					apiUrl + "/sendsms_api_json.aspx", HttpMethod.GET, requestEntity, String.class);
			thirdPartyHistoDTO.setCategory("MobiShastra/sendOtp");
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
