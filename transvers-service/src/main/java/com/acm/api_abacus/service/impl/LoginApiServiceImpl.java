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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.acm.api_abacus.model.LoginAPI;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.configuration.rest.RestTemplateConfig;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link LoginApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Service
public class LoginApiServiceImpl implements LoginApiService {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoginApiServiceImpl.class);

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri authentification. */
	@Value("${rest.api.abacus.authentification.uri}")
	private String uriAuthentification;

	/** The username. */
	@Value("${rest.api.abacus.authentification.login}")
	private String username;

	/** The password. */
	@Value("${rest.api.abacus.authentification.pass}")
	private String password;

	/** The device id. */
	@Value("${rest.api.abacus.authentification.device.id}")
	private String deviceId;

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoginApiService#loginAPIAbacus()
	 */
	@Override
	public LoginAPI loginAPIAbacus() throws IOException {

		logger.info("************ REQUEST : LOGIN API ************");
		try {
			// init RestTemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// build body object (JSON)
			JSONObject userJsonObject = new JSONObject();
			userJsonObject.put("deviceId", deviceId);
			userJsonObject.put("password", password);
			userJsonObject.put("username", username);

			// init request
			HttpEntity<String> request = new HttpEntity<>(userJsonObject.toString(), headers);

			// init URI
			String accessTokenUrl = urlServeurApi + uriAuthentification;
			URI uri = new URI(accessTokenUrl);
			LoginAPI newLoginAPI = null;
			// sending request to server using POST method
			ResponseEntity<LoginAPI> responseLoginAPI =
					restTemplate.postForEntity(uri, request, LoginAPI.class);
			// mapping result
			newLoginAPI = responseLoginAPI.getBody();
			logger.debug("login API Abacus = {}", newLoginAPI);
			return newLoginAPI;
		}
		catch (RestClientResponseException e) {
			e.printStackTrace();
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error("RawStatusCode =  {}", e.getRawStatusCode());
			logger.error("ResponseBodyAsString = {}", e.getResponseBodyAsString());
			// mapping result
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(e.getResponseBodyAsString());
			LoginAPI newLoginAPI = new LoginAPI(node.path("message").asText(),
					String.valueOf(e.getRawStatusCode()));
			logger.error("Error message = {}", newLoginAPI);
			return newLoginAPI;
		}
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException
				| URISyntaxException e) {
			logger.error("Error has been occurred while authenticating to ABACUS API. {}",
					e.getMessage());
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoginApiService#settingHttpHeaders(org.springframework.http.
	 * HttpHeaders)
	 */
	@Override
	public HttpHeaders settingHttpHeaders(HttpHeaders headers) {

		try {
			LoginAPI loginAPI = loginAPIAbacus();
			headers.set("X-Fern-Token", loginAPI.getToken());
		}
		catch (IOException e) {
			logger.error("Error has been occurred while getting token.");
		}
		return headers;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoginApiService#loginAPIAbacus(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public LoginAPI loginAPIAbacus(String usernameAbacus, String paymentFrom) throws IOException {

		logger.info("************ REQUEST : PAYMENT LOGIN API ************");
		try {
			// init RestTemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// build body object (JSON)
			JSONObject userJsonObject = new JSONObject();
			userJsonObject.put("deviceId", deviceId);
			userJsonObject.put("password",
					paymentFrom.equals("SANAD") ? password : "Talys@Dev@123Pay");
			userJsonObject.put("username", usernameAbacus);

			// init request
			HttpEntity<String> request = new HttpEntity<>(userJsonObject.toString(), headers);

			// init URI
			String accessTokenUrl = urlServeurApi + uriAuthentification;
			URI uri = new URI(accessTokenUrl);
			LoginAPI newLoginAPI = null;
			// sending request to server using POST method
			ResponseEntity<LoginAPI> responseLoginAPI =
					restTemplate.postForEntity(uri, request, LoginAPI.class);
			// mapping result
			newLoginAPI = responseLoginAPI.getBody();
			logger.debug("login API Abacus = {}", newLoginAPI);
			return newLoginAPI;
		}
		catch (RestClientResponseException e) {
			e.printStackTrace();
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error("RawStatusCode =  {}", e.getRawStatusCode());
			logger.error("ResponseBodyAsString = {}", e.getResponseBodyAsString());
			// mapping result
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(e.getResponseBodyAsString());
			LoginAPI newLoginAPI = new LoginAPI(node.path("message").asText(),
					String.valueOf(e.getRawStatusCode()));
			logger.error("Error message = {}", newLoginAPI);
			return newLoginAPI;
		}
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException
				| URISyntaxException e) {
			logger.error("Error has been occurred while authenticating to ABACUS API. {}",
					e.getMessage());
		}
		return null;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoginApiService#settingHttpHeaders(org.springframework.http.
	 * HttpHeaders, java.lang.String, java.lang.String)
	 */
	@Override
	public HttpHeaders settingHttpHeaders(HttpHeaders headers, String usernameAbacus,
			String paymentFrom) {

		try {
			LoginAPI loginAPI = loginAPIAbacus(usernameAbacus, paymentFrom);
			headers.set("X-Fern-Token", loginAPI.getToken());
		}
		catch (IOException e) {
			logger.error("Error has been occurred while getting token.");
		}
		return headers;
	}
}
