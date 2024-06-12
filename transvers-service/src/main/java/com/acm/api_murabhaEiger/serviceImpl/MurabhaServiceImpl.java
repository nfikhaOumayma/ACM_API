/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_murabhaEiger.serviceImpl;

import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.List;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.api_murabhaEiger.service.MurabhaService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AuthMurabhaApiRequestDTO;
import com.acm.utils.dtos.AuthParametersDTO;
import com.acm.utils.dtos.AuthResponseMurabhaApiDTO;
import com.acm.utils.dtos.ConfirmPurchaseOrSaleRequestApiDTO;
import com.acm.utils.dtos.ConfirmPurchaseResponseApiDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiRequestDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiResponseDTO;
import com.acm.utils.dtos.SaleMurabhaApiRequestDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The Class MurabhaServiceImpl.
 */
@Service
public class MurabhaServiceImpl implements MurabhaService {

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The rest template. */
	private final RestTemplate restTemplate = new RestTemplate();

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(MurabhaServiceImpl.class);

	/**
	 * Calculate secret hash.
	 *
	 * @param authParmDTO the auth parm DTO
	 * @param clientSecret the client secret
	 * @param clientId the client id
	 * @return the auth murabha api request DTO
	 * @throws InvalidKeyException the invalid key exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	public AuthMurabhaApiRequestDTO calculateSecretHash(AuthMurabhaApiRequestDTO authParmDTO,
			String clientSecret, String clientId)
			throws InvalidKeyException, NoSuchAlgorithmException {

		// Create a SecretKeySpec with the client_secret and SHA-256 digest
		SecretKeySpec keySpec =
				new SecretKeySpec(clientSecret.getBytes(StandardCharsets.UTF_8), "HmacSHA256");

		// Create an instance of the HMAC-SHA-256 algorithm
		Mac hmacSha256 = Mac.getInstance("HmacSHA256");
		hmacSha256.init(keySpec);

		// Compute the HMAC by updating with the message (username + client_id)
		byte[] digest =
				hmacSha256.doFinal((authParmDTO.getAuthParameters().getUSERNAME() + clientId)
						.getBytes(StandardCharsets.UTF_8));

		// Encode the digest as Base64 and print it
		String secretHash = Base64.getEncoder().encodeToString(digest);

		authParmDTO.getAuthParameters().setSECRET_HASH(secretHash);
		authParmDTO.setClientId(clientId);
		return authParmDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#getToken()
	 */
	@Override
	public ResponseEntity<String> getToken() throws InvalidKeyException, NoSuchAlgorithmException,
			IllegalArgumentException, IllegalAccessException {

		AuthMurabhaApiRequestDTO authParametersDTO = new AuthMurabhaApiRequestDTO();
		String apiUrl = "https://cognito-idp.us-east-2.amazonaws.com";
		String clientSecret = "1b95mro78dm51obc9d87ngd3o6llv8us6rrcn7klvpedi2adtl7t";
		String clientId = "4hedrost6hp3efau1kauljnpf4";
		String userName = "mkhemissi@talys.digital";
		String password = "TalysSanad@123456";
		String authFlow = "USER_PASSWORD_AUTH";

		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL_TOKEN", apiUrl);
		clientSecret = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_CLIENT_SECRET", clientSecret);
		clientId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_CLIENT_ID", clientId);
		userName = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_USERNAME", userName);
		password = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_PASSWORD", password);
		authFlow = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_AUTHFLOW", authFlow);

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();

		AuthParametersDTO authParmDTO = new AuthParametersDTO();
		authParmDTO.setUSERNAME(userName);
		authParmDTO.setPASSWORD(password);
		authParametersDTO.setAuthParameters(authParmDTO);
		authParametersDTO.setAuthFlow(authFlow);

		// calculate Secret Hash
		AuthMurabhaApiRequestDTO authParms =
				calculateSecretHash(authParametersDTO, clientSecret, clientId);
		// set the secret hash
		authParametersDTO.getAuthParameters()

				.setSECRET_HASH((authParms.getAuthParameters().getSECRET_HASH()));

		String requestBody = String.format(
				"{\n" + " \"AuthParameters\" : {\n" + " \"USERNAME\" : \"%s\",\n"
						+ " \"PASSWORD\" : \"%s\",\n" + " \"SECRET_HASH\": \"%s\"\n" + " },\n"
						+ " \"AuthFlow\" : \"USER_PASSWORD_AUTH\",\n" + " \"ClientId\" : \"%s\"\n"
						+ "}",
				userName, password, authParms.getAuthParameters().getSECRET_HASH(), clientId);

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.valueOf("application/x-amz-json-1.1"));

		headers.set("X-Amz-Target", "AWSCognitoIdentityProviderService.InitiateAuth");
		headers.set("Accept", "application/json");

		String finalUrl = apiUrl;

		// Create the HTTP entity with the request body and headers
		HttpEntity<String> request = new HttpEntity<>(requestBody, headers);

		try {
			logger.info(
					"INIT Murabha Request For GET_TOKEN API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP POST request
			ResponseEntity<String> response =
					restTemplate.exchange(finalUrl, HttpMethod.POST, request, String.class);

			thirdPartyHistoDTO.setCategory("MurabahaToken");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For GET_TOKEN API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/**
	 * Convert response entity to DTO.
	 *
	 * @param response the response
	 * @return the auth response murabha api DTO
	 */
	public AuthResponseMurabhaApiDTO convertResponseEntityToDTO(ResponseEntity<String> response) {

		// Create ObjectMapper
		ObjectMapper objectMapper = new ObjectMapper();
		AuthResponseMurabhaApiDTO authenticationResponseDTO = null;

		try {
			// Parse JSON to AuthenticationResponseDTO object
			authenticationResponseDTO =
					objectMapper.readValue(response.getBody(), AuthResponseMurabhaApiDTO.class);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return authenticationResponseDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#purchase(com.acm.utils.dtos.
	 * PurchaseMurabhaApiRequestDTO, java.lang.Long)
	 */
	@Override
	public ResponseEntity<PurchaseMurabhaApiResponseDTO> purchase(
			PurchaseMurabhaApiRequestDTO purchaseRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException {

		String apiUrl = "https://api.eigertest.com/beta-test";
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL", apiUrl);
		ResponseEntity<String> responseEntity = getToken();

		// convert response api get token to dto
		AuthResponseMurabhaApiDTO authenticationResponseDTO =
				convertResponseEntityToDTO(responseEntity);

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.set("Accept", "application/json");
		headers.set("Authorization",
				authenticationResponseDTO.getAuthenticationResult().getIdToken());
		headers.set("email", "sanad_api");

		String finalUrl = apiUrl + "/order/purchase";

		// Create the HTTP entity with the request body and headers
		HttpEntity<PurchaseMurabhaApiRequestDTO> request =
				new HttpEntity<>(purchaseRequestDTO, headers);
		try {
			logger.info(
					"INIT Murabha Request For PURCHASE API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP POST request
			ResponseEntity<PurchaseMurabhaApiResponseDTO> response = restTemplate.exchange(finalUrl,
					HttpMethod.POST, request, PurchaseMurabhaApiResponseDTO.class);

			thirdPartyHistoDTO.setCategory("MURABHA/PURCHASE");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For PURCHASE API = [{}] ### ", response);

			return ResponseEntity.ok(response.getBody());
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#confirmPurchase(com.acm.utils.dtos.
	 * ConfirmPurchaseOrSaleRequestApiDTO, java.lang.Long)
	 */
	@Override
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmPurchase(
			ConfirmPurchaseOrSaleRequestApiDTO confirmPurchaseRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException {

		String apiUrl = "https://api.eigertest.com/beta-test";
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL", apiUrl);
		ResponseEntity<String> responseEntity = getToken();

		// convert response api get token to dto
		AuthResponseMurabhaApiDTO authenticationResponseDTO =
				convertResponseEntityToDTO(responseEntity);

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Authorization",
				authenticationResponseDTO.getAuthenticationResult().getIdToken());
		headers.set("email", "sanad_api");

		String finalUrl = apiUrl + "/order/purchase";

		// Create the HTTP entity with the request body and headers
		HttpEntity<ConfirmPurchaseOrSaleRequestApiDTO> request =
				new HttpEntity<>(confirmPurchaseRequestDTO, headers);

		RestTemplate template = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
		ConfirmPurchaseResponseApiDTO response = null;
		try {
			logger.info(
					"INIT Murabha Request For CONFIRM_PURCHASE API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.PATCH, request);
			// Make the HTTP PATCH request
			response =
					template.patchForObject(finalUrl, request, ConfirmPurchaseResponseApiDTO.class);

			thirdPartyHistoDTO.setCategory("MURABHA/CONFIRM_PURCHASE");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getEigerResultCode()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For CONFIRM_PURCHASE API = [{}] ### ", response);

			return ResponseEntity.ok(response);
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#sale(com.acm.utils.dtos.
	 * SaleMurabhaApiRequestDTO, java.lang.Long)
	 */
	@Override
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> sale(
			SaleMurabhaApiRequestDTO salePurchaseRequestDTO, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException {

		String apiUrl = "https://api.eigertest.com/beta-test";
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL", apiUrl);
		ResponseEntity<String> responseEntity = getToken();

		// convert response api get token to dto
		AuthResponseMurabhaApiDTO authenticationResponseDTO =
				convertResponseEntityToDTO(responseEntity);

		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Authorization",
				authenticationResponseDTO.getAuthenticationResult().getIdToken());
		headers.set("email", "sanad_api");

		String finalUrl = apiUrl + "/order/sale";

		// Create the HTTP entity with the request body and headers
		HttpEntity<SaleMurabhaApiRequestDTO> request =
				new HttpEntity<>(salePurchaseRequestDTO, headers);
		try {
			logger.info(
					"INIT Murabha Request For SALE API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP POST request
			ResponseEntity<ConfirmPurchaseResponseApiDTO> response = restTemplate.exchange(finalUrl,
					HttpMethod.POST, request, ConfirmPurchaseResponseApiDTO.class);

			thirdPartyHistoDTO.setCategory("MURABHA/SALE");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For SALE API = [{}] ### ", response);

			return ResponseEntity.ok(response.getBody());
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#confirmSale(com.acm.utils.dtos.
	 * ConfirmPurchaseOrSaleRequestApiDTO, java.lang.Long)
	 */
	@Override
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> confirmSale(
			ConfirmPurchaseOrSaleRequestApiDTO confirmSaleRequest, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException {

		String apiUrl = "https://api.eigertest.com/beta-test";
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL", apiUrl);
		ResponseEntity<String> responseEntity = getToken();

		// convert response api get token to dto
		AuthResponseMurabhaApiDTO authenticationResponseDTO =
				convertResponseEntityToDTO(responseEntity);
		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Authorization",
				authenticationResponseDTO.getAuthenticationResult().getIdToken());
		headers.set("email", "sanad_api");

		String finalUrl = apiUrl + "/order/sale";

		// Create the HTTP entity with the request body and headers
		HttpEntity<ConfirmPurchaseOrSaleRequestApiDTO> request =
				new HttpEntity<>(confirmSaleRequest, headers);

		RestTemplate template = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
		ConfirmPurchaseResponseApiDTO response = null;
		try {
			logger.info(
					"INIT Murabha Request For CONFIRM_SALE API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.POST, request);
			// Make the HTTP PATCH request
			response =
					template.patchForObject(finalUrl, request, ConfirmPurchaseResponseApiDTO.class);

			thirdPartyHistoDTO.setCategory("MURABHA/CONFIRM_SALE");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getEigerResultCode()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For CONFIRM_SALE API = [{}] ### ", response);

			return ResponseEntity.ok(response);
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#cancelTransaction(com.acm.utils.dtos.
	 * ConfirmPurchaseOrSaleRequestApiDTO)
	 */
	@Override
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> cancelTransaction(
			ConfirmPurchaseOrSaleRequestApiDTO cancelTransaction) throws InvalidKeyException,
			NoSuchAlgorithmException, IllegalArgumentException, IllegalAccessException {

		String apiUrl = "https://api.eigertest.com/beta-test";
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL", apiUrl);
		ResponseEntity<String> responseEntity = getToken();

		// convert response api get token to dto
		AuthResponseMurabhaApiDTO authenticationResponseDTO =
				convertResponseEntityToDTO(responseEntity);
		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Authorization",
				authenticationResponseDTO.getAuthenticationResult().getIdToken());

		String finalUrl = apiUrl + "/order/canceltransaction/";

		// Create the HTTP entity with the request body and headers
		HttpEntity<ConfirmPurchaseOrSaleRequestApiDTO> request =
				new HttpEntity<>(cancelTransaction, headers);

		RestTemplate template = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
		ConfirmPurchaseResponseApiDTO response = null;
		try {
			logger.info(
					"INIT Murabha Request For CANCEL_TRANSACTION API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.PATCH, request);
			// Make the HTTP PATCH request
			response =
					template.patchForObject(finalUrl, request, ConfirmPurchaseResponseApiDTO.class);

			thirdPartyHistoDTO.setCategory("MurabahaCancelTransaction");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getEigerResultCode()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For CANCEL_TRANSACTION API = [{}] ### ",
					response);

			return ResponseEntity.ok(response);
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_murabhaEiger.service.MurabhaService#transferNotice(com.acm.utils.dtos.
	 * ConfirmPurchaseOrSaleRequestApiDTO, java.lang.Long)
	 */
	@Override
	public ResponseEntity<ConfirmPurchaseResponseApiDTO> transferNotice(
			ConfirmPurchaseOrSaleRequestApiDTO transferNotice, Long loanId)
			throws InvalidKeyException, NoSuchAlgorithmException, IllegalArgumentException,
			IllegalAccessException {

		String apiUrl = "https://api.eigertest.com/beta-test";
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("MURABHA_API_REQUEST"));

		apiUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"MURABHA_API_REQUEST_URL", apiUrl);
		ResponseEntity<String> responseEntity = getToken();

		// convert response api get token to dto
		AuthResponseMurabhaApiDTO authenticationResponseDTO =
				convertResponseEntityToDTO(responseEntity);
		// Set up the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.set("Accept", "application/json");
		headers.set("Authorization",
				authenticationResponseDTO.getAuthenticationResult().getIdToken());
		headers.set("email", "sanad_api");

		String finalUrl = apiUrl + "/order/transfernotification";

		// Create the HTTP entity with the request body and headers
		HttpEntity<ConfirmPurchaseOrSaleRequestApiDTO> request =
				new HttpEntity<>(transferNotice, headers);

		RestTemplate template = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
		ConfirmPurchaseResponseApiDTO response = null;
		try {
			logger.info(
					"INIT Murabha Request For TRANSFER_NOTICE API = \"{}\", method = \"{}\", entity = \"{}\"",
					finalUrl, HttpMethod.PATCH, request);
			// Make the HTTP PATCH request
			response =
					template.patchForObject(finalUrl, request, ConfirmPurchaseResponseApiDTO.class);

			thirdPartyHistoDTO.setCategory("MurabahaTransferNotice");
			thirdPartyHistoDTO.setRequestValue(request.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getEigerResultCode()));
			thirdPartyHistoDTO.setIdLoan(loanId);
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT Murabha Response For TRANSFER_NOTICE API = [{}] ### ", response);

			return ResponseEntity.ok(response);
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

}
