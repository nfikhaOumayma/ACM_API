package com.acm.api_nafath.serviceImpl;

import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.RSAPublicKeySpec;
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

import com.acm.api_nafath.service.NafathService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ElmResponseDTO;
import com.acm.utils.dtos.KeyDTO;
import com.acm.utils.dtos.KeysDTO;
import com.acm.utils.dtos.MfaRequestDTO;
import com.acm.utils.dtos.NafathCallbackPayloadDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.JWSVerifier;
import com.nimbusds.jose.crypto.RSASSAVerifier;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;

/**
 * The Class NafathServiceImpl is an implementation of the NafathService interface. It provides
 * methods for interacting with the Nafath API.
 */
@Service
public class NafathServiceImpl implements NafathService {

	/** The Constant logger for logging. */
	private static final Logger logger = LoggerFactory.getLogger(NafathServiceImpl.class);

	/** The base URL for the Nafath API. */
	private static String baseUrl = "https://nafath.api.elm.sa/stg/";

	/** The application ID for authentication. */
	private String appId = "xxx-xxx-xxx";

	/** The RestTemplate for making HTTP requests. */
	private final RestTemplate restTemplate = new RestTemplate();

	/** The CreditClient for recording API usage. */
	@Autowired
	private CreditClient creditClient;

	/** The ParametrageClient for retrieving configuration parameters. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The organization number for authentication. */
	private String org_number = "org_number";

	/** The platform key for authentication. */
	private String platform_key = "platform_key";

	/** The request number. */
	private String request_number = "request_number";

	/**
	 * Sends a request to the Nafath API.
	 *
	 * @param requestPayload the payload for the request
	 * @param local the local parameter
	 * @param requestId the request ID
	 * @return ResponseEntity containing the API response
	 */
	@Override
	public ResponseEntity<?> sendRequest(MfaRequestDTO requestPayload, String local,
			String requestId) {

		// Create a DTO for recording API usage
		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();

		// Retrieve configuration parameters from the ParametrageClient
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("NAFATH_API_REQUEST"));
		baseUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_BASE_URL", baseUrl);
		appId = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_APP_ID", appId);

		// Construct the URL for the API request
		String url = baseUrl + "api/v1/mfa/request" + "?local=" + local + "&requestId=" + requestId;

		// Create HTTP headers for the request
		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");

		HttpEntity<MfaRequestDTO> requestEntity = new HttpEntity<>(requestPayload, headers);

		try {
			// Log the initiation of the Nafath request
			logger.info(
					"INIT NAFATH Request For sendRequest API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.POST, requestEntity);

			// Send the HTTP request and get the response
			ResponseEntity<ElmResponseDTO> response = restTemplate.exchange(url, HttpMethod.POST,
					requestEntity, ElmResponseDTO.class);

			// Record the API usage in the CreditClient
			thirdPartyHistoDTO.setCategory("NAFATH/sendRequest");
			thirdPartyHistoDTO.setReportTag(response.getBody().getTransId());
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody().toString());
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			// Log the Nafath response
			logger.info("### INIT NAFATH Response For sendRequest API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.api_nafath.service.NafathAuthenticationService#retrieveJwk()
	 */
	@Override
	public ResponseEntity<KeysDTO> retrieveJwk() {

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("NAFATH_API_REQUEST"));

		baseUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_BASE_URL", baseUrl);

		String url = baseUrl + "api/v1/mfa/jwk";

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT NAFATH Request For retrieveJwk API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<KeysDTO> response =
					restTemplate.exchange(url, HttpMethod.GET, requestEntity, KeysDTO.class);

			thirdPartyHistoDTO.setCategory("NAFATH/retrieveJwk");
			thirdPartyHistoDTO.setResponseValue(response.getBody().toString());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT NAFATH Response For retrieveJwk API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}
	}

	/* (non-Javadoc)
	 * @see com.acm.api_nafath.service.NafathService#receiveCallback(com.acm.utils.dtos.NafathCallbackPayloadDTO)
	 */
	@Override
	public ResponseEntity<?> receiveCallback(NafathCallbackPayloadDTO callbackPayload) {

		String token = callbackPayload.getToken();
		ResponseEntity<?> x = this.verifyJwk(token);
		logger.info("xxxxxxxxxxxxx", HttpMethod.GET, x);

		ThirdPartyHistoriqueDTO thirdPartyHistoDTO = new ThirdPartyHistoriqueDTO();
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("NAFATH_API_REQUEST"));

		baseUrl = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_BASE_URL", baseUrl);
		org_number = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_ORG_NUMBER", org_number);
		platform_key = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_PLATFORM_KEY", platform_key);
		request_number = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"NAFATH_API_REQUEST_NUMBER", request_number);

		String url = baseUrl + "/jwt/valid";

		HttpHeaders headers = new HttpHeaders();
		headers.set("Content-Type", "application/json");
		headers.set("Accept", "application/json");
		headers.add("ORGANIZATION-NUMBER", org_number);
		headers.add("PLATFORM-KEY", platform_key);
		headers.add("REQUEST-NUMBER", request_number);

		HttpEntity<String> requestEntity = new HttpEntity<>(headers);

		try {
			logger.info(
					"INIT NAFATH Request For verifyJwk API = \"{}\", method = \"{}\", entity = \"{}\"",
					url, HttpMethod.GET, requestEntity);

			ResponseEntity<?> response =
					restTemplate.exchange(url, HttpMethod.GET, requestEntity, String.class);

			thirdPartyHistoDTO.setCategory("NAFATH/retrieveIamPageUrl");
			thirdPartyHistoDTO.setRequestValue(requestEntity.getBody());
			thirdPartyHistoDTO.setResponseValue((String) response.getBody());
			thirdPartyHistoDTO.setStatus(String.valueOf(response.getStatusCodeValue()));
			creditClient.create(thirdPartyHistoDTO);

			logger.info("### INIT NAFATH Response For verifyJwk API = [{}] ### ", response);

			return response;
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();

		}

	}

	/**
	 * This method verifies a JWT using a JSON Web Key (JWK).
	 *
	 * @param jwtToken The JWT token to be verified.
	 * @return ResponseEntity containing JWT claims if verification is successful, or an error
	 *         response.
	 */
	public ResponseEntity<?> verifyJwk(String jwtToken) {

		ResponseEntity<KeysDTO> jwkResponse = retrieveJwk();

		if (jwkResponse.getStatusCode() != HttpStatus.OK) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
					.body("Error retrieving JWK.");
		}

		try {
			KeyDTO firstKey = extractFirstKey(jwkResponse.getBody());

			if (firstKey == null) {
				return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
						.body("No keys found in the JWK response.");
			}

			RSAPublicKey publicKey = buildRSAPublicKey(firstKey);
			SignedJWT signedJWT = SignedJWT.parse(jwtToken);

			if (verifyJWTSignature(signedJWT, publicKey)) {
				JWTClaimsSet claimsSet = signedJWT.getJWTClaimsSet();
				return ResponseEntity.ok(claimsSet.getClaims());
			}
			else {
				return ResponseEntity.status(401).body("Invalid JWT signature.");
			}
		}
		catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.status(500).body("Internal Server Error");
		}
	}

	/**
	 * Extracts the first key from the KeysDTO.
	 *
	 * @param keysDTO The KeysDTO containing a list of keys.
	 * @return The first KeyDTO, or null if none are found.
	 */
	private KeyDTO extractFirstKey(KeysDTO keysDTO) {

		if (keysDTO != null && keysDTO.getKeys() != null && !keysDTO.getKeys().isEmpty()) {
			return keysDTO.getKeys().get(0);
		}
		return null;
	}

	/**
	 * Builds an RSAPublicKey from a KeyDTO.
	 *
	 * @param keyDTO The KeyDTO containing RSA key components.
	 * @return The RSAPublicKey.
	 * @throws NoSuchAlgorithmException If the RSA algorithm is not available.
	 * @throws InvalidKeySpecException If the key specification is invalid.
	 */
	private RSAPublicKey buildRSAPublicKey(KeyDTO keyDTO)
			throws NoSuchAlgorithmException, InvalidKeySpecException {

		String rsaPublicKeyModulus = keyDTO.getN();
		String rsaPublicKeyExponent = keyDTO.getE();

		byte[] modulusBytes = Base64.getUrlDecoder().decode(rsaPublicKeyModulus);
		byte[] exponentBytes = Base64.getUrlDecoder().decode(rsaPublicKeyExponent);

		BigInteger modulus = new BigInteger(1, modulusBytes);
		BigInteger exponent = new BigInteger(1, exponentBytes);

		RSAPublicKeySpec rsaPublicKeySpec = new RSAPublicKeySpec(modulus, exponent);
		KeyFactory keyFactory = KeyFactory.getInstance("RSA");
		return (RSAPublicKey) keyFactory.generatePublic(rsaPublicKeySpec);
	}

	/**
	 * Verifies the JWT signature using the provided RSAPublicKey.
	 *
	 * @param signedJWT The SignedJWT to be verified.
	 * @param publicKey The RSAPublicKey used for verification.
	 * @return True if the signature is valid, false otherwise.
	 * @throws JOSEException If an error occurs during JWT signature verification.
	 */
	private boolean verifyJWTSignature(SignedJWT signedJWT, RSAPublicKey publicKey)
			throws JOSEException {

		JWSVerifier verifier = new RSASSAVerifier(publicKey);
		return signedJWT.verify(verifier);
	}

}
