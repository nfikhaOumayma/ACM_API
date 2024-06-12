package com.acm.api_abacus.service.impl;

import java.io.IOException;
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

import com.acm.api_abacus.service.ChargeFeeAPIService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.ResponseChargeFeeDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The Class ChargeFeeAPIServiceImpl.
 */
@Service
public class ChargeFeeAPIServiceImpl implements ChargeFeeAPIService {
	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoanCreateUpdateApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The url post charge fee. */
	@Value("${rest.api.abacus.charge.fee.post.uri}")
	private String urlPostChargeFee;

	/** The url get charge fee. */
	@Value("${rest.api.abacus.charge.fee.get.uri}")
	private String urlGetChargeFee;

	/** The url recipt type. */
	@Value("${rest.api.abacus.charge.fee.recipt.type}")
	private String urlReciptType;

	/**
	 * Initialize charge fee.
	 *
	 * @param accountId the account id
	 * @return the response charge fee DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanCreateUpdateApiService#getLoanInfo(java.lang.Long)
	 */
	@Override
	public ResponseChargeFeeDTO initializeChargeFee(Long accountId)
			throws IOException, ApiAbacusException, URISyntaxException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			logger.info("************ REQUEST : GET DATA API ************");
			logger.info("ACCOUNTID = {}", accountId);
			logger.info("*************** GET DATA API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			logger.info("GET Request findInformationsChargeFee = {}", request);
			String accessUrl = urlServeurApi + urlGetChargeFee + accountId + urlReciptType;
			// sending request to server using GET method
			URI uri = new URI(accessUrl);
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<ResponseChargeFeeDTO> responseAPI =
					restTemplate.exchange(uri, HttpMethod.GET, request, ResponseChargeFeeDTO.class);

			if (responseAPI.getBody() != null) {
				TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
						"findInformationsChargeFee", RequestMethode.GET.name(), uri.toString(),
						responseAPI.getStatusCode().toString(), request.toString(),
						responseAPI.getBody().toString());

				logger.info("responseAPI of findInformations to ChargeFee = {}",
						responseAPI.getBody());
				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}

				return responseAPI.getBody();
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" get Loan RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" get Loan ResponseBodyAsString = {}", e.getResponseBodyAsString());
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
			logger.error("Error has been occured while consuming get LOAN API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return null;

	}

	/**
	 * Post charge fees.
	 *
	 * @param chargeFeeDTO the charge fee DTO
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanCreateUpdateApiService#chargeFee(com.acm.utils.dtos.
	 * ResponseChargeFeeDTO)
	 */
	@Override
	public ResponseEntity<String> postChargeFees(ResponseChargeFeeDTO chargeFeeDTO)
			throws IOException, ApiAbacusException, URISyntaxException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException {

		try {
			// init resttemplate
			RestTemplate restTemplate = RestTemplateConfig.initRestTemplate();
			// setting token
			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			logger.info("************ REQUEST : POST DATA API ************");
			logger.info("*************** POST DATA API ******************");

			// create request body
			HttpEntity<ResponseChargeFeeDTO> request = new HttpEntity<>(chargeFeeDTO, headers);

			String accessUrl = urlServeurApi + urlPostChargeFee;

			// sending request to server using POST method
			URI uri = new URI(accessUrl);
			logger.info("uri POST DATA  = {}", uri);
			ResponseChargeFeeDTO requestChargeFee = request.getBody();
			ObjectMapper objectMapper = new ObjectMapper();

			logger.info("BODY chargeFeeAbacus  = {}",
					objectMapper.writeValueAsString(requestChargeFee));

			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);

			// check if response is not NULL
			if (responseAPI.getBody() != null) {
				logger.info("responseAPI of ChargeFee = {}", responseAPI.getBody());
				TransversHistoriqueDTO transversHistoriqueDTO =
						new TransversHistoriqueDTO("ChargeFee", RequestMethode.POST.name(),
								uri.toString(), responseAPI.getStatusCode().toString(),
								objectMapper.writeValueAsString(requestChargeFee),
								responseAPI.getBody().toString());

				if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
					creditClient.create(transversHistoriqueDTO);
				}

				return ResponseEntity.status(responseAPI.getStatusCode())
						.contentType(MediaType.APPLICATION_JSON).body(responseAPI.getBody());
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" post CHARGE FEE RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" post CHARGE FEE ResponseBodyAsString = {}", e.getResponseBodyAsString());
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
