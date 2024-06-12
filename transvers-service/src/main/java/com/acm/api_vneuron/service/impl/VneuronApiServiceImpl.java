/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_vneuron.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.api_vneuron.service.VneuronApiService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.enums.ThirdPartyCategory;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.vneuron.utils.dtos.CustomerStatusResponse;
import com.vneuron.utils.dtos.CustomerVneuron;
import com.vneuron.utils.dtos.Items;
import com.vneuron.utils.dtos.OnboardCustomerDTO;
import com.vneuron.utils.dtos.OnboardCustomerResponse;
import com.vneuron.utils.dtos.RiskResponse;
import com.vneuron.utils.dtos.SearchDecisionsResponse;
import com.vneuron.utils.dtos.SearchPersonCustomerResponse;
import com.vneuron.utils.dtos.SearchWebhookResponse;
import com.vneuron.utils.dtos.UserAuthVneuron;
import com.vneuron.utils.dtos.UserVneuron;

/**
 * {@link VneuronApiServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class VneuronApiServiceImpl implements VneuronApiService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(VneuronApiServiceImpl.class);

	/** The uri post journal page. */
	private String uriUserToken;

	/** The uri search person customer. */
	private String uriSearchPersonCustomer;

	/** The uri search decision. */
	private String uriSearchDecision;

	/** The uri onboard customer. */
	private String uriOnboardCustomer;

	/** The user name. */
	private String userName;

	/** The password. */
	private String password;

	/** The uri search risk. */
	private String uriSearchRisk;

	private String uriCustomerStatus;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The system name. */
	private String systemName;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/**
	 * Post AML.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @return the search person customer response
	 * @throws JsonProcessingException the json processing exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_vneuron.service.VneuronApiService#postAML(com.vneuron.utils.dtos.CustomerVneuron)
	 */
	@Override
	public SearchPersonCustomerResponse postAML(CustomerVneuron customerVneuron, Long loanId,
			Long customerId) throws JsonProcessingException {

		getPramConnectionAndUriReis();
		UserVneuron userVneuron = getUserToken();

		return searchPersonCustomer(customerVneuron, userVneuron, loanId, customerId);

	}

	/**
	 * Gets the score.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @return the score
	 * @throws JsonProcessingException the json processing exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_vneuron.service.VneuronApiService#getScore(com.vneuron.utils.dtos.
	 * CustomerVneuron)
	 */
	@Override
	public RiskResponse getScore(CustomerVneuron customerVneuron, Long idLoan, Long customerId)
			throws JsonProcessingException {

		getPramConnectionAndUriReis();
		UserVneuron userVneuron = getUserToken();
		return calculRisk(onboardCustomer(customerVneuron, userVneuron, idLoan, customerId),
				userVneuron, idLoan, customerId);

	}

	/**
	 * Gets the pram connection and uri reis.
	 */
	public void getPramConnectionAndUriReis() {

		logger.info("begin get param reis from ACM_ENVIRONNEMENT");
		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		// loading list of customers from ABACUS DB
		List<AcmEnvironnementDTO> environnementDTOs = new ArrayList<AcmEnvironnementDTO>();
		environnementDTOs = parametrageClient
				.findLikeKeyWithToken(new AcmEnvironnementDTO("VNEURON_API"), token);

		uriUserToken = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_REQUEST_AUTH_URL", uriUserToken);

		uriSearchPersonCustomer = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_REQUEST_SEARCH_CUSTOMER_URL", uriSearchPersonCustomer);
		uriSearchDecision = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_REQUEST_DECISION_URL", uriSearchDecision);
		uriOnboardCustomer = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_REQUEST_ONBOARD_CUSTOMER_URL", uriOnboardCustomer);
		uriSearchRisk = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_REQUEST_RISK_CALCULATION_URL", uriSearchRisk);

		uriCustomerStatus = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_REQUEST_CUSTOMER_STATUS_URL", uriCustomerStatus);
		userName = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_AUTH_USERNAME", userName);
		password = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_AUTH_PASSWORD", password);
		systemName = CommonFunctions.findConfigurationFromList(environnementDTOs,
				"VNEURON_API_SYSTEM_NAME", systemName);
		logger.info("end get param reis from ACM_ENVIRONNEMENT");

	}

	/**
	 * Gets the user token.
	 *
	 * @return the user token
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_vneuron.service.VneuronApiService#getUserToken()
	 */
	@Override
	public UserVneuron getUserToken() {

		logger.info("begin get token REIS");
		UserAuthVneuron userAuthVneuron = new UserAuthVneuron();
		// Set the request headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.add("x-auth-tenant", "public");
		userAuthVneuron.setUser_name(userName);
		userAuthVneuron.setPassword(password);
		// Create a RestTemplate instance
		HttpEntity<UserAuthVneuron> requestEntity = new HttpEntity<>(userAuthVneuron, headers);
		// Create a RestTemplate instance
		RestTemplate restTemplate = new RestTemplate();
		// Make the POST request
		logger.info("begin POST  get Token from REIS");
		ResponseEntity<UserVneuron> response = restTemplate.exchange(uriUserToken, HttpMethod.POST,
				requestEntity, UserVneuron.class);
		logger.info("get Token with sucess REIS");

		return response.getBody();
	}

	/**
	 * Search person customer.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param userVneuron the user vneuron
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @return the search person customer response
	 */
	public SearchPersonCustomerResponse searchPersonCustomer(CustomerVneuron customerVneuron,
			UserVneuron userVneuron, Long loanId, Long customerId) {

		// Set the request URL
		try {
			logger.info("begin searchPersonCustomer REIS");
			customerVneuron.setSystemId(customerId.toString());
			customerVneuron.setSystemName(systemName);
			customerVneuron.setSearchQuerySource("KYC");
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.add("x-auth-tenant", "public");
			headers.add("x-auth-token", userVneuron.getToken());
			// Create the request entity with the object as the body
			HttpEntity<CustomerVneuron> requestEntity = new HttpEntity<>(customerVneuron, headers);

			// Create a RestTemplate instance
			RestTemplate restTemplate = new RestTemplate();
			// Make the POST request
			logger.info("request searchPersonCustomer REIS {}", customerVneuron);
			ResponseEntity<SearchPersonCustomerResponse> response =
					restTemplate.exchange(uriSearchPersonCustomer, HttpMethod.POST, requestEntity,
							SearchPersonCustomerResponse.class);
			logger.info("response searchPersonCustomer REIS {}", response.getBody());

			logger.info("end AML REIS");
			ObjectMapper objectMapper = new ObjectMapper();
			saveThirdParty(objectMapper.writeValueAsString(response.getBody()),
					objectMapper.writeValueAsString(customerVneuron),
					ThirdPartyCategory.SearchPersonCustomer.name(), loanId,
					response.getBody().getSearch_query_id(), response.getBody().getCustomer_id(),
					customerId);
			// Get the response body
			return response.getBody();
			// Print the response body
		}
		catch (Exception e) {
			logger.info("check postAML {}", e.getMessage());
		}
		return null;
	}

	/**
	 * Onboard customer.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param userVneuron the user vneuron
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @return the onboard customer response
	 * @throws JsonProcessingException the json processing exception
	 */
	public OnboardCustomerResponse onboardCustomer(CustomerVneuron customerVneuron,
			UserVneuron userVneuron, Long idLoan, Long customerId) throws JsonProcessingException {

		// Set the request URL
		logger.info("send api onboardCustomer");
		OnboardCustomerDTO onboardCustomerDTO = new OnboardCustomerDTO();
		// set DTO OnboardCustomerDTO
		onboardCustomerDTO.setSystemId(customerId.toString());
		onboardCustomerDTO.setSystemName(systemName);
		customerVneuron.setSearchQuerySource("KYC");
		onboardCustomerDTO.setFormId(12);
		// set items
		Items items = new Items();

		items.setFirst_name(customerVneuron.getFirstName());
		items.setLast_name(customerVneuron.getLastName());
		items.setCus_birth_date(customerVneuron.getBirthDate());
		items.setNationality(customerVneuron.getNationality());
		items.setCitizenship(customerVneuron.getNationality());

		// creditClient.getThirdPartyMapping(customerVneuron.getNationality(), "NATIONALITY")
		// .getMappedData());
		items.setTin("CIN");
		items.setNid(customerVneuron.getNid());
		items.setEntity_type("PP");
		items.setIs_pep(false);
		items.setDelivery_channel("1");
		// stand by Khaled
		items.setProduct("1");
		items.setCustomer_type("");
		onboardCustomerDTO.setItems(items);

		// create headers
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.add("x-auth-tenant", "public");
		headers.add("x-auth-token", userVneuron.getToken());
		// Create the request entity with the object as the body
		HttpEntity<OnboardCustomerDTO> requestEntity =
				new HttpEntity<>(onboardCustomerDTO, headers);

		ObjectMapper objectMapper = new ObjectMapper();

		logger.info("onboardCustomer json request {}",
				objectMapper.writeValueAsString(onboardCustomerDTO));
		// Create a RestTemplate instance
		RestTemplate restTemplate = new RestTemplate();

		// Make the POST request
		ResponseEntity<OnboardCustomerResponse> response = restTemplate.exchange(uriOnboardCustomer,
				HttpMethod.POST, requestEntity, OnboardCustomerResponse.class);
		logger.info("onboardCustomer json response {}", response.getBody());
		// Get the response body

		saveThirdParty(objectMapper.writeValueAsString(response.getBody()),
				CommonFunctions.convertObjectToJSONString(onboardCustomerDTO),
				ThirdPartyCategory.OnboardCustomerReis.name(), idLoan, null, null, customerId);
		return response.getBody();
	}

	/**
	 * Calcul risk.
	 *
	 * @param onboardCustomerResponse the onboard customer response
	 * @param userVneuron the user vneuron
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @return the onboard customer response
	 * @throws JsonProcessingException the json processing exception
	 */
	public RiskResponse calculRisk(OnboardCustomerResponse onboardCustomerResponse,
			UserVneuron userVneuron, Long idLoan, Long customerId) throws JsonProcessingException {

		// Set the request URL
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.add("x-auth-tenant", "public");
		headers.add("x-auth-token", userVneuron.getToken());
		// Create the request entity with the object as the body
		// Create a RestTemplate instance
		RestTemplate restTemplate = new RestTemplate();
		// Make the GET request
		String url = uriSearchRisk + onboardCustomerResponse.getRiskCalculationId();
		HttpEntity<String> request = new HttpEntity<>("", headers);
		ResponseEntity<RiskResponse> response =
				restTemplate.exchange(url, HttpMethod.GET, request, RiskResponse.class);
		// Get the response body
		saveThirdPartyWithLoan(response.getBody(), url, ThirdPartyCategory.KYCRies.name(), idLoan,
				customerId, response.getBody().getCustomer().getId());

		return response.getBody();

	}

	/**
	 * Search web hook.
	 *
	 * @param searchWebhookResponse the search webhook response
	 * @return the loan DTO
	 * @throws JsonProcessingException the json processing exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_vneuron.service.VneuronApiService#searchWebHook(com.acm.api_vneuron.model.
	 * SearchWebhookResponse)
	 */
	@Override
	public LoanDTO searchWebHook(SearchWebhookResponse searchWebhookResponse)
			throws JsonProcessingException {

		logger.info("begin searchWebhookResponse");

		getPramConnectionAndUriReis();
		UserVneuron userVneuron = getUserToken();

		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);

		// get loan by Search_query_id
		LoanDTO loanDTO = creditClient.findLoanThirdPartyHistoriqueWithToken(
				searchWebhookResponse.getSearchQueryId(),
				ThirdPartyCategory.SearchPersonCustomer.name(), token);

		// launch api get decision for getting the matching customer
		// get customer with search queryId : searchQueryId = searchWebhookResponse.searchQueryId
		// api get decision vneuron
		List<SearchDecisionsResponse> searchDecisionsResponses =
				correspondenceProcessingWithQueryId(searchWebhookResponse.getSearchQueryId(),
						userVneuron, loanDTO.getLoanId(), loanDTO.getCustomerId());
		// filter searchDecisionsResponses with criteria action = 1
		List<SearchDecisionsResponse> searchDecisionsResponsesFlt = searchDecisionsResponses
				.stream().filter(item -> item.action == 1).collect(Collectors.toList());

		ObjectMapper objectMapper = new ObjectMapper();
		saveThirdParty("", objectMapper.writeValueAsString(searchWebhookResponse),
				ThirdPartyCategory.SearchWebhookReis.name(), loanDTO.getLoanId(),
				searchWebhookResponse.getSearchQueryId(), searchWebhookResponse.getCustomerId(),
				loanDTO.getCustomerId());

		if (ACMValidationUtils.isNullOrEmpty(searchDecisionsResponsesFlt)
				|| ACMValidationUtils.isNullOrEmpty(searchDecisionsResponses)) {
			// launch workFlow Loan
			logger.info("end  searchWebhookResponse with result");
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
			return creditClient.validate(loanDTO, token);
		}
		else {
			loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
			loanDTO.setParentId(0L);
			loanDTO.setCodeExternMotifRejet(1);
			loanDTO.setNote("Automatically Rejected Due To Unauthorized Customer");
			creditClient.rejectLoan(loanDTO, token);
			logger.info("end  searchWebhookResponse without result");

		}

		return null;

	}

	/**
	 * Correspondence processing with query id.
	 *
	 * @param queryId the query id
	 * @param userVneuron the user vneuron
	 * @param Idloan the idloan
	 * @param customerId the customer id
	 * @return the risk response
	 * @throws JsonProcessingException the json processing exception
	 */
	public List<SearchDecisionsResponse> correspondenceProcessingWithQueryId(Long queryId,
			UserVneuron userVneuron, Long Idloan, Long customerId) throws JsonProcessingException {

		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.add("x-auth-tenant", "public");
		headers.add("x-auth-token", userVneuron.getToken());
		// Create the request entity with the object as the body
		HttpEntity<String> requestEntity = new HttpEntity<>("", headers);

		// Create a RestTemplate instance
		RestTemplate restTemplate = new RestTemplate();
		ParameterizedTypeReference<List<SearchDecisionsResponse>> responseType =
				new ParameterizedTypeReference<List<SearchDecisionsResponse>>() {

				};
		// Make the GET request
		ResponseEntity<List<SearchDecisionsResponse>> response = restTemplate
				.exchange(uriSearchDecision + queryId, HttpMethod.GET, requestEntity, responseType);

		// Get the response body
		List<SearchDecisionsResponse> searchDecisionsResponse = response.getBody();

		ObjectMapper objectMapper = new ObjectMapper();

		saveThirdParty(objectMapper.writeValueAsString(searchDecisionsResponse),
				uriSearchDecision + queryId, ThirdPartyCategory.AMLReis.name(), Idloan, null, null,
				customerId);

		return searchDecisionsResponse;

	}

	/**
	 * Save third party.
	 *
	 * @param response the response
	 * @param request the request
	 * @param category the category
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @param customerIdRais the customer id rais
	 */

	void saveThirdPartyWithLoan(RiskResponse response, String request, String category, Long idLoan,
			Long customerId, Long customerIdRais) {

		ObjectMapper objectMapper = new ObjectMapper();
		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		thirdPartyHistoriqueDTO.setCategory(category);
		thirdPartyHistoriqueDTO.setRequestValue(request);
		thirdPartyHistoriqueDTO.setIdCustomer(customerId);
		thirdPartyHistoriqueDTO.setCustomerReisId(customerIdRais);

		try {
			thirdPartyHistoriqueDTO.setResponseValue(objectMapper.writeValueAsString(response));
		}
		catch (JsonProcessingException e) {
			logger.info("check saveThirdPartyWithLoan {} ", e.getMessage());
		}
		thirdPartyHistoriqueDTO.setRiskLevel(response.getRiskLevel().getName());

		thirdPartyHistoriqueDTO.setIdLoan(idLoan);

		creditClient.saveThirdPartyHistoriqueWithToken(thirdPartyHistoriqueDTO, token);
	}

	/**
	 * Save third party.
	 *
	 * @param response the response
	 * @param request the request
	 * @param category the category
	 * @param Idloan the idloan
	 * @param searchQueryId the search query id
	 * @param customerIdReis the customer id reis
	 * @param customerId the customer id
	 */
	void saveThirdParty(String response, String request, String category, Long Idloan,
			Long searchQueryId, Long customerIdReis, Long customerId) {

		logger.info("begin saveThirdParty");

		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		thirdPartyHistoriqueDTO.setCategory(category);
		thirdPartyHistoriqueDTO.setRequestValue(request);
		thirdPartyHistoriqueDTO.setResponseValue(request);
		thirdPartyHistoriqueDTO.setIdLoan(Idloan);
		thirdPartyHistoriqueDTO.setSearchQueryId(searchQueryId);
		thirdPartyHistoriqueDTO.setCustomerReisId(customerIdReis);
		thirdPartyHistoriqueDTO.setIdCustomer(customerId);

		creditClient.saveThirdPartyHistoriqueWithToken(thirdPartyHistoriqueDTO, token);
		logger.info("end saveThirdParty");

	}

	/**
	 * Customer status.
	 *
	 * @param customerStatusResponse the customer status response
	 * @throws JsonProcessingException the json processing exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_vneuron.service.VneuronApiService#clientStaus(com.acm.api_vneuron.model.
	 * CustomerStatusResponse)
	 */
	@Override
	public void customerStatus(CustomerStatusResponse customerStatusResponse)
			throws JsonProcessingException {

		logger.info("begin  customerStatus with Reis Response : {}", customerStatusResponse);

		// get token
		String token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		// get status client from REIS

		// get loan with customer id Reis
		LoanDTO loanDTO = creditClient.findLoanThirdPartyHistoriqueByCustomerReisWithToken(
				customerStatusResponse.getCustomerId(), ThirdPartyCategory.KYCRies.name(), token);
		// a modifier from acm third party Historique

		ObjectMapper objectMapper = new ObjectMapper();
		saveThirdParty("", objectMapper.writeValueAsString(customerStatusResponse),
				ThirdPartyCategory.WebhookCustumerStatusReis.name(), loanDTO.getLoanId(), null,
				null, loanDTO.getCustomerId());

		if (customerStatusResponse.getStatus().equals("BLOCKED")) {
			// reject loan
			loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
			loanDTO.setParentId(0L);
			loanDTO.setCodeExternMotifRejet(1);
			loanDTO.setNote("Automatically Rejected Due To Unauthorized Customer (REIS)");
			creditClient.rejectLoan(loanDTO, token);
			logger.info("end  webhook customerStatus with reject loan");

		}
		if (customerStatusResponse.getStatus().equals("CUSTOMER")) {
			// validate loan
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
			creditClient.validate(loanDTO, token);

			logger.info("end  webhook customerStatus with validate loan");

		}

	}

	@Override
	public String getStatus(Long customerId, Long idLoan) {

		getPramConnectionAndUriReis();
		UserVneuron userVneuron = getUserToken();
		logger.info("begin customer status input customerId : {} , idLoan  {}", customerId, idLoan);

		// Set the request URL
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.add("x-auth-tenant", "public");
		headers.add("x-auth-token", userVneuron.getToken());
		// Create the request entity with the object as the body
		// Create a RestTemplate instance
		RestTemplate restTemplate = new RestTemplate();
		// Make the GET request
		String url = uriCustomerStatus + customerId + "/" + systemName;
		HttpEntity<String> request = new HttpEntity<>("", headers);
		logger.info("url  customer status for call reis   : {} ", url);
		ResponseEntity<String> response =
				restTemplate.exchange(url, HttpMethod.GET, request, String.class);
		logger.info("end call customer staus", url);

		// Get the response body
		saveThirdParty(response.getBody(), url, ThirdPartyCategory.CustomerStatus.name(), idLoan,
				null, null, customerId);

		return response.getBody();
	}

}
