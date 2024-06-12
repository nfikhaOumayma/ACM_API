/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.api_ib.impl;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.stream.Collectors;

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
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmDocumentsService;
import com.acm.service.api_ib.LoadDataIBService;
import com.acm.utils.dtos.AcmClaimsDTO;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.CustomMultipartFile;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.IBLoanDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.ClaimsPaginationDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;

/**
 * The Class LoadDataIBServiceImpl.
 */
@Service
public class LoadDataIBServiceImpl implements LoadDataIBService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoadDataIBServiceImpl.class);

	/** The rest template. */

	private RestTemplate restTemplate;

	/** The ib oauth url. */
	@Value("${ib.environment.oauthUrl}")
	private String ibOauthUrl;

	/** The ib api url. */
	@Value("${ib.environment.apiUrl}")
	private String ibApiUrl;

	/** The object mapper. */
	private ObjectMapper objectMapper;

	/** The documents loan service. */
	@Autowired
	private AcmDocumentsService documentsLoanService;

	/**
	 * Instantiates a new load data IB service impl.
	 */
	public LoadDataIBServiceImpl() {

		restTemplate = new RestTemplate();
		objectMapper = new ObjectMapper();
		objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

	}

	/**
	 * Login api ib.
	 *
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */

	public String loginApiIb() throws IOException, URISyntaxException {

		logger.info("BEGIN : login to IB ");
		String apiUrl = ibOauthUrl + "oauth/token?grant_type=client_credentials";
		URI uri = new URI(apiUrl);
		String authAuthorization = "ibclient:appclient@123";
		byte[] originalBytes = authAuthorization.getBytes();
		String authAuthorizationBase64 = Base64.getEncoder().encodeToString(originalBytes);

		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		headers.setAccept(
				Arrays.asList(MediaType.APPLICATION_JSON, MediaType.TEXT_PLAIN, MediaType.ALL));
		headers.set("Authorization", "Basic " + authAuthorizationBase64);
		String data = "grant_type=client_credentials";
		byte[] postData = data.getBytes(StandardCharsets.UTF_8);
		int postDataLength = postData.length;
		headers.set("Content-Length", Integer.toString(postDataLength));

		HttpEntity<String> request = new HttpEntity<>("", headers);
		ResponseEntity<String> response =
				restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
		logger.info("END : login to IB ");
		JsonNode jsonNode = objectMapper.readTree(response.getBody());
		return jsonNode.get("access_token").asText();

	}

	/**
	 * Find.
	 *
	 * @param ibLoanPaginationDTO the ib loan pagination DTO
	 * @return the IB loan pagination DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#find(com.acm.utils.dtos.pagination.
	 * IBLoanPaginationDTO)
	 */
	@Override
	public IBLoanPaginationDTO find(IBLoanPaginationDTO ibLoanPaginationDTO) {

		try {
			logger.info("BEGIN : find pagination of IBLoan from IB");
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			String apiUrl =
					ibApiUrl + "credit-service/loans-ib/find-pagination/" + userDTO.getLogin();
			URI uri = new URI(apiUrl);
			// convert loanIbPaginationDTO to json
			String body = objectMapper.writeValueAsString(ibLoanPaginationDTO);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : find pagination of IBLoan from IB");
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				JsonNode jsonNode = objectMapper.readTree(responseAPI.getBody());
				ibLoanPaginationDTO.setResultsIbLoans(
						objectMapper.readValue(jsonNode.get("resultsIBLoans").toString(),
								new TypeReference<List<IBLoanDTO>>() {

								}));
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return ibLoanPaginationDTO;
	}

	/**
	 * Find.
	 *
	 * @param id the id
	 * @return the IB loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#find(java.lang.Long)
	 */
	@Override
	public IBLoanDTO find(Long id) {

		try {
			logger.info("BEGIN : find IBLoanById  from IB with id = {}", id);
			String apiUrl = ibApiUrl + "credit-service/loans-ib/" + id;
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>("", headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.GET, request, String.class);
			logger.info("END : find IBLoanById  from IB with id = {}", id);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<IBLoanDTO>() {

						});
			}

		}
		catch (Exception e) {
			// to do throw custom exception in each api call
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Find customer by id.
	 *
	 * @param customerId the customer id
	 * @return the customer DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#findCustomerById(java.lang.Long)
	 */
	@Override
	public CustomerDTO findCustomerById(Long customerId) {

		try {
			logger.info("BEGIN : find CustomerDTO  from IB with id = {}", customerId);
			String apiUrl = ibApiUrl + "credit-service/customers/" + customerId;
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>("", headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.GET, request, String.class);
			logger.info("END : find CustomerDTO  from IB with id = {}", customerId);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<CustomerDTO>() {

						});
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Update.
	 *
	 * @param ibLoanDTO the ib loan DTO
	 * @return the IB loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#save(java.lang.Long,
	 * com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public IBLoanDTO update(IBLoanDTO ibLoanDTO) {

		try {
			logger.info("BEGIN : update IBLoanDTO in IB with id = {}", ibLoanDTO.getId());
			String apiUrl = ibApiUrl + "credit-service/loans-ib/update";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(ibLoanDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);
			logger.info("END : update IBLoanDTO in IB with id = {}", ibLoanDTO.getId());
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<IBLoanDTO>() {

						});
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Save acm loan in IB.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#saveAcmLoanInIB(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO saveAcmLoanInIB(LoanDTO loanDTO) {

		try {
			logger.info("BEGIN : save LoanDTO in IB with acmLoanId = {}", loanDTO.getLoanId());
			String apiUrl = ibApiUrl + "credit-service/loans-ib/create-acm-loan";
			URI uri = new URI(apiUrl);
			String body = objectMapper.writeValueAsString(loanDTO);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : save LoanDTO in IB with acmLoanId = {}", loanDTO.getLoanId());
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(), new TypeReference<LoanDTO>() {

				});
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Update acm loan and customer in IB.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#updateAcmLoanInIB(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO updateAcmLoanAndCustomerInIB(LoanDTO loanDTO) {

		try {
			logger.info("BEGIN : update LoanDTO in IB with acmLoanId = {}", loanDTO.getLoanId());
			String apiUrl = ibApiUrl + "credit-service/loans/update-loan-and-customer/ACM";
			URI uri = new URI(apiUrl);
			String body = objectMapper.writeValueAsString(loanDTO);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);
			logger.info("END : update LoanDTO in IB with acmLoanId = {}", loanDTO.getLoanId());

			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(), new TypeReference<LoanDTO>() {

				});
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Update acm loan in IB.
	 *
	 * @param loanDTO the loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#updateAcmLoanInIB(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void updateAcmLoanInIB(LoanDTO loanDTO) {

		try {

			logger.info("BEGIN : update LoanDTO in IB with acmLoanId = {}", loanDTO.getLoanId());
			String apiUrl = ibApiUrl + "credit-service/loans/update";
			URI uri = new URI(apiUrl);
			String body = objectMapper.writeValueAsString(loanDTO);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : update LoanDTO in IB with acmLoanId = {}", loanDTO.getLoanId());

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Creates the user.
	 *
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#createUser(com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public UserDTO createUser(UserDTO userDTO) {

		try {
			logger.info("BEGIN : create UserDTO in IB for the customer with login = {}",
					userDTO.getLogin());

			String apiUrl = ibApiUrl + "authentication-service/users/create-user-ib";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(userDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : create UserDTO in IB for the customer with login = {}",
					userDTO.getLogin());
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(), new TypeReference<UserDTO>() {

				});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Update.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#update(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public CustomerDTO update(CustomerDTO customerDTO) {

		try {
			logger.info("BEGIN : update customerDTO in IB with acmCustomerId =  {}",
					customerDTO.getId());
			String apiUrl = ibApiUrl + "credit-service/customers/update-for-application";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));

			// empty the list values of the udf fields of the udf links that will be sent to IB
			customerDTO.getUserDefinedFieldsLinksDTOs().forEach(udfLink -> {
				udfLink.getUserDefinedFieldsDTO().setFieldListValuesDTOs(null);
			});
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(customerDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);
			logger.info("END : update customerDTO in IB with acmCustomerId =  {}",
					customerDTO.getId());
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<CustomerDTO>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Find udf link.
	 *
	 * @param userDefinedFieldsLinksDTO the user defined fields links DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#findUdfLink(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public List<UDFLinksGroupeFieldsDTO> findUdfLink(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		try {
			logger.info("BEGIN : find UDFLinksGroupeFieldsDTO from IB by given parameters ");
			String apiUrl = ibApiUrl + "credit-service/udf-links/find-udf-groupby";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(userDefinedFieldsLinksDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : find UDFLinksGroupeFieldsDTO from IB by given parameters");
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<UDFLinksGroupeFieldsDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Update gurantors.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#updateGurantors(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO updateGurantors(LoanDTO loanDTO) {

		try {
			logger.info("BEGIN : update guarantor in IB for loan with acmLoanId = {}",
					loanDTO.getLoanId());
			String apiUrl = ibApiUrl + "credit-service/customers/update-guarantors";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(loanDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);

			restTemplate.exchange(uri, HttpMethod.POST, request, Void.class);
			logger.info("END : update guarantor in IB for loan with acmLoanId = {}",
					loanDTO.getLoanId());
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Find.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#find(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public List<CustomerDTO> find(CustomerDTO customerDTO) {

		try {
			String apiUrl = ibApiUrl + "credit-service/customers/";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(customerDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<CustomerDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Find.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#find(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> find(LoanDTO loanDTO) {

		try {
			String apiUrl = ibApiUrl + "credit-service/loans/";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(loanDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<LoanDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Find address.
	 *
	 * @param addressDTO the address DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#findAddress(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public List<AddressDTO> findAddress(AddressDTO addressDTO) {

		try {
			String apiUrl = ibApiUrl + "credit-service/address";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert to json
			String body = objectMapper.writeValueAsString(addressDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<AddressDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Find all customer information.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#findAllCustomerInformation(com.acm.utils.dtos.
	 * CustomerDTO)
	 */
	@Override
	public List<CustomerDTO> findAllCustomerInformation(CustomerDTO customerDTO) {

		try {
			String apiUrl = ibApiUrl + "credit-service/customers/find-all-customer-information";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(customerDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<CustomerDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Save loan schedule in ib.
	 *
	 * @param loanSchedule the loan schedule
	 * @return the loan schedule DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#saveLoanScheduleInIb(com.acm.utils.dtos.
	 * LoanScheduleDTO)
	 */
	@Override
	public LoanScheduleDTO saveLoanScheduleInIb(LoanScheduleDTO loanSchedule) {

		try {
			logger.info("BEGIN : save LoanSchedule in IB with idLoanIb = {}",
					loanSchedule.getLoanDTO().getIdIbLoan());
			String apiUrl = ibApiUrl + "credit-service/loan-schedule/create-schedules";
			URI uri = new URI(apiUrl);
			String body = objectMapper.writeValueAsString(loanSchedule);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : save LoanSchedule in IB with idLoanIb = {}",
					loanSchedule.getLoanDTO().getIdIbLoan());

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return loanSchedule;
	}

	/**
	 * Gets the documents from ib.
	 *
	 * @param documentsLoanDTO the documents loan DTO
	 * @return the documents from ib
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#getDocumentsFromIb(com.acm.utils.dtos.
	 * AcmDocumentsDTO)
	 */
	@Override
	public List<AcmDocumentsDTO> getDocumentsFromIb(AcmDocumentsDTO documentsLoanDTO) {

		List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
		try {
			logger.info("BEGIN : get documents from IB with idLoanIb = {}",
					documentsLoanDTO.getLoanId());
			String apiUrl = ibApiUrl + "credit-service/loans-documents/";
			URI uri = new URI(apiUrl);
			String body = objectMapper.writeValueAsString(documentsLoanDTO);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : get documents from IB with idLoanIb = {}",
					documentsLoanDTO.getLoanId());
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				acmDocumentsDTOs = objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<AcmDocumentsDTO>>() {

						});
				for (AcmDocumentsDTO acmDocument : acmDocumentsDTOs) {

					acmDocument
							.setDocumentFile(displayDocument(acmDocument.getIdDocument().toString(),
									acmDocument.getLoanId().toString()));
				}
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return acmDocumentsDTOs;
	}

	/**
	 * Save to ged.
	 *
	 * @param uploadedFiles the uploaded files
	 * @param documentsLoanDTOs the documents loan DT os
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#saveToGed(byte[], java.lang.String)
	 */
	@Override
	public void saveToGed(byte[] uploadedFiles, List<AcmDocumentsDTO> documentsLoanDTOs) {

		try {
			logger.info("BEGIN : save documents in IB  ");
			String apiUrl = ibApiUrl + "credit-service/loans-documents/save-bytes-to-ged";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.MULTIPART_FORM_DATA);
			MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();

			body.add("documentsLoanDTO", objectMapper.writeValueAsString(documentsLoanDTOs));

			body.add("uploadedFiles", uploadedFiles);
			HttpEntity<MultiValueMap<String, Object>> request = new HttpEntity<>(body, headers);
			restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END: save documents in IB ");

		}
		catch (IOException | URISyntaxException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Display document.
	 *
	 * @param id the id
	 * @param loanId the loan id
	 * @return the byte[]
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#displayDocument(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public byte[] displayDocument(String id, String loanId) {

		try {
			logger.info("BEGIN : display documents from IB  ");
			String apiUrl = ibApiUrl + "ged-service/documents/display/?password=&id=" + id
					+ "&loanId=" + loanId;
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_OCTET_STREAM));
			HttpEntity<String> request = new HttpEntity<>("", headers);

			ResponseEntity<byte[]> responseAPI =
					restTemplate.exchange(uri, HttpMethod.GET, request, byte[].class);

			return responseAPI.getBody();

		}
		catch (IOException | URISyntaxException e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Gets the documents from ib and save in acm.
	 *
	 * @param documentsLoanDTO the documents loan DTO
	 * @param idAcmLoan the id acm loan
	 * @return the documents from ib and save in acm
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.api_ib.LoadDataIBService#getDocumentsFromIbAndSaveInAcm(com.acm.utils.dtos.
	 * AcmDocumentsDTO, java.lang.Long)
	 */
	@Override
	public List<AcmDocumentsDTO> getDocumentsFromIbAndSaveInAcm(AcmDocumentsDTO documentsLoanDTO,
			Long idAcmLoan) {

		List<Long> ibDocumentsIds = new ArrayList<>();
		List<Long> settingDocumentTypeIds = new ArrayList<>();
		List<AcmDocumentsDTO> acmDocumentsDTOs;
		List<AcmDocumentsDTO> ibDocumentsDTOToBeSaved;
		List<AcmDocumentsDTO> ibDocumentsDTOs = getDocumentsFromIb(documentsLoanDTO);

		// save ibDocuments'Ids in ibDocumentsIds list
		for (AcmDocumentsDTO documentDTO : ibDocumentsDTOs) {
			ibDocumentsIds.add(documentDTO.getIdDocument());
		}
		// find documents in ACM with the same IbIdDocument
		acmDocumentsDTOs = documentsLoanService.findInIdIbDocuments(ibDocumentsIds);
		// Init ibDocumentsDTOToBeSaved to ibDocuments got from IB
		ibDocumentsDTOToBeSaved = ibDocumentsDTOs;
		for (AcmDocumentsDTO documentDTO : ibDocumentsDTOs) {
			// check if ibDocument already exist in acm
			if (acmDocumentsDTOs.stream()
					.anyMatch(doc -> doc.getIdIbDocument().equals(documentDTO.getIdDocument()))) {
				// remove this item from ibDocumentsDTOToBeSaved
				ibDocumentsDTOToBeSaved = ibDocumentsDTOToBeSaved.stream()
						.filter(doc -> !doc.getIdDocument().equals(documentDTO.getIdDocument()))
						.collect(Collectors.toList());
			}
		}
		// if there is no ibDocument to save in ACM then return
		if (ACMValidationUtils.isNullOrEmpty(ibDocumentsDTOToBeSaved)) {
			return null;
		}

		// List<AcmDocumentsDTO> acmDocumentsDTOToSave = new ArrayList<>();

		List<MultipartFile> multipartFiles = new ArrayList<>();

		// save docs in acm
		logger.info("BEGIN : save documents got from IB in ACM  ");
		// prepare documents to be sent to saveToGed() method
		for (AcmDocumentsDTO documentDTO : ibDocumentsDTOToBeSaved) {
			multipartFiles.add(new CustomMultipartFile(documentDTO.getDocumentFile()));
			documentDTO.setLoanId(idAcmLoan);
			documentDTO.setDocumentSize(documentDTO.getDocumentFile().length);
			documentDTO.setDocumentFile(null);
			documentDTO.setIdIbDocument(documentDTO.getIdDocument());
			settingDocumentTypeIds.add(documentDTO.getSettingDocumentTypeDTO().getId());
			// acmDocumentsDTOToSave.add(documentDTO);
		}

		try {
			// find documents in ACM of current loan and with document type IN (document types of
			// the ibDocumentsToBeSaved in ACM)
			AcmDocumentsDTO documentsLoanDTOParam = new AcmDocumentsDTO();
			documentsLoanDTOParam.setLoanId(idAcmLoan);
			List<AcmDocumentsDTO> acmDocumentsToBeDisabled = documentsLoanService
					.findInSettingDocumentTypeIds(documentsLoanDTOParam, settingDocumentTypeIds);
			// disable old documents in ACM that have the same setting doc type as the new ib
			// documents
			for (AcmDocumentsDTO documentDTO : acmDocumentsToBeDisabled) {
				documentDTO.setEnabled(Boolean.FALSE);
				documentsLoanService.save(documentDTO.getIdDocument(), documentDTO);
			}
			// save documents in acm
			documentsLoanService.saveToGed(
					multipartFiles.toArray(new MultipartFile[ibDocumentsDTOToBeSaved.size()]),
					objectMapper.writeValueAsString(
							ibDocumentsDTOToBeSaved /* acmDocumentsDTOToSave */));

		}
		catch (JsonProcessingException | ResourcesNotFoundException e) {

			e.printStackTrace();
		}
		logger.info("END : save documents got from IB in ACM  ");

		return null;
	}

	/**
	 * Find claims.
	 *
	 * @param acmClaimsDTO the acm claims DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#findClaims(com.acm.utils.dtos.AcmClaimsDTO)
	 */
	@Override
	public List<AcmClaimsDTO> findClaims(AcmClaimsDTO acmClaimsDTO) {

		try {
			String apiUrl = ibApiUrl + "credit-service/claims/";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(acmClaimsDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<AcmClaimsDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Update claims.
	 *
	 * @param acmClaimsDTO the acm claims DTO
	 * @return the acm claims DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#updateClaims(com.acm.utils.dtos.AcmClaimsDTO)
	 */
	@Override
	public AcmClaimsDTO updateClaims(AcmClaimsDTO acmClaimsDTO) {

		try {
			String apiUrl = ibApiUrl + "credit-service/claims/create";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(acmClaimsDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<AcmClaimsDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	/**
	 * Sync setting from ACM.
	 *
	 * @param productsACM the products ACM
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#SyncSettingFromACM(java.util.List)
	 */
	@Override
	public String SyncSettingFromACM(List<ProductDTO> productsACM) {

		try {

			logger.info("BEGIN : Synchronize setting from ACM", productsACM.size());
			String apiUrl = ibApiUrl + "parametrage-service/products/reload-setting";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));

			// convert productsACM to json
			String body = objectMapper.writeValueAsString(productsACM);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : Synchronize setting from ACM");
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(), new TypeReference<String>() {

				});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return "An error occurred in IB";
	}

	/**
	 * Find guarantors.
	 *
	 * @param customerLinksRelationshipDTO the customer links relationship DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.api_ib.LoadDataIBService#findGuarantors(com.acm.utils.dtos.
	 * CustomerLinksRelationshipDTO)
	 */
	@Override
	public List<CustomerLinksRelationshipDTO> findGuarantors(
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO) {

		try {
			logger.info("BEGIN : find guarantors from IB by given parameters ");
			String apiUrl = ibApiUrl + "credit-service/customer-link-relationship/";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(customerLinksRelationshipDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			logger.info("END : find guarantors from IB by given parameters");
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<List<CustomerLinksRelationshipDTO>>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}

	/**
	 * Creates the single sanad el amer.
	 *
	 * @param loanIbId the loan ib id
	 * @return true, if successful
	 * @see com.acm.service.api_ib.LoadDataIBService#createSingleSanadElAmer(java.lang.Long)
	 */
	@Override
	public boolean createSingleSanadElAmer(Long loanIbId) {

		try {
			logger.info("BEGIN : Call api create sanad el amer from IB with loan id = {}",
					loanIbId);
			String apiUrl = ibApiUrl + "transvers-service/sanad-ElAmer-api/create-single-sanad";
			URI uri = new URI(apiUrl);
			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.set("Content-Type", "application/json");
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
			HttpEntity<Long> request = new HttpEntity<>(loanIbId, headers);
			ResponseEntity<Boolean> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, boolean.class);

			logger.info("END : call api single sanad el amer from IB with id = {}", loanIbId);

			return responseAPI.getBody();

		}
		catch (Exception e) {
			logger.error("exception in call api single sanad el amer from IB", e.getMessage());
			return false;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.api_ib.LoadDataIBService#findClaimsPagination(com.acm.utils.dtos.pagination.
	 * ClaimsPaginationDTO)
	 */
	@Override
	public ClaimsPaginationDTO findClaimsPagination(ClaimsPaginationDTO claimsPaginationDTO) {

		Preconditions.checkNotNull(claimsPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		try {
			String apiUrl = ibApiUrl + "credit-service/claims/find-pagination";
			URI uri = new URI(apiUrl);

			HttpHeaders headers = new HttpHeaders();
			headers.set("Authorization", "Bearer " + loginApiIb());
			headers.setContentType(MediaType.APPLICATION_JSON_UTF8);

			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON_UTF8));
			// convert loanIbDTO to json
			String body = objectMapper.writeValueAsString(claimsPaginationDTO);
			HttpEntity<String> request = new HttpEntity<>(body, headers);
			ResponseEntity<String> responseAPI =
					restTemplate.exchange(uri, HttpMethod.POST, request, String.class);
			if (!ACMValidationUtils.isNullOrEmpty(responseAPI.getBody())) {
				return objectMapper.readValue(responseAPI.getBody(),
						new TypeReference<ClaimsPaginationDTO>() {

						});
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		return null;
	}
}
