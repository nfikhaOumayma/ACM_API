/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;
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
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.acm.api_abacus.model.CustomerAbacusAPIModel;
import com.acm.api_abacus.model.CustomerAbacusAPIModelLoanSchedule;
import com.acm.api_abacus.model.CustomerAbacusAPIModelPaymentAllocation;
import com.acm.api_abacus.model.CustomerAbacusAPIModelSurvey;
import com.acm.api_abacus.model.CustomerAbacusAPIModelUdfLink;
import com.acm.api_abacus.model.MetaDataAPIAbacus;
import com.acm.api_abacus.service.CustomerAbacusApiService;
import com.acm.api_abacus.service.LoginApiService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.configuration.rest.RestTemplateConfig;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.RelationshipDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsModelDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.enums.RequestMethode;
import com.acm.utils.enums.TransversHistoryObject;
import com.acm.utils.models.UserDefinedFieldsLinks;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;

/**
 * {@link CustomerAbacusApiServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class CustomerAbacusApiServiceImpl implements CustomerAbacusApiService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(CustomerAbacusApiServiceImpl.class);

	/** The login api service. */
	@Autowired
	private LoginApiService loginApiService;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The url serveur api. */
	@Value("${rest.api.abacus.url.server}")
	private String urlServeurApi;

	/** The uri add customer. */
	@Value("${rest.api.abacus.customer.add.uri}")
	private String uriAddCustomer;

	/** The uri update customer. */
	@Value("${rest.api.abacus.customer.update.uri}")
	private String uriUpdateCustomer;

	/** The uri get customer data. */
	@Value("${rest.api.abacus.customer.get.uri}")
	private String uriGetCustomerData;

	/** The uri get customer data. */
	@Value("${rest.api.abacus.payment.allocation.get.uri}")
	private String uriGetAccountAllocationData;
	/** The rest template. */
	RestTemplate restTemplate;

	// @Autowired
	// private Tracer tracer;

	/**
	 * Save.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.api_abacus.service.LoanAbacusApiService#addCustomer(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public CustomerDTO save(CustomerDTO customerDTO) throws IOException, ApiAbacusException {

		HttpEntity<String> request;
		ResponseEntity<String> responseAPI;
		JSONObject customerJsonObject = new JSONObject();
		// Span saveSpan = tracer.buildSpan("Transvers-service/add-customer : HttpHeaders")
		// .withTag("Customer", "Customer Save Abacus Tracking").start();
		try {
			// init resttemplate
			restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);
			// saveSpan.finish();
			// saveSpan = tracer
			// .buildSpan("Transvers-service/add-customer : customerJsonObject JSONObject")
			// .withTag("Customer", "Customer Save Abacus Tracking").start();
			// build body object (JSON)
			customerJsonObject =
					generateJSONObjectByTypeAndAction(customerDTO, CommonConstants.ADD);
			// saveSpan.finish();
			// saveSpan = tracer
			// .buildSpan("Transvers-service/add-customer : customerJsonObject JSONObject")
			// .withTag("Customer", "Customer Save Abacus Tracking").start();
			logger.info("************ REQUEST : ADD CUSTOMER ************");
			logger.info("{}", customerJsonObject);
			logger.info("**************** ADD CUSTOMER *****************");
			// init request
			request = new HttpEntity<>(customerJsonObject.toString(), headers);

			// init URI
			String accessUrl = urlServeurApi + uriAddCustomer;
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using POST method
			responseAPI = restTemplate.postForEntity(uri, request, String.class);
			// mapping result
			logger.debug("{}", responseAPI.getBody());
			// saveSpan.finish();
			// saveSpan = tracer.buildSpan("Transvers-service/add-customer :
			// TransversHistoriqueDTO")
			// .withTag("Customer", "Customer Save Abacus Tracking").start();

			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.CUSTOMER.name(), RequestMethode.POST.name(),
					uri.toString(), responseAPI.getStatusCode().toString(),
					customerJsonObject.toString(), responseAPI.getBody());

			logger.info("transversHistorique add Customer = {}", transversHistoriqueDTO.toString());
			// // add history of request and response api create customer in transvers historique
			// table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}
			// saveSpan.finish();
			// saveSpan = tracer.buildSpan("Transvers-service/add-customer : Mapping Result")
			// .withTag("Customer", "Customer Save Abacus Tracking").start();
			// mapping result
			ObjectMapper mapper = new ObjectMapper();
			JsonNode node = mapper.readTree(responseAPI.getBody());
			customerDTO.setCustomerIdExtern(Long.valueOf(node.path("customerID").asText()));
			customerDTO.setCustomerNumber(node.path("number").asText());
			// set correspondance name to null because we cannot updated in the future if the
			// customer change the name
			customerDTO.setCorrespondanceName(null);
			customerDTO.setCustomerOpenDate(new Date());
			customerDTO.setIsCustomer(Boolean.TRUE);

			// parse json array value to get PERSONID
			ArrayNode personsNode = (ArrayNode) node.get("persons");
			if (!ACMValidationUtils.isNullOrEmpty(personsNode)) {
				Iterator<JsonNode> personsIterator = personsNode.elements();
				while (personsIterator.hasNext()) {
					JsonNode personeNode = personsIterator.next();
					customerDTO.setPersonIdExtern(personeNode.get("personID").asLong());
				}
			}
			else {
				customerDTO.setPersonIdExtern(0L);
			}

			// parse json array value to get UDF data
			ArrayNode surveysNode = (ArrayNode) node.get("surveys");
			List<UserDefinedFieldsLinksDTO> retrunedUserDefinedFieldsLinksDTO =
					parseABACUSResponseUDFs(surveysNode, customerDTO);
			// setting list UDF
			customerDTO.setUserDefinedFieldsLinksDTOs(retrunedUserDefinedFieldsLinksDTO);
			// saveSpan.finish();
			return customerDTO;
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" add customer RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" add customer ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl = urlServeurApi + uriAddCustomer;

			sendTranverseHistorique(TransversHistoryObject.CUSTOMER.name(),
					RequestMethode.POST.name(), accessUrl, String.valueOf(e.getRawStatusCode()),
					customerJsonObject.toString(), e.getResponseBodyAsString());

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
			logger.error("Error has been occured while consuming add customer API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}

	}

	/**
	 * Parses the ABACUS response UDFs.
	 * 
	 * @author HaythemBenizid
	 * @param surveysNode the surveys node
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	private List<UserDefinedFieldsLinksDTO> parseABACUSResponseUDFs(ArrayNode surveysNode,
			CustomerDTO customerDTO) {

		logger.debug("parse ABACUS Response UDFs :: START");
		List<UserDefinedFieldsLinksDTO> retrunedUserDefinedFieldsLinksDTO = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(surveysNode)) {
			Iterator<JsonNode> surveysIterator = surveysNode.elements();
			while (surveysIterator.hasNext()) {
				JsonNode surveysUDFsNode = surveysIterator.next();
				Long surveyID = surveysUDFsNode.get("surveyID").asLong();
				Long userDefinedFieldGroupID =
						surveysUDFsNode.get("userDefinedFieldGroupID").asLong();

				logger.debug("surveyID =  {}", surveyID);
				logger.debug("userDefinedFieldGroupID =  {}", userDefinedFieldGroupID);
				// list UDF links
				ArrayNode udfLinksNode = (ArrayNode) surveysUDFsNode.get("udfLinks");
				if (!ACMValidationUtils.isNullOrEmpty(udfLinksNode)) {
					Iterator<JsonNode> udfLinksIterator = udfLinksNode.elements();
					while (udfLinksIterator.hasNext()) {
						JsonNode linkUDFsNode = udfLinksIterator.next();
						String udfFieldValue = linkUDFsNode.get("value").asText();
						// only add UDF with no empty VALUE
						if (!ACMValidationUtils.isNullOrEmpty(udfFieldValue)) {
							UserDefinedFieldsLinksDTO udfLinkDTO =
									new UserDefinedFieldsLinksDTO(
											new UserDefinedFieldsDTO(
													new UserDefinedFieldGroupDTO(
															userDefinedFieldGroupID),
													linkUDFsNode.get("udfFieldID").asLong(),
													linkUDFsNode.get("fieldName").asText()),
											null, customerDTO.getCustomerIdExtern(), udfFieldValue,
											linkUDFsNode.get("udfLinkID").asLong(), surveyID,
											linkUDFsNode.get("userDefinedFieldListID").asLong());
							retrunedUserDefinedFieldsLinksDTO.add(udfLinkDTO);
						}
					}
				}
			}
		}
		logger.debug("{} : UDF elements parsed.", retrunedUserDefinedFieldsLinksDTO.size());
		logger.debug("parse ABACUS Response UDFs :: DONE");
		return retrunedUserDefinedFieldsLinksDTO;
	}

	/**
	 * Update.
	 *
	 * @param customerDTO the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.LoanAbacusApiService#updateCustomer(com.acm.utils.dtos.
	 * CustomerDTO)
	 */
	@Override
	public void update(CustomerDTO customerDTO) throws IOException, ApiAbacusException {

		HttpEntity<String> request;
		ResponseEntity<String> responseEntity;
		JSONObject customerJsonObject = new JSONObject();
		try {
			// init resttemplate
			restTemplate = RestTemplateConfig.initRestTemplate();

			// init headers
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));

			// setting token
			headers = loginApiService.settingHttpHeaders(headers);

			// build body object (JSON)
			customerJsonObject =
					generateJSONObjectByTypeAndAction(customerDTO, CommonConstants.UPDATE);
			logger.info("************ REQUEST : UPDATE CUSTOMER ************");
			logger.info("{}", customerJsonObject);
			logger.info("************** UPDATE CUSTOMER *******************");
			// init request
			request = new HttpEntity<>(customerJsonObject.toString(), headers);

			// init URI
			String accessUrl =
					urlServeurApi + uriUpdateCustomer + customerDTO.getCustomerIdExtern();
			URI uri = new URI(accessUrl);
			logger.info("uri = {}", uri);
			// sending request to server using PUT method
			responseEntity = restTemplate.exchange(uri, HttpMethod.PUT, request, String.class);

			TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(
					TransversHistoryObject.CUSTOMER.name(), RequestMethode.PUT.name(),
					uri.toString(), responseEntity.getStatusCode().toString(),
					customerJsonObject.toString(), responseEntity.getBody());

			logger.info("transversHistorique edit Customer = {}", transversHistoriqueDTO);
			// add history of request and response api update customer in transvers historique table
			if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
				creditClient.create(transversHistoriqueDTO);
			}

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" Update customer RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" Update customer ResponseBodyAsString = {}", e.getResponseBodyAsString());
			logger.error(" RestClientResponseException = {}", e.getMessage());

			String accessUrl =
					urlServeurApi + uriUpdateCustomer + customerDTO.getCustomerIdExtern();
			sendTranverseHistorique(TransversHistoryObject.CUSTOMER.name(),
					RequestMethode.PUT.name(), accessUrl, String.valueOf(e.getRawStatusCode()),
					customerJsonObject.toString(), e.getResponseBodyAsString());

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
			logger.error(
					"Error has been occurred while consuming update customer API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}

	}

	/**
	 * Generate JSON object by type ({@link CustomerType}) and action (ADD or UPDATE).
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject generateJSONObjectByTypeAndAction(CustomerDTO customerDTO, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		JSONObject jsonObject = new JSONObject();
		switch (action) {
			case CommonConstants.ADD:
				if (customerDTO.getCustomerType().equals(CustomerType.INDIV.name())) {
					jsonObject = initJSONObjectCustomerINDIV(customerDTO, CommonConstants.ADD);
				}
				else if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
					jsonObject = initJSONObjectCustomerGroupe(customerDTO, CommonConstants.ADD);
				}
				if (customerDTO.getCustomerType().equals(CustomerType.ORG.name())) {
					jsonObject =
							initJSONObjectCustomerOrganisation(customerDTO, CommonConstants.ADD);
				}
				break;

			case CommonConstants.UPDATE:
				if (customerDTO.getCustomerType().equals(CustomerType.INDIV.name())) {
					jsonObject = initJSONObjectCustomerINDIV(customerDTO, CommonConstants.UPDATE);
					jsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
							customerDTO.getCustomerIdExtern());
				}
				else if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
					jsonObject = initJSONObjectCustomerGroupe(customerDTO, CommonConstants.UPDATE);
					jsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
							customerDTO.getCustomerIdExtern());
				}
				if (customerDTO.getCustomerType().equals(CustomerType.ORG.name())) {
					jsonObject =
							initJSONObjectCustomerOrganisation(customerDTO, CommonConstants.UPDATE);
					jsonObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
							customerDTO.getCustomerIdExtern());
				}
				break;

			default:
				break;
		}
		return jsonObject;
	}

	/**
	 * Inits the JSON object customer INDIV.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject initJSONObjectCustomerINDIV(CustomerDTO customerDTO, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		JSONObject customerJsonObject = new JSONObject();
		// Customer Type INDIV = 1
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_TYPE.fieldName(), 1);
		customerJsonObject.put(MetaDataAPIAbacus.BRANCH_ID.fieldName(),
				ACMValidationUtils.isNullOrEmpty(customerDTO.getBranchId()) ? ""
						: customerDTO.getBranchId());
		customerJsonObject.put(MetaDataAPIAbacus.DEFAULT_CU_ACCOUNT_PORTFOLIO_ID.fieldName(),
				ACMValidationUtils.isNullOrEmpty(customerDTO.getAccountPortfolioID()) ? 1
						: customerDTO.getAccountPortfolioID());
		// default Status = 1 (active customer)
		customerJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 1);
		customerJsonObject.put(MetaDataAPIAbacus.DATE_JOINED.fieldName(),
				java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));

		// Creating a json array
		JSONArray arrayPersonne = buildPersonsJsonObject(customerDTO, action);
		customerJsonObject.put(MetaDataAPIAbacus.PERSONS.fieldName(), arrayPersonne);

		// Creating a json array
		JSONArray arrayAddress = buildAddressJsonObject(customerDTO, action);
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ADDRESS.fieldName(), arrayAddress);

		// Creating a json array
		JSONArray arrayCustomerRelationships = buildRelationshipsJsonObject(customerDTO);
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_RELATIONSHIPS.fieldName(),
				arrayCustomerRelationships);

		// Creating a json array surveys
		JSONArray arraySurveys = buildSurveysJsonObject(customerDTO, action);
		customerJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);
		return customerJsonObject;
	}

	/**
	 * Inits the JSON object customer groupe.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private JSONObject initJSONObjectCustomerGroupe(CustomerDTO customerDTO, String action)
			throws ApiAbacusException, IOException {

		JSONObject customerJsonObject = new JSONObject();
		// Customer Type
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_TYPE.fieldName(), 8);
		customerJsonObject.put(MetaDataAPIAbacus.BRANCH_ID.fieldName(),
				ACMValidationUtils.isNullOrEmpty(customerDTO.getBranchId()) ? ""
						: customerDTO.getBranchId());
		customerJsonObject.put(MetaDataAPIAbacus.DEFAULT_CU_ACCOUNT_PORTFOLIO_ID.fieldName(), 1);
		customerJsonObject.put(MetaDataAPIAbacus.NAME.fieldName(),
				customerDTO.getSolidarityName() != null ? customerDTO.getSolidarityName() : "");
		customerJsonObject.put(MetaDataAPIAbacus.ALT_NAME.fieldName(),
				customerDTO.getSolidarityName() != null ? customerDTO.getSolidarityName() : "");
		customerJsonObject.put(MetaDataAPIAbacus.CORRESPONDENCE_NAME.fieldName(),
				customerDTO.getSolidarityName() != null ? customerDTO.getSolidarityName() : "");
		customerJsonObject.put(MetaDataAPIAbacus.TRACK_ACCS_INDIVIDUALLY.fieldName(), Boolean.TRUE);
		// Status 1 active customer
		customerJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 1);
		customerJsonObject.put(MetaDataAPIAbacus.DATE_JOINED.fieldName(),
				java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));

		// Creating a json array
		JSONArray arrayCustomers = new JSONArray();
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerLinksRelationshipDTOs())) {
			for (CustomerLinksRelationshipDTO customerLinksRelationshipDTO : customerDTO
					.getCustomerLinksRelationshipDTOs()) {
				if (customerLinksRelationshipDTO.getCategory() != null
						&& customerLinksRelationshipDTO.getCategory()
								.equals(LinkRelationshipsCategory.MEMBERS.name())) {
					JSONObject customerJSONObject = new JSONObject();

					// create user ABACUS if not exist
					if (customerLinksRelationshipDTO.getMember().getCustomerIdExtern() == 0) {
						// setting missing data
						if (ACMValidationUtils.isNullOrEmpty(
								customerLinksRelationshipDTO.getMember().getListAddress())) {
							customerLinksRelationshipDTO.getMember()
									.setListAddress(customerDTO.getListAddress());
						}
						customerLinksRelationshipDTO.getMember()
								.setCustomerType(CustomerType.INDIV.name());
						// save new customer in ABACUS DB
						CustomerDTO customerABACUS = save(customerLinksRelationshipDTO.getMember());
						customerJSONObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
								customerABACUS.getCustomerIdExtern());
					}
					else {
						customerJSONObject.put(MetaDataAPIAbacus.CUSTOMER_ID.fieldName(),
								customerLinksRelationshipDTO.getMember().getCustomerIdExtern());
					}
					customerJSONObject.put(MetaDataAPIAbacus.COMMENTS.fieldName(), "");
					customerJSONObject.put(MetaDataAPIAbacus.COMMUNITY_STATUS.fieldName(), 1);
					Integer relationShipType =
							customerLinksRelationshipDTO.getLinkRelationshipType() != null
									&& customerLinksRelationshipDTO.getLinkRelationshipType()
											.equals("Head") ? 1 : 2;
					customerJSONObject.put(MetaDataAPIAbacus.CUSTOMER_ROLE.fieldName(),
							relationShipType);
					arrayCustomers.put(customerJSONObject);
				}
			}
		}
		customerJsonObject.put(MetaDataAPIAbacus.COMMUNITY_CUSTOMERS.fieldName(), arrayCustomers);

		// Creating a json array
		JSONArray arrayAddress = buildAddressJsonObject(customerDTO, action);
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ADDRESS.fieldName(), arrayAddress);

		// Creating a json array
		JSONArray arrayCustomerRelationships = buildRelationshipsJsonObject(customerDTO);
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_RELATIONSHIPS.fieldName(),
				arrayCustomerRelationships);

		return customerJsonObject;
	}

	/**
	 * Inits the JSON object customer organisation.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the JSON object
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONObject initJSONObjectCustomerOrganisation(CustomerDTO customerDTO, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		JSONObject customerJsonObject = new JSONObject();
		// Customer Type
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_TYPE.fieldName(), 4);
		customerJsonObject.put(MetaDataAPIAbacus.BRANCH_ID.fieldName(),
				ACMValidationUtils.isNullOrEmpty(customerDTO.getBranchId()) ? ""
						: customerDTO.getBranchId());
		customerJsonObject.put(MetaDataAPIAbacus.DEFAULT_CU_ACCOUNT_PORTFOLIO_ID.fieldName(), 1);
		customerJsonObject.put(MetaDataAPIAbacus.NAME.fieldName(),
				customerDTO.getOrganizationName() != null ? customerDTO.getOrganizationName() : "");
		customerJsonObject.put(MetaDataAPIAbacus.ALT_NAME.fieldName(),
				customerDTO.getOrganizationName() != null ? customerDTO.getOrganizationName() : "");
		customerJsonObject.put(MetaDataAPIAbacus.CORRESPONDENCE_NAME.fieldName(),
				customerDTO.getOrganizationName() != null ? customerDTO.getOrganizationName() : "");
		// Status 1 active customer
		customerJsonObject.put(MetaDataAPIAbacus.STATUS.fieldName(), 1);
		customerJsonObject.put(MetaDataAPIAbacus.DATE_JOINED.fieldName(),
				java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(new Date())));

		// json organisation
		JSONObject organisationJSONObject = new JSONObject();
		organisationJSONObject.put(MetaDataAPIAbacus.REG_NUMBER.fieldName(), "");
		organisationJSONObject.put(MetaDataAPIAbacus.TYPE.fieldName(),
				customerDTO.getIndustryCode() != null
						? customerDTO.getIndustryCode().getIndustryID()
						: 1);
		organisationJSONObject.put(MetaDataAPIAbacus.TELEPHONE_1.fieldName(),
				customerDTO.getTelephone1() != null ? customerDTO.getTelephone1() : "");
		organisationJSONObject.put(MetaDataAPIAbacus.TELEPHONE_2.fieldName(),
				customerDTO.getTelephone2() != null ? customerDTO.getTelephone2() : "");
		organisationJSONObject.put(MetaDataAPIAbacus.FAX.fieldName(),
				customerDTO.getFax() != null ? customerDTO.getFax() : "");
		organisationJSONObject.put(MetaDataAPIAbacus.WEB_SITE.fieldName(),
				customerDTO.getWebSite() != null ? customerDTO.getWebSite() : "");
		organisationJSONObject.put(MetaDataAPIAbacus.EMAIL.fieldName(),
				customerDTO.getEmail() != null ? customerDTO.getEmail() : "");
		organisationJSONObject.put(MetaDataAPIAbacus.ACCOUNTS_YEAR_END.fieldName(),
				customerDTO.getAccountYearEnd() != null
						? java.sql.Date.valueOf(DateUtil
								.convertToLocalDateViaInstant(customerDTO.getAccountYearEnd()))
						: java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
								DateUtil.getLastDateOfYear(new Date()))));
		customerJsonObject.put(MetaDataAPIAbacus.ORGANISATION.fieldName(), organisationJSONObject);

		// Creating a json array
		JSONArray arrayPersonne = buildPersonsForOrganisationJsonObject(customerDTO);
		customerJsonObject.put(MetaDataAPIAbacus.PERSONS.fieldName(), arrayPersonne);

		// Creating a json array
		JSONArray arrayAddress = buildAddressJsonObject(customerDTO, action);
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_ADDRESS.fieldName(), arrayAddress);

		// Creating a json array
		JSONArray arrayCustomerRelationships = buildRelationshipsJsonObject(customerDTO);
		customerJsonObject.put(MetaDataAPIAbacus.CUSTOMER_RELATIONSHIPS.fieldName(),
				arrayCustomerRelationships);

		// Creating a json array surveys
		JSONArray arraySurveys = buildSurveysJsonObject(customerDTO, action);
		customerJsonObject.put(MetaDataAPIAbacus.SURVEYS.fieldName(), arraySurveys);

		return customerJsonObject;
	}

	/**
	 * Builds the surveys {@link UserDefinedFieldsLinks} JSON object.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the JSON array
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	private JSONArray buildSurveysJsonObject(CustomerDTO customerDTO, String action)
			throws ApiAbacusException, IOException, URISyntaxException {

		customerDTO
				.setUserDefinedFieldsLinksDTOs(customerDTO.getUserDefinedFieldsLinksDTOs().stream()
						.filter(udfLink ->  (udfLink.getUserDefinedFieldsDTO().getIdUDFField() != 0))
						.collect(Collectors.toList()));
		JSONArray arraySurveys = new JSONArray();
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getUserDefinedFieldsLinksDTOs())) {
			// get list of GROUP GROUP INDEX
			List<Long> groupIDindex = new ArrayList<>();

			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : customerDTO
					.getUserDefinedFieldsLinksDTOs()) {
				groupIDindex.add(userDefinedFieldsLinksDTO.getIndexGroup());

			}
			// filter list group ID (Remove Duplicates)
			List<Long> groupIDindexWithoutDuplicates = new ArrayList<>(new HashSet<>(groupIDindex));

			// get list of GROUP ID ABACUS
			Map<Long, Long> groupIDList = new HashMap<>();
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : customerDTO
					.getUserDefinedFieldsLinksDTOs()) {
				if (groupIDindexWithoutDuplicates
						.indexOf(userDefinedFieldsLinksDTO.getIndexGroup()) != -1) {
					groupIDList.put(userDefinedFieldsLinksDTO.getIndexGroup(),
							userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO()
									.getUserDefinedFieldGroupDTO().getIdUDGroupAbacus());
					groupIDindexWithoutDuplicates.remove(groupIDindexWithoutDuplicates
							.indexOf(userDefinedFieldsLinksDTO.getIndexGroup()));
				}
			}
			List<UDFLinksGroupeFieldsDTO> udfGroupeFields = new ArrayList<>();
			for (Map.Entry<Long, Long> mapGroupIndex : groupIDList.entrySet()) {
				Long idUDFGroup = mapGroupIndex.getValue();
				Long indexGroup = mapGroupIndex.getKey();
				logger.debug("*** idUDFGroup = {} ***", idUDFGroup);
				// init object
				UDFLinksGroupeFieldsDTO udfGroupeField = new UDFLinksGroupeFieldsDTO();
				udfGroupeField.setDate(new Date());
				udfGroupeField.setTotalScore(0L);
				udfGroupeField.setUserDefinedFieldGroupID(idUDFGroup);
				List<UDFLinksGroupeFieldsModelDTO> udfGroupeFieldsModels = new ArrayList<>();
				for (UserDefinedFieldsLinksDTO dto : customerDTO.getUserDefinedFieldsLinksDTOs()) {
					if (dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO()
							.getIdUDGroupAbacus().equals(idUDFGroup)
							&& dto.getIndexGroup().equals(indexGroup)) {
						UDFLinksGroupeFieldsModelDTO udfGroupeFieldsModel =
								new UDFLinksGroupeFieldsModelDTO(
										dto.getUserDefinedFieldsDTO().getIdUDFField(),
										dto.getFieldValue(), dto.getUdfListValueId(), 1,
										dto.getUserDefinedFieldsDTO().getFieldMasc(),
										dto.getUserDefinedFieldsDTO().getMandatory(),
										dto.getUserDefinedFieldsDTO().getUniqueField(),
										Boolean.FALSE, dto.getUserDefinedFieldsDTO().getFieldType(),
										dto.getUserDefinedFieldsDTO().getName(), "1", null,
										dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO()
												.getCustomerType());
						// setting IDs
						udfGroupeFieldsModel.setId(dto.getId());
						udfGroupeFieldsModel.setIdAbacusUDFLink(dto.getIdAbacusUDFLink());
						udfGroupeFieldsModel.setSurveysId(dto.getSurveysId());
						if (dto.getSurveysId() != null) {
							udfGroupeField.setSurveysId(dto.getSurveysId());
						}
						udfGroupeFieldsModels.add(udfGroupeFieldsModel);
					}
				}
				udfGroupeField.setUdfGroupeFieldsModels(udfGroupeFieldsModels);
				udfGroupeFields.add(udfGroupeField);
			}
			// build json schema
			for (UDFLinksGroupeFieldsDTO udfFields : udfGroupeFields) {
				JSONObject surveysJSONObject = new JSONObject();
				surveysJSONObject.put(MetaDataAPIAbacus.USER_DEFINED_FIELD_GROUP_ID.fieldName(),
						udfFields.getUserDefinedFieldGroupID());
				surveysJSONObject.put(MetaDataAPIAbacus.DATE.fieldName(), java.sql.Date
						.valueOf(DateUtil.convertToLocalDateViaInstant(udfFields.getDate())));
				if (CommonConstants.UPDATE.equals(action)
						&& !ACMValidationUtils.isNullOrEmpty(udfFields.getSurveysId())) {
					// add surveys ID (cas UPDATE)
					surveysJSONObject.put(MetaDataAPIAbacus.SURVEY_ID.fieldName(),
							udfFields.getSurveysId());
				}
				// Default value = 0
				surveysJSONObject.put(MetaDataAPIAbacus.TOTAL_SCORE.fieldName(), 0);
				// build udfLinks
				JSONArray arrayUdfLinks = new JSONArray();
				for (UDFLinksGroupeFieldsModelDTO udfGroupeFieldsModel : udfFields
						.getUdfGroupeFieldsModels()) {
					JSONObject udfLinksJSONObject = new JSONObject();
					if (CommonConstants.UPDATE.equals(action) && !ACMValidationUtils
							.isNullOrEmpty(udfGroupeFieldsModel.getIdAbacusUDFLink())) {
						// add UDF links ID (cas UPDATE)
						udfLinksJSONObject.put(MetaDataAPIAbacus.UDFLINK_ID.fieldName(),
								udfGroupeFieldsModel.getIdAbacusUDFLink());
					}
					// setting ID field in ABACUS
					udfLinksJSONObject.put(MetaDataAPIAbacus.UDF_FIELD_ID.fieldName(),
							udfGroupeFieldsModel.getUdfFieldID());
					udfLinksJSONObject.put(MetaDataAPIAbacus.VALUE.fieldName(),
							udfGroupeFieldsModel.getValue() != null
									? udfGroupeFieldsModel.getValue()
									: "");
					udfLinksJSONObject.put(MetaDataAPIAbacus.USER_DEFINED_FIELD_LIST_ID.fieldName(),
							udfGroupeFieldsModel.getUserDefinedFieldListID());
					udfLinksJSONObject.put(MetaDataAPIAbacus.ORDER.fieldName(),
							udfGroupeFieldsModel.getOrder());
					udfLinksJSONObject.put(MetaDataAPIAbacus.MASK.fieldName(),
							udfGroupeFieldsModel.getMask());
					udfLinksJSONObject.put(MetaDataAPIAbacus.MANDATORY.fieldName(),
							udfGroupeFieldsModel.getMandatory());
					udfLinksJSONObject.put(MetaDataAPIAbacus.UNIQUE_FIELD.fieldName(),
							udfGroupeFieldsModel.getUniqueField());
					udfLinksJSONObject.put(MetaDataAPIAbacus.VALIDATE_ACTIVE_UDF.fieldName(),
							udfGroupeFieldsModel.getValidateActiveUDF());
					udfLinksJSONObject.put(MetaDataAPIAbacus.UDF_TYPE.fieldName(),
							udfGroupeFieldsModel.getUdfType());
					udfLinksJSONObject.put(MetaDataAPIAbacus.UDF_FIELD_NAME.fieldName(),
							udfGroupeFieldsModel.getFieldName());
					udfLinksJSONObject.put(MetaDataAPIAbacus.FIELD_CURRENCY.fieldName(),
							udfGroupeFieldsModel.getFieldCurrency());
					udfLinksJSONObject.put(MetaDataAPIAbacus.DATE_VALUE.fieldName(),
							udfGroupeFieldsModel.getDateValue() != null
									? java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
											udfGroupeFieldsModel.getDateValue()))
									: java.sql.Date.valueOf(
											DateUtil.convertToLocalDateViaInstant(new Date())));
					udfLinksJSONObject.put(MetaDataAPIAbacus.CUSTOMER_TYPES.fieldName(),
							udfGroupeFieldsModel.getCustomerTypes());
					udfLinksJSONObject.put(MetaDataAPIAbacus.SURVEY_ID.fieldName(),
							udfFields.getSurveysId());
					arrayUdfLinks.put(udfLinksJSONObject);
				}
				surveysJSONObject.put(MetaDataAPIAbacus.UDF_LINKS.fieldName(), arrayUdfLinks);
				arraySurveys.put(surveysJSONObject);
			}
		}
		if (CommonConstants.UPDATE.equals(action)) {
			// get loan DATA
			CustomerAbacusAPIModel oldCustomerAbacusAPIModel =
					getData(customerDTO.getCustomerIdExtern());
			if (oldCustomerAbacusAPIModel != null) {
				String initialJson =
						CommonFunctions.convertObjectToJSONString(oldCustomerAbacusAPIModel);
				logger.info("Get CUSTOMER ABACUS JSON : {}", initialJson);
				// builds && update JSON
				JSONObject customerJsonObject = new JSONObject(initialJson);
				// builds && update JSON
				JSONArray arraySurveysAbacus = customerJsonObject.getJSONArray("surveys");
				try {
					final ObjectMapper objectMapper = new ObjectMapper();
					// get & map JSON ABACUS
					CustomerAbacusAPIModelSurvey[] abacusSurveys = objectMapper.readValue(
							arraySurveysAbacus.toString(), CustomerAbacusAPIModelSurvey[].class);
					List<CustomerAbacusAPIModelSurvey> abacusSurveyList =
							new ArrayList<>(Arrays.asList(abacusSurveys));
					// get & map JSON ACM
					CustomerAbacusAPIModelSurvey[] acmSurvey = objectMapper.readValue(
							arraySurveys.toString(), CustomerAbacusAPIModelSurvey[].class);
					List<CustomerAbacusAPIModelSurvey> acmSurveyList =
							new ArrayList<>(Arrays.asList(acmSurvey));

					// Update existing UDF
					for (CustomerAbacusAPIModelSurvey abacusSurveyObject : abacusSurveyList) {
						// Get surveyID && userDefinedFieldGroupID
						int surveyID = abacusSurveyObject.getSurveyID();
						int userDefinedFieldGroupID =
								abacusSurveyObject.getUserDefinedFieldGroupID();
						// Find surveys by surveyID && userDefinedFieldGroupID
						CustomerAbacusAPIModelSurvey acmSurveyObject = acmSurveyList.stream()
								.filter(s -> s.getSurveyID() == surveyID && s
										.getUserDefinedFieldGroupID() == userDefinedFieldGroupID)
								.findFirst().orElse(null);
						// Check if surveys exist
						if (acmSurveyObject != null) {
							List<CustomerAbacusAPIModelUdfLink> abacusUDFs =
									abacusSurveyObject.getUdfLinks();
							List<CustomerAbacusAPIModelUdfLink> acmUDFs =
									acmSurveyObject.getUdfLinks();
							for (CustomerAbacusAPIModelUdfLink abacusUDF : abacusUDFs) {
								// Get udfFieldID
								int udfFieldID = abacusUDF.getUdfFieldID();
								// Find UDF by udfFieldID
								CustomerAbacusAPIModelUdfLink acmUDFObject = acmUDFs.stream()
										.filter(s -> s.getUdfFieldID() == udfFieldID).findFirst()
										.orElse(null);
								// Check if UDF exist
								if (acmUDFObject != null) {
									abacusUDF.setValue(acmUDFObject.getValue());
								}
								else {
									abacusUDF.setValue("");
								}
							}
						}
						else {
							for (CustomerAbacusAPIModelUdfLink abacusUDF : abacusSurveyObject
									.getUdfLinks()) {
								abacusUDF.setValue("");
							}
						}
					}
					// Add new UDF if exist
					for (CustomerAbacusAPIModelSurvey acmSurveyObject : acmSurveyList) {
						// check survey ID = 0 => new udf group to add
						if (acmSurveyObject.getSurveyID() == 0) {
							abacusSurveyList.add(acmSurveyObject);
						}
					}
					JSONArray arrayResultatSurveys = new JSONArray(abacusSurveyList);
					logger.debug("*** array final Surveys = {} ***", arrayResultatSurveys);
					return arrayResultatSurveys;
				}
				catch (JsonParseException | JsonMappingException e) {
					logger.error("Error : JsonParseException | JsonMappingException");
					e.printStackTrace();
				}
				catch (IOException e) {
					logger.error("Error : IOException");
					e.printStackTrace();
				}
			}
		}
		return arraySurveys;
	}

	/**
	 * Builds the address json object.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action (ADD / UPDATE)
	 * @return the JSON array
	 */
	private JSONArray buildAddressJsonObject(CustomerDTO customerDTO, String action) {

		// build json schema
		JSONArray arrayAddress = new JSONArray();
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getListAddress())) {
			for (AddressDTO addressDTO : customerDTO.getListAddress()) {
				JSONObject arrayAddressObject = new JSONObject();
				// setting type and primary address
				arrayAddressObject.put(MetaDataAPIAbacus.ADDRESS_TYPE_ID.fieldName(),
						addressDTO.getAddressTypeId() != null ? addressDTO.getAddressTypeId() : 1);
				Integer isPrimaryAddress = addressDTO.getIsPrimary() != null
						&& Boolean.TRUE.equals(addressDTO.getIsPrimary()) ? 1 : 0;
				arrayAddressObject.put(MetaDataAPIAbacus.IS_PRIMARY.fieldName(), isPrimaryAddress);

				arrayAddressObject.put(MetaDataAPIAbacus.DATE_MOVED_IN.fieldName(),
						addressDTO.getDateMovedIn() != null
								? java.sql.Date.valueOf(DateUtil
										.convertToLocalDateViaInstant(addressDTO.getDateMovedIn()))
								: java.sql.Date.valueOf(
										DateUtil.convertToLocalDateViaInstant(new Date())));
				arrayAddressObject.put(MetaDataAPIAbacus.DATE_MOVED_OUT.fieldName(),
						addressDTO.getDateMovedOut() != null
								? java.sql.Date.valueOf(DateUtil
										.convertToLocalDateViaInstant(addressDTO.getDateMovedOut()))
								: JSONObject.NULL);
				JSONObject addressJSONObject = new JSONObject();
				addressJSONObject.put(MetaDataAPIAbacus.ADDRESS_1.fieldName(),
						addressDTO.getAddress1() != null ? addressDTO.getAddress1() : "");
				addressJSONObject.put(MetaDataAPIAbacus.ADDRESS_2.fieldName(),
						addressDTO.getAddress2() != null ? addressDTO.getAddress2() : "");
				addressJSONObject.put(MetaDataAPIAbacus.ADDRESS_3.fieldName(),
						addressDTO.getAddress3() != null ? addressDTO.getAddress3() : "");
				addressJSONObject.put(MetaDataAPIAbacus.TOWN_CITY.fieldName(),
						addressDTO.getTownCity() != null ? addressDTO.getTownCity() : "");
				addressJSONObject.put(MetaDataAPIAbacus.COUNTY.fieldName(),
						addressDTO.getCounty() != null ? addressDTO.getCounty() : "");
				addressJSONObject.put(MetaDataAPIAbacus.STATE.fieldName(),
						addressDTO.getState() != null ? addressDTO.getState() : "");
				addressJSONObject.put(MetaDataAPIAbacus.POSTAL_CODE.fieldName(),
						addressDTO.getPostalCode() != null ? addressDTO.getPostalCode() : "");
				addressJSONObject.put(MetaDataAPIAbacus.COUNTRY.fieldName(),
						addressDTO.getCountry() != null ? addressDTO.getCountry() : "");
				addressJSONObject.put(MetaDataAPIAbacus.REGION.fieldName(),
						addressDTO.getRegion() != null ? addressDTO.getRegion() : "");
				addressJSONObject.put(MetaDataAPIAbacus.ADDRESS_1_ID.fieldName(),
						addressDTO.getAddress1Id() != null ? addressDTO.getAddress1Id() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.ADDRESS_2_ID.fieldName(),
						addressDTO.getAddress2Id() != null ? addressDTO.getAddress2Id() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.ADDRESS_3_ID.fieldName(),
						addressDTO.getAddress3Id() != null ? addressDTO.getAddress3Id() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.TOWN_CITY_ID.fieldName(),
						addressDTO.getTownCityId() != null ? addressDTO.getTownCityId() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.COUNTY_ID.fieldName(),
						addressDTO.getCountyId() != null ? addressDTO.getCountyId() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.STATE_ID.fieldName(),
						addressDTO.getStateId() != null ? addressDTO.getStateId() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.POSTAL_CODE_ID.fieldName(),
						addressDTO.getPostalCodeId() != null ? addressDTO.getPostalCodeId() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.COUNTRY_ID.fieldName(),
						addressDTO.getCountryId() != null ? addressDTO.getCountryId() : 0);
				addressJSONObject.put(MetaDataAPIAbacus.REGION_ID.fieldName(),
						addressDTO.getRegionId() != null ? addressDTO.getRegionId() : 0);

				arrayAddressObject.put(MetaDataAPIAbacus.ADDRESS.fieldName(), addressJSONObject);
				arrayAddress.put(arrayAddressObject);
			}
		}
		return arrayAddress;
	}

	/**
	 * Builds the persons json object.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the JSON array
	 */
	private JSONArray buildPersonsJsonObject(CustomerDTO customerDTO, String action) {

		// build json schema
		JSONArray arrayPersonne = new JSONArray();
		JSONObject personsJSONObject = new JSONObject();
		// Add Pesron ID mode Update
		if (CommonConstants.UPDATE.equals(action)) {
			personsJSONObject.put(MetaDataAPIAbacus.PERSON_ID.fieldName(),
					customerDTO.getPersonIdExtern());
		}

		personsJSONObject.put(MetaDataAPIAbacus.TITLE.fieldName(), 1);
		// Male = 0
		// Female = 1
		personsJSONObject.put(MetaDataAPIAbacus.GENDER.fieldName(),
				customerDTO.getGender() != null && customerDTO.getGender().equals("M") ? 0 : 1);
		personsJSONObject.put(MetaDataAPIAbacus.FORENAME_PART_ONE.fieldName(),
				customerDTO.getFirstName() != null ? customerDTO.getFirstName() : "");

		personsJSONObject.put(MetaDataAPIAbacus.FORENAME_PART_TWO.fieldName(),
				customerDTO.getSecondName() != null ? customerDTO.getSecondName() : "");

		personsJSONObject.put(MetaDataAPIAbacus.FORENAME_PART_THREE.fieldName(),
				customerDTO.getMiddleName() != null ? customerDTO.getMiddleName() : "");

		personsJSONObject.put(MetaDataAPIAbacus.SUR_NAME.fieldName(),
				customerDTO.getLastName() != null ? customerDTO.getLastName() : "");

		personsJSONObject.put(MetaDataAPIAbacus.SOCIAL_SECURITY_NUMBER.fieldName(), "");
		personsJSONObject.put(MetaDataAPIAbacus.DRIVING_LICENCE_NUMBER.fieldName(), "");
		personsJSONObject.put(MetaDataAPIAbacus.DATE_OF_BIRTH.fieldName(),
				customerDTO.getDateOfBirth() != null
						? java.sql.Date.valueOf(
								DateUtil.convertToLocalDateViaInstant(customerDTO.getDateOfBirth()))
						: "");
		personsJSONObject.put(MetaDataAPIAbacus.TELEPHONE_1.fieldName(),
				customerDTO.getTelephone1() != null ? customerDTO.getTelephone1() : "");
		personsJSONObject.put(MetaDataAPIAbacus.TELEPHONE_2.fieldName(),
				customerDTO.getTelephone2() != null ? customerDTO.getTelephone2() : "");
		personsJSONObject.put(MetaDataAPIAbacus.TELEPHONE_3.fieldName(),
				customerDTO.getTelephone3() != null ? customerDTO.getTelephone3() : "");
		personsJSONObject.put(MetaDataAPIAbacus.EMAIL.fieldName(),
				customerDTO.getEmail() != null ? customerDTO.getEmail() : "");
		personsJSONObject.put(MetaDataAPIAbacus.EMPLOYERS.fieldName(), new JSONArray());
		personsJSONObject.put(MetaDataAPIAbacus.FINGER_PRINTS.fieldName(), new JSONArray());

		arrayPersonne.put(personsJSONObject);
		return arrayPersonne;
	}

	/**
	 * Builds the persons for organisation json object.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the JSON array
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private JSONArray buildPersonsForOrganisationJsonObject(CustomerDTO customerDTO)
			throws ApiAbacusException, IOException {

		// build json schema
		JSONArray arrayPersonne = new JSONArray();
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerLinksRelationshipDTOs())) {
			for (CustomerLinksRelationshipDTO customerLinksRelationshipDTO : customerDTO
					.getCustomerLinksRelationshipDTOs()) {
				if (customerLinksRelationshipDTO.getCategory() != null
						&& customerLinksRelationshipDTO.getCategory()
								.equals(LinkRelationshipsCategory.MEMBERS.name())) {
					JSONObject personsJSONObject = new JSONObject();
					// create user ABACUS if not exist
					if (customerLinksRelationshipDTO.getMember().getPersonIdExtern() == 0) {
						// setting missing data
						if (ACMValidationUtils.isNullOrEmpty(
								customerLinksRelationshipDTO.getMember().getListAddress())) {
							customerLinksRelationshipDTO.getMember()
									.setListAddress(customerDTO.getListAddress());
						}
						customerLinksRelationshipDTO.getMember()
								.setCustomerType(CustomerType.INDIV.name());
						// save new customer in ABACUS DB
						CustomerDTO customerABACUS = save(customerLinksRelationshipDTO.getMember());
						personsJSONObject.put(MetaDataAPIAbacus.PERSON_ID.fieldName(),
								customerABACUS.getPersonIdExtern());
					}
					else {
						personsJSONObject.put(MetaDataAPIAbacus.PERSON_ID.fieldName(),
								customerLinksRelationshipDTO.getMember().getPersonIdExtern());
					}
					personsJSONObject.put(MetaDataAPIAbacus.FORENAME_PART_ONE.fieldName(),
							customerLinksRelationshipDTO.getMember().getFirstName() != null
									? customerLinksRelationshipDTO.getMember().getFirstName()
									: "");
					personsJSONObject.put(MetaDataAPIAbacus.FORENAME_PART_TWO.fieldName(),
							customerLinksRelationshipDTO.getMember().getSecondName() != null
									? customerLinksRelationshipDTO.getMember().getSecondName()
									: "");
					personsJSONObject.put(MetaDataAPIAbacus.FORENAME_PART_THREE.fieldName(),
							customerLinksRelationshipDTO.getMember().getMiddleName() != null
									? customerLinksRelationshipDTO.getMember().getMiddleName()
									: "");
					personsJSONObject.put(MetaDataAPIAbacus.SUR_NAME.fieldName(),
							customerLinksRelationshipDTO.getMember().getLastName() != null
									? customerLinksRelationshipDTO.getMember().getLastName()
									: "");
					personsJSONObject.put(MetaDataAPIAbacus.DATE_OF_BIRTH.fieldName(),
							customerLinksRelationshipDTO.getMember().getDateOfBirth() != null
									? java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
											customerLinksRelationshipDTO.getMember()
													.getDateOfBirth()))
									: "");
					arrayPersonne.put(personsJSONObject);
				}
			}
		}
		return arrayPersonne;
	}

	/**
	 * Builds the relationships json object.
	 * 
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @return the JSON array
	 */
	private JSONArray buildRelationshipsJsonObject(CustomerDTO customerDTO) {

		// build json schema
		JSONArray arrayRelationships = new JSONArray();
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerLinksRelationshipDTOs())) {
			for (CustomerLinksRelationshipDTO customerLinksRelationshipDTO : customerDTO
					.getCustomerLinksRelationshipDTOs()) {
				if (customerLinksRelationshipDTO.getCategory() != null
						&& customerLinksRelationshipDTO.getCategory()
								.equals(LinkRelationshipsCategory.RELATIONSHIP.name())) {
					JSONObject relationshipJSONObject = new JSONObject();
					// load & filter list of relationships
					RelationshipDTO relationshipDTO = parametrageClient.findRelationship().stream()
							.filter(relationShip -> customerLinksRelationshipDTO
									.getLinkRelationshipType().equals(relationShip.getName()))
							.findFirst().orElse(null);
					// setting Relationship ID else default RelationshipID = 7 (RELATIVES-قرابة)
					relationshipJSONObject.put(MetaDataAPIAbacus.RELATIONSHIP_ID.fieldName(),
							relationshipDTO != null && !ACMValidationUtils
									.isNullOrEmpty(relationshipDTO.getRelationshipID())
											? relationshipDTO.getRelationshipID()
											: 7);
					relationshipJSONObject.put(MetaDataAPIAbacus.CHILD_CUSTOMER_ID.fieldName(),
							customerLinksRelationshipDTO.getMember().getCustomerIdExtern());
					relationshipJSONObject.put(MetaDataAPIAbacus.RELATIONSHIP_LIST_KEY.fieldName(),
							JSONObject.NULL);
					relationshipJSONObject.put(MetaDataAPIAbacus.NAME.fieldName(), JSONObject.NULL);
					relationshipJSONObject.put(MetaDataAPIAbacus.DIRECTIONAL.fieldName(),
							JSONObject.NULL);
					relationshipJSONObject.put(MetaDataAPIAbacus.ACTIVE.fieldName(),
							JSONObject.NULL);
					relationshipJSONObject.put(MetaDataAPIAbacus.REVERSED.fieldName(),
							Boolean.FALSE);
					arrayRelationships.put(relationshipJSONObject);
				}
			}
		}
		return arrayRelationships;
	}

	/**
	 * Gets the data.
	 *
	 * @param idCustomer the id customer
	 * @return the data
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.CustomerAbacusApiService#getData(java.lang.Long)
	 */
	@Override
	public CustomerAbacusAPIModel getData(Long idCustomer)
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
			logger.info("idCustomer = {}", idCustomer);
			logger.info("*************** GET DATA API ******************");

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			String accessUrl = urlServeurApi + uriGetCustomerData + idCustomer;
			// sending request to server using POST method
			URI uri = new URI(accessUrl);
			logger.info("uri GET DATA  = {}", uri);
			ResponseEntity<CustomerAbacusAPIModel> responseAPI = restTemplate.exchange(uri,
					HttpMethod.GET, request, CustomerAbacusAPIModel.class);
			// check if response is not NULL
			if (responseAPI.getBody() != null) {
				logger.info("responseAPI.getBody = {}", responseAPI.getBody());
				return responseAPI.getBody();
			}
		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" get customer RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" get customer ResponseBodyAsString = {}", e.getResponseBodyAsString());
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
			logger.error("Error has been occured while consuming add customer API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return null;
	}

	/**
	 * Send tranverse historique.
	 * 
	 * @author hchaouachi
	 * @param objectValue the object value
	 * @param methode the methode
	 * @param uri the uri
	 * @param status the status
	 * @param requestValue the request value
	 * @param responseValue the response value
	 */
	void sendTranverseHistorique(String objectValue, String methode, String uri, String status,
			String requestValue, String responseValue) {

		TransversHistoriqueDTO transversHistoriqueDTO = new TransversHistoriqueDTO(objectValue,
				methode, uri, status, requestValue, requestValue);
		logger.info("transversHistorique add Customer = {}", transversHistoriqueDTO.toString());
		// add history of request and response api create customer in transvers historique
		// table
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO)) {
			creditClient.create(transversHistoriqueDTO);
		}

	}

	/**
	 * Find account schedule by customer id.
	 *
	 * @param idCustomerExtern the id customer extern
	 * @param accountNumberExtern the account number extern
	 * @return the list
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ParseException the parse exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.api_abacus.service.CustomerAbacusApiService#getData(java.lang.Long)
	 */
	@Override
	public List<ScheduleDTO> findAccountScheduleByCustomerId(Long idCustomerExtern,
			String accountNumberExtern)
			throws IOException, ApiAbacusException, URISyntaxException, ParseException {

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
			logger.info("idCustomerExtern = {}", idCustomerExtern);
			logger.info("accountNumberExtern = {}", accountNumberExtern);

			// init request
			HttpEntity<String> request = new HttpEntity<>(headers);
			String accessUrl = urlServeurApi + uriGetAccountAllocationData + idCustomerExtern;
			// sending request to server using POST method
			logger.info("uri GET DATA  = {}", accessUrl);
			ResponseEntity<List<CustomerAbacusAPIModelPaymentAllocation>> responseAPI =
					restTemplate.exchange(accessUrl, HttpMethod.GET, request,
							new ParameterizedTypeReference<List<CustomerAbacusAPIModelPaymentAllocation>>() {

							});
			// check if response is not NULL
			List<ScheduleDTO> scheduleDTOs = new ArrayList<>();
			if (responseAPI.getBody() != null) {
				List<CustomerAbacusAPIModelPaymentAllocation> customerAbacusAPIModelPaymentAllocation =
						responseAPI.getBody().stream()
								.filter(responseValue -> (responseValue.getAccountNumber())
										.equals(accountNumberExtern))
								.collect(Collectors.toList());
				if (!ACMValidationUtils.isNullOrEmpty(customerAbacusAPIModelPaymentAllocation)) {
					logger.info("responseAPI.getBody = {}",
							customerAbacusAPIModelPaymentAllocation.get(0).getLoanSchedule());
					SimpleDateFormat parseDate = new SimpleDateFormat("dd/MM/yyyy");
					for (CustomerAbacusAPIModelLoanSchedule loanScheduleDTO : customerAbacusAPIModelPaymentAllocation
							.get(0).getLoanSchedule()) {
						ScheduleDTO schedule = new ScheduleDTO();
						schedule.setBalance(loanScheduleDTO.getBalance());
						schedule.setPeriod(loanScheduleDTO.getPeriod());
						schedule.setTotalRepayment(loanScheduleDTO.getTotalRepayment());
						schedule.setInterestRepayment(loanScheduleDTO.getInterestRepayment());
						schedule.setLoanRepayment(loanScheduleDTO.getLoanRepayment());
						schedule.setBalance(loanScheduleDTO.getBalance());
						schedule.setInterestAmtPaid(loanScheduleDTO.getInterestAmtPaid());
						schedule.setLoanRepaymentPaid(loanScheduleDTO.getLoanRepaymentPaid());
						schedule.setStatusLabel(loanScheduleDTO.getStatus());
						if (loanScheduleDTO.getStatus().equals("Fully Paid")) {
							schedule.setStatus(new BigDecimal(1));
						}
 
						else if (loanScheduleDTO.getStatus().equals("Partially Paid")) {
							schedule.setStatus(new BigDecimal(2));
						}
						else if (loanScheduleDTO.getStatus().equals("Unpaid")) {
							schedule.setStatus(new BigDecimal(3));
						}
						if (!ACMValidationUtils.isNullOrEmpty(loanScheduleDTO.getRepaymentDate())) {
							schedule.setRepaymentDate(
									parseDate.parse(loanScheduleDTO.getRepaymentDate()));
						}
						if (!ACMValidationUtils.isNullOrEmpty(loanScheduleDTO.getRepaidDate())) {
							schedule.setRepaidOn(parseDate.parse(loanScheduleDTO.getRepaidDate()));
						}
						schedule.setPenalityDue(loanScheduleDTO.getPenaltyDue());
						schedule.setPenalityPaid(loanScheduleDTO.getPenaltyDuePaid());
						schedule.setAmountWrittenOff(loanScheduleDTO.getWrittenOffAmount());
						schedule.setLateDays(loanScheduleDTO.getLateDays());
						scheduleDTOs.add(schedule);
					}
					return scheduleDTOs;
				}

			}

		}
		catch (RestClientResponseException e) {
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			ResponseEntity.status(e.getRawStatusCode()).body(e.getResponseBodyAsString());
			logger.error(" get customer RawStatusCode =  {}", e.getRawStatusCode());
			logger.error(" get customer ResponseBodyAsString = {}", e.getResponseBodyAsString());
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
		catch (KeyManagementException | KeyStoreException | NoSuchAlgorithmException e) {
			logger.error("Error has been occured while consuming add customer API from ABACUS. {}",
					e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.API_ABACUS_FAILED, new TechnicalException()),
					CommonExceptionsMessage.API_ABACUS_FAILED);
		}
		return null;
	}
}
