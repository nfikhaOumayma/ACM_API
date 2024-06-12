/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.EnumUtils;
import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CheckFieldsConfigurationException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.MezaCardExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CustomerRepository;
import com.acm.service.AddressService;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.service.CustomerService;
import com.acm.service.LoanService;
import com.acm.service.MezaCardService;
import com.acm.service.SupplierService;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.service.api_ib.LoadDataIBService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmValidatorDTO;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerDetailsReportsDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.CustomerMezaCardStatutDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SupplierDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsModelDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.CustomerPaginationDTO;
import com.acm.utils.dtos.pagination.SupplierPaginationDTO;
import com.acm.utils.enums.CustomerMezaCardStatus;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.MezaCardStatus;
import com.acm.utils.enums.SettingIHMValidatorCode;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.Customer;
import com.acm.utils.models.QCustomer;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.StringTemplate;

import feign.FeignException;

/**
 * {@link CustomerServiceImpl} CustomerServiceImpl.
 *
 * @author YesserSOmai
 * @since 0.2.0
 */
@Service
public class CustomerServiceImpl implements CustomerService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerServiceImpl.class);

	/** The customer repository. */
	@Autowired
	private CustomerRepository customerRepository;

	/** The customer links relationship service. */
	@Autowired
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/** The address service. */
	@Autowired
	private AddressService addressService;

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The supplier service. */
	@Autowired
	private SupplierService supplierService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The meza cardservice. */
	@Autowired
	private MezaCardService mezaCardservice;

	/** The load data IB service. */
	@Autowired
	private LoadDataIBService loadDataIBService;

	/**
	 * Find.
	 *
	 * @param id the id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#find(java.lang.Long)
	 */
	@Override
	public CustomerDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Customer by ID : {}", id);
		Customer customer = customerRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(customer)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Customer.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		CustomerDTO customerDTO = mapper.map(customer, CustomerDTO.class);

		// find list ADDRESS
		AddressDTO paramsAddress = new AddressDTO();
		paramsAddress.setCustomerId(id);
		List<AddressDTO> addressDTOs = addressService.find(paramsAddress);
		customerDTO.setListAddress(addressDTOs);

		// find UDF by customer ID
		UserDefinedFieldsLinksDTO paramsUDFLinksDTO = new UserDefinedFieldsLinksDTO();
		paramsUDFLinksDTO.setCustomerId(id);
		List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs =
				userDefinedFieldsLinksService.find(paramsUDFLinksDTO);
		customerDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksDTOs);
		// Set Meza Card Details
		if (!ACMValidationUtils.isNullOrEmpty(customer.getAcmMezaCards())) {
			customerDTO.setAcmMezaCardDTO(
					mapper.map(customer.getAcmMezaCards().iterator().next(), AcmMezaCardDTO.class));
		}
		else {
			customerDTO.setAcmMezaCardDTO(new AcmMezaCardDTO());
		}
		return customerDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findForClaims(java.lang.Long)
	 */
	@Override
	public CustomerDTO findForClaims(Long id) {

		try {
			logger.info("Find Customer by ID : {}", id);
			List<Customer> customer = customerRepository.findByIbCustomerId(id);
			// check if object is null
			CustomerDTO customerDTO = mapper.map(customer.get(0), CustomerDTO.class);
			return customerDTO;

		}
		catch (Exception e) {
			return new CustomerDTO();
		}
	}

	/**
	 * Find customer.
	 *
	 * @param id the id
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomer(java.lang.Long)
	 */
	@Override
	public CustomerDTO findCustomer(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Customer by ID : {}", id);
		Customer customer = customerRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(customer)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Customer.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(customer, CustomerDTO.class);
	}

	/**
	 * Find customer by ib customer id.
	 *
	 * @param ibCustomerId the ib customer id
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Override
	public List<CustomerDTO> findCustomerByIbCustomerId(Long ibCustomerId)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(ibCustomerId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Customer by ibCustomerId : {}", ibCustomerId);
		List<Customer> customers = customerRepository.findByIbCustomerId(ibCustomerId);
		List<CustomerDTO> customersDTOs = new ArrayList<>();

		// check if object is null
		if (!ACMValidationUtils.isNullOrEmpty(customers)) {
			customers.forEach(
					customer -> customersDTOs.add(mapper.map(customer, CustomerDTO.class)));
		}

		return customersDTOs;
	}

	/**
	 * Find customer id extern.
	 *
	 * @param customerIdExtern the customer id extern
	 * @param getAllData the get all data
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomerIdExtern(java.lang.Long)
	 */
	@Override
	public List<CustomerDTO> findCustomerIdExtern(Long customerIdExtern,
			Optional<Boolean> getAllData) {

		Preconditions.checkNotNull(customerIdExtern, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Customer by CUSTOMER_ID_EXTERN : {}", customerIdExtern);
		List<Customer> customers = customerRepository.findByCustomerIdExtern(customerIdExtern);
		logger.info("Method = findCustomerIdExtern() :{} : Customers was founded",
				customers.size());
		// check if returned list is not EMPTY
		if (ACMValidationUtils.isNullOrEmpty(customers)) {
			return new ArrayList<>();
		}
		List<CustomerDTO> customersDTOs = new ArrayList<>();
		customers.forEach(customer -> {

			CustomerDTO customerDTO = mapper.map(customer, CustomerDTO.class);
			if (!ACMValidationUtils.isNullOrEmpty(getAllData) && Boolean.TRUE.equals(getAllData)) {
				// find list ADDRESS
				AddressDTO paramsAddress = new AddressDTO();
				paramsAddress.setCustomerId(customer.getId());
				List<AddressDTO> addressDTOs = addressService.find(paramsAddress);
				customerDTO.setListAddress(addressDTOs);

				// find UDF by customer ID
				UserDefinedFieldsLinksDTO paramsUDFLinksDTO = new UserDefinedFieldsLinksDTO();
				paramsUDFLinksDTO.setCustomerId(customer.getId());
				List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs =
						userDefinedFieldsLinksService.find(paramsUDFLinksDTO);
				customerDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksDTOs);
			}
			customersDTOs.add(customerDTO);
		});

		return customersDTOs;
	}

	/**
	 * Find.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#find(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public List<CustomerDTO> find(CustomerDTO customerDTO) {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QCustomer
		QCustomer qCustomer = QCustomer.customer;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCustomer.enabled.eq(Boolean.TRUE));

		// find by CustomerIdExtern
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerIdExtern())) {
			predicate.and(qCustomer.customerIdExtern.eq(customerDTO.getCustomerIdExtern()));
		}
		// find by ibCustomerId
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIbCustomerId())) {
			predicate.and(qCustomer.ibCustomerId.eq(customerDTO.getIbCustomerId()));
		}

		// find LIKE CustomerNumber
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerNumber())
				&& (ACMValidationUtils.isNullOrEmpty(customerDTO.getSearchFormDocument())
						|| Boolean.FALSE.equals(customerDTO.getSearchFormDocument()))) {
			predicate.and(
					qCustomer.customerNumber.like("%" + customerDTO.getCustomerNumber() + "%"));
		}
		// find EQUALS CustomerNumber for document search
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerNumber())
				&& Boolean.TRUE.equals(customerDTO.getSearchFormDocument())) {
			predicate.and(qCustomer.customerNumber.eq(customerDTO.getCustomerNumber()));
		}

		// find by Identity
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIdentity())
				&& ACMValidationUtils.isNullOrEmpty(customerDTO.getRegisterNumber())) {
			predicate.and(qCustomer.identity.eq(customerDTO.getIdentity()));
		}

		// find by registerNumber
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getRegisterNumber())
				&& ACMValidationUtils.isNullOrEmpty(customerDTO.getIdentity())) {
			predicate.and(qCustomer.registerNumber.eq(customerDTO.getRegisterNumber()));
		}

		// find by Identity or registerNumber
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIdentity())
				&& !ACMValidationUtils.isNullOrEmpty(customerDTO.getRegisterNumber())) {
			BooleanBuilder subPredicate = new BooleanBuilder();
			subPredicate.and(qCustomer.registerNumber.eq(customerDTO.getRegisterNumber()));
			subPredicate.or(qCustomer.identity.eq(customerDTO.getIdentity()));
			predicate.and(subPredicate);
		}

		// find by OrganisationId
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getOrganisationId())) {
			predicate.and(qCustomer.organisationId.eq(customerDTO.getOrganisationId()));
		}
		// find by telephone
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getTelephone1())) {
			predicate.and(qCustomer.telephone1.eq(customerDTO.getTelephone1()));
		}

		// find by CustomerType
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerType())) {
			predicate.and(qCustomer.customerType.eq(customerDTO.getCustomerType()));
		}

		// find by CustomerCategory
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIsCustomer())) {
			predicate.and(qCustomer.isCustomer.eq(customerDTO.getIsCustomer()));
		}

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.OPERATION.name())) {
			// find all user COLLABORATOR
			List<Long> wheresAccountPortfolioID = new ArrayList<>();
			List<UserDTO> userDTOs = userClient.findUsers();
			userDTOs.forEach(user -> {
				if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
					wheresAccountPortfolioID.add(user.getAccountPortfolioId());
				}
			});
			// setting predicate to find by accountPortfolioID
			predicate.and(qCustomer.accountPortfolioID
					.in(new ArrayList<>(new HashSet<>(wheresAccountPortfolioID))));
		}
		// find loan by Access Branches for connected user
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// setting predicate to find by given branch Id
			predicate.and(qCustomer.branchId.in(parseAccessUserBranch(userDTO)));
		}
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// find by given branch Id
			predicate.and(qCustomer.branchId.eq(userDTO.getBranchID()));
		}
		// find by AltName
		else if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getAltName())) {
			predicate.and(qCustomer.altName.eq(customerDTO.getAltName()));
		}
		else {
			// return Empty list
			return new ArrayList<>();
		}

		Iterable<Customer> iterable = customerRepository.findAll(predicate);
		List<Customer> customers = new ArrayList<>();
		iterable.forEach(customers::add);
		logger.info("Method = find() :{} : Customer was founded", customers.size());

		List<CustomerDTO> customersDTOs = new ArrayList<>();
		customers.forEach(customer -> customersDTOs.add(mapper.map(customer, CustomerDTO.class)));
		return customersDTOs;
	}

	/**
	 * Parses the access user branch && returning list of {@link Integer} branchIDs.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	private List<Integer> parseAccessUserBranch(UserDTO userDTO) {

		int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(",")).stream()
				.map(String::trim).mapToInt(Integer::parseInt).toArray();
		List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length + 1);
		for (int i : arrayBranchIds) {
			listBranchIds.add(Integer.valueOf(i));
		}
		// add branch ID in returned List
		listBranchIds.add(userDTO.getBranchID());
		return listBranchIds;
	}

	/**
	 * Save.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws CalculateAgeException the calculate age exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#save(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public CustomerDTO save(CustomerDTO customerDTO) throws CalculateAgeException {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Customer customer = mapper.map(customerDTO, Customer.class);
		// enable the customer
		customer.setIsCustomer(Boolean.TRUE);
		customer.setUpdateCustomer(Boolean.FALSE);

		// Processing customer
		if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
			// setting SolidarityName field if type = Group
			customer.setSolidarityName(customerDTO.getCustomerName());
		}
		else if (customerDTO.getCustomerType().equals(CustomerType.ORG.name())) {
			// setting OrganizationName field if type = organisation
			customer.setOrganizationName(customerDTO.getCustomerName());
		}
		// setting MEZA-CARD status
		if (ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardStatus()) && !EnumUtils
				.isValidEnum(CustomerMezaCardStatus.class, customerDTO.getMezaCardStatus())) {
			customer.setMezaCardStatus(CustomerMezaCardStatus.NONE.name());
		}
		// check OpenDate
		if (ACMValidationUtils.isNullOrEmpty(customer.getCustomerOpenDate())) {
			customer.setCustomerOpenDate(new Date());
		}
		CommonFunctions.mapperToSave(customer, userClient, logger);
		Customer newCustomer = customerRepository.save(customer);
		// check if customer is prospect
		if (newCustomer.getCustomerType().equals(CommonConstants.PROSPECT_CATEGORY)) {
			// save udf
			try {
				userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
						customerDTO.getUserDefinedFieldsLinksDTOs(), newCustomer.getId(), null);
			}
			catch (ResourcesNotFoundException e) {
				logger.error(e.getMessage());
			}
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Customer.class.getSimpleName());
		return mapper.map(newCustomer, CustomerDTO.class);
	}

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException Save. @param id the id @param customerDTO the customer DTO @return
	 *         the customer DTO @throws ResourcesNotFoundException the resources not found
	 *         exception @throws CalculateAgeException the calculate age exception @throws
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#save(java.lang.Long, com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public CustomerDTO save(Long id, CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Customer with ID = {}", id);
		Customer oldCustomer = customerRepository.findById(id).orElse(null);

		// check if object is null
		if (oldCustomer == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Customer.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// calculate AGE if not exist
		if (customerDTO.getCustomerType().equals(CustomerType.INDIV.name())) {
			Integer age = DateUtil.calculateAge(customerDTO.getDateOfBirth());
			if (age == -1) {
				logger.error(CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH);
				throw new CalculateAgeException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_INVALID_DATE_BIRTH,
								CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH),
						CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH);
			}
		}
		// setting customer
		if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
			// setting SolidarityName field if type = Group
			oldCustomer.setSolidarityName(customerDTO.getCustomerName());
		}
		else if (customerDTO.getCustomerType().equals(CustomerType.ORG.name())) {
			// setting OrganizationName field if type = organisation
			oldCustomer.setOrganizationName(customerDTO.getCustomerName());
		}
		// setting MEZA-CARD status
		if ((ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardStatus()) && !EnumUtils
				.isValidEnum(CustomerMezaCardStatus.class, customerDTO.getMezaCardStatus()))
				|| (!ACMValidationUtils.isNullOrEmpty(customerDTO.getDisbursementMethodSelected())
						&& !customerDTO.getDisbursementMethodSelected()
								.equals(CommonConstants.UDF_MEZA_CARD_INTERNAL))) {
			if ((oldCustomer.getMezaCardStatus() != null) && (!oldCustomer.getMezaCardStatus()
					.equals(CustomerMezaCardStatus.UNTRUSTED.name()))) {
				customerDTO.setMezaCardStatus(CustomerMezaCardStatus.NONE.name());
			}
		}
		// mapping new data with existing data (oldCustomer)
		mapper.map(customerDTO, oldCustomer);
		CommonFunctions.mapperToUpdate(oldCustomer, userClient, logger);

		// update & persist data in DB
		Customer newCustomer = new Customer();
		try {
			newCustomer = customerRepository.save(oldCustomer);
		}
		catch (DataIntegrityViolationException e) {
			throw new CreditException(new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_EXIST,
					CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY, new TechnicalException()),
					CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Customer.class.getSimpleName());
		return mapper.map(newCustomer, CustomerDTO.class);
	}

	/**
	 * Find customers.
	 *
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomers()
	 */
	@Override
	public List<CustomerDTO> findCustomers() {

		// init QCustomer
		QCustomer qCustomer = QCustomer.customer;
		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCustomer.enabled.eq(Boolean.TRUE));

		// find customer by Account Portfolio Id
		predicate.and(qCustomer.accountPortfolioID.eq(userDTO.getAccountPortfolioId()));

		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.OPERATION.name())) {
			// find all user COLLABORATOR
			List<Long> wheresAccountPortfolioID = new ArrayList<>();
			List<UserDTO> userDTOs = userClient.findUsers();
			userDTOs.forEach(user -> {
				if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
					wheresAccountPortfolioID.add(user.getAccountPortfolioId());
				}
			});
			// setting predicate to find by accountPortfolioID
			predicate.or(qCustomer.accountPortfolioID
					.in(new ArrayList<>(new HashSet<>(wheresAccountPortfolioID))));
		}
		// find loan by Access Branches for connected user
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// setting predicate to find by given branch Id
			predicate.or(qCustomer.branchId.in(parseAccessUserBranch(userDTO)));
		}
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// find by given branch Id
			predicate.or(qCustomer.branchId.eq(userDTO.getBranchID()));
		}

		// find data
		Iterable<Customer> iterableCustomer = customerRepository.findAll(predicate);
		List<Customer> customers = new ArrayList<>();
		iterableCustomer.forEach(customers::add);

		// mapping && returning data
		List<CustomerDTO> customerDTOs = new ArrayList<>();
		customers.forEach(customer -> customerDTOs.add(mapper.map(customer, CustomerDTO.class)));
		logger.info("Method = findCustomers() : {} : customer was founded", customerDTOs.size());
		return customerDTOs;
	}

	/**
	 * Find.
	 *
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#find(com.acm.utils.dtos.pagination.
	 * CustomerPaginationDTO)
	 */
	@Override
	public CustomerPaginationDTO find(CustomerPaginationDTO customerPaginationDTO) {

		Preconditions.checkNotNull(customerPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getPageNumber())) {
			customerPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getPageSize())) {
			customerPaginationDTO.setPageSize(10);
		}
		// setting default data
		customerPaginationDTO.setResultsCustomers(new ArrayList<>());
		// setting default totals pages
		customerPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		customerPaginationDTO.setTotalPages(0);
		// init QCustomer
		QCustomer qCustomer = QCustomer.customer;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCustomer.enabled.eq(Boolean.TRUE));

		// find LIKE CustomerNumber
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerNumber())) {
			predicate.and(qCustomer.customerNumber
					.like("%" + customerPaginationDTO.getParams().getCustomerNumber() + "%"));
		}

		// find LIKE customerName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerName())) {
			StringTemplate convertedCustomerName = Expressions
					.stringTemplate("function('replace', {0}, '|', ' ')", qCustomer.customerName);
			predicate.and(convertedCustomerName
					.like("%" + customerPaginationDTO.getParams().getCustomerName() + "%"));
		}

		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getParams().getIdentity())) {
			predicate.and(qCustomer.identity
					.like("%" + customerPaginationDTO.getParams().getIdentity() + "%"));
		}

		// find LIKE solidarityName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getSolidarityName())) {
			// search solidarityName only for INDIV customerType
			predicate.and(qCustomer.customerType.eq(CustomerType.INDIV.name()));
			predicate.and(qCustomer.solidarityName
					.like("%" + customerPaginationDTO.getParams().getSolidarityName() + "%"));
		}

		// find LIKE branchesName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getBranchesName())) {
			predicate.and(qCustomer.branchesName
					.like("%" + customerPaginationDTO.getParams().getBranchesName() + "%"));
		}

		// find LIKE accountPortfolioDescription
		if (!ACMValidationUtils.isNullOrEmpty(
				customerPaginationDTO.getParams().getAccountPortfolioDescription())) {
			predicate.and(qCustomer.accountPortfolioDescription.like("%"
					+ customerPaginationDTO.getParams().getAccountPortfolioDescription() + "%"));
		}

		// find by CustomerType
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerType())) {
			predicate.and(
					qCustomer.customerType.eq(customerPaginationDTO.getParams().getCustomerType()));
		}

		// find by AltName
		if (!ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getParams().getAltName())) {
			predicate.and(qCustomer.altName.eq(customerPaginationDTO.getParams().getAltName()));
		}

		// find by supplierRecommandation
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getSupplierRecommandation())) {
			predicate.and(qCustomer.supplierRecommandation
					.eq(customerPaginationDTO.getParams().getSupplierRecommandation()));
		}
		// find by telephone
		if (!ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getParams().getTelephone1())) {
			predicate.and(
					qCustomer.telephone1.eq(customerPaginationDTO.getParams().getTelephone1()));
		}
		// find by date insertion
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getDateInsertion())) {
			Timestamp StartTimesTamp = DateUtil.dateToDateTime(
					customerPaginationDTO.getParams().getDateInsertion(), "00:00:01");
			Timestamp EndTimesTamp = DateUtil.dateToDateTime(
					customerPaginationDTO.getParams().getDateInsertion(), "23:59:59");
			predicate.and(qCustomer.dateInsertion.between(StartTimesTamp, EndTimesTamp));
		}

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.OPERATION.name())) {
			// find all user COLLABORATOR
			List<Long> wheresAccountPortfolioID = new ArrayList<>();
			List<UserDTO> userDTOs = userClient.findUsers();
			userDTOs.forEach(user -> {
				if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
					wheresAccountPortfolioID.add(user.getAccountPortfolioId());
				}
			});
			// setting predicate to find by accountPortfolioID
			predicate.and(qCustomer.accountPortfolioID
					.in(new ArrayList<>(new HashSet<>(wheresAccountPortfolioID))));
		}
		// find loan by Access Branches for connected user
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// setting predicate to find by given branch Id
			predicate.and(qCustomer.branchId.in(parseAccessUserBranch(userDTO)));
		}
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// find by given branch Id
			predicate.and(qCustomer.branchId.eq(userDTO.getBranchID()));
		}
		else {
			// return Empty list
			return customerPaginationDTO;
		}

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(customerPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = customerPaginationDTO.getSortField();
			if (customerPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(customerPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = customerPaginationDTO.getSortField();
			if (customerPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by dateInsertion : DESC
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}

		// load data
		Page<Customer> pagedResult = customerRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Customer> customers = pagedResult.getContent();
			List<CustomerDTO> customerDTOs = new ArrayList<>();
			// mapping data
			customers.forEach(customer -> {
				CustomerDTO dto = mapper.map(customer, CustomerDTO.class);
				// setting assigned MEZACARD data
				if (!ACMValidationUtils.isNullOrEmpty(customer.getAcmMezaCards())) {
					dto.setAcmMezaCardDTO(mapper.map(customer.getAcmMezaCards().iterator().next(),
							AcmMezaCardDTO.class));
				}
				else {
					dto.setAcmMezaCardDTO(new AcmMezaCardDTO());
				}
				customerDTOs.add(dto);
			});

			logger.info("{} : Customer was founded (PageNumber = {} / PageSize = {} )",
					customers.size(), customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize());
			// setting data
			customerPaginationDTO.setResultsCustomers(customerDTOs);
			// setting totals pages
			customerPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			customerPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return customerPaginationDTO;
	}

	/**
	 * Find customer account.
	 *
	 * @param idCustomer the id customer
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomerAccount(java.lang.Long)
	 */
	@Override
	public List<CustomerAccountDTO> findCustomerAccount(Long idCustomer) {

		List<CustomerAccountDTO> listCustomerAccounts = new ArrayList<>();
		// find accounts by customer id
		listCustomerAccounts = transversClient.findCustomerAccountByCustomer(idCustomer);
		// get list of products id from customer accounts
		List<Long> productsIds =
				listCustomerAccounts.stream().map(CustomerAccountDTO::getProductIdAbacus).distinct()
						.collect(Collectors.toList());
		// get setting of topup / refinance by products id
		List<ProductDTO> products = parametrageClient.findByIds(productsIds);
		for (ProductDTO product : products) {
			listCustomerAccounts.stream().filter(
					account -> (account.getProductIdAbacus()).equals(product.getProductIdAbacus()))
					.forEach(a -> {
						a.setTopupProduct(product.getTopup());
						a.setRefinanceProduct(product.getRefinance());
					});
		}
		return listCustomerAccounts;
	}

	/**
	 * Save for application.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws CreditException the credit exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws CheckFieldsConfigurationException the check fields configuration exception
	 * @throws MezaCardExistException the meza card exist exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#saveForApplication(com.acm.utils.dtos. CustomerDTO)
	 */
	@Override
	public CustomerDTO saveForApplication(CustomerDTO customerDTO)
			throws CreditException, ResourcesNotFoundException, ApiAbacusException, IOException,
			CalculateAgeException, CustomerMaxActiveAccountException,
			CheckFieldsConfigurationException, MezaCardExistException {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// *** Check field configuration ***
		if (!Boolean.FALSE.equals(customerDTO.getCheckCustomer())) {
			checkFieldsConfiguration(customerDTO);
		}
		// processing customer by type indiv
		if (CustomerType.INDIV.name().equals(customerDTO.getCustomerType())) {
			// init customer name
			customerDTO
					.setCustomerName(customerDTO.getFirstName() + "|" + customerDTO.getSecondName()
							+ "|" + customerDTO.getMiddleName() + "|" + customerDTO.getLastName());
			// CHECK DOUBLE By Identity and check telephone number and calculate age
			if (ACMValidationUtils.isNullOrEmpty(customerDTO.getIbCustomerId())) {
				checkCustomer(customerDTO);
			}
		}

		SupplierDTO isSupplierDTO = checkIsSupplier(customerDTO);
		SupplierDTO isNotSupplierDTO = checkIsNotSupplier(customerDTO);

		if (!ACMValidationUtils.isNullOrEmpty(isSupplierDTO)) {
			customerDTO.setIsSupplier(Boolean.TRUE);
		}
		else {
			customerDTO.setIsSupplier(Boolean.FALSE);
		}

		// processing customer by type grp
		if (CustomerType.GRP.name().equals(customerDTO.getCustomerType())) {
			List<String> messagesError = new ArrayList<>();
			Boolean fireException = Boolean.FALSE;
			for (CustomerLinksRelationshipDTO customerLinksRelationshipDTOElement : customerDTO
					.getCustomerLinksRelationshipDTOs()) {
				CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
						new CustomerLinksRelationshipDTO();
				customerLinksRelationshipDTO.setCategory(LinkRelationshipsCategory.MEMBERS.name());
				customerLinksRelationshipDTO
						.setMember(customerLinksRelationshipDTOElement.getMember());
				if (!ACMValidationUtils.isNullOrEmpty(
						customerLinksRelationshipService.find(customerLinksRelationshipDTO))) {
					messagesError.add(customerLinksRelationshipDTOElement.getMember()
							.getCorrespondanceName());
					fireException = Boolean.TRUE;
				}
			}
			if (fireException.equals(Boolean.TRUE)) {
				throw new CustomerMaxActiveAccountException(
						new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
								CommonExceptionsMessage.CUSTOMER_MAX_ACTIVE_ACCOUNT + " : "
										+ messagesError),
						CommonExceptionsMessage.CUSTOMER_MAX_ACTIVE_ACCOUNT + " : "
								+ messagesError);
			}
		}

		// Check meza card Exist
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardId())) {
			checkMezaCardStatut(customerDTO, CommonConstants.ADD);
		}
		// Set Id EXTERN 0 for new customer from ACM
		customerDTO.setCustomerIdExtern(0L);
		// Set Id Person EXTERN 0 for new customer from ACM
		customerDTO.setPersonIdExtern(0L);
		// Set enable_critical_data 0
		customerDTO.setEnableCriticalData(Boolean.FALSE);
		CustomerDTO customerApi = null;

		try {
			// CALL API ABACUS TO SAVE CAUSTOMER
			customerApi = transversClient.addCustomer(customerDTO);
			logger.info("Customer Inserted to Abacus (customer API = {})", customerApi.toString());
		}
		catch (Exception e) {
			logger.error("Error Calling API (customer ERROR API = {})", e.getMessage());
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
		}

		CustomerDTO newCustomerDTO = null;
		try {
			// Save All data IN ACM
			customerApi.setIsSupplier(customerDTO.getIsSupplier());
			customerApi.setUserDefinedFieldsLinksDTOs(customerDTO.getUserDefinedFieldsLinksDTOs());
			customerApi.setBeneficialEffective(customerDTO.getBeneficialEffective());
			customerApi.setProspectionSource(customerDTO.getProspectionSource());
			customerApi.setProspectionComment(customerDTO.getProspectionComment());
			customerApi.setSupplierRecommandation(customerDTO.getSupplierRecommandation());
			newCustomerDTO = saveACMCustomer(customerApi);
		}
		catch (DataIntegrityViolationException e) {
			throw new CreditException(new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_EXIST,
					CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY, new TechnicalException()),
					CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
		}
		catch (Exception e) {
			logger.error("Error Insertion In ACM (customer ACM = {})", e.getMessage());
		}

		if (ACMValidationUtils.isNullOrEmpty(newCustomerDTO)) {
			newCustomerDTO = new CustomerDTO();
		}
		else {
			if (!ACMValidationUtils.isNullOrEmpty(isNotSupplierDTO)) {
				supplierService.save(isNotSupplierDTO);
			}

			if (!ACMValidationUtils.isNullOrEmpty(isSupplierDTO)) {
				supplierService.save(isSupplierDTO);
			}
		}

		return newCustomerDTO;
	}

	/**
	 * Check meza card statut.
	 *
	 * @author MOEZ
	 * @param customerDTO the customer DTO
	 * @param action the action
	 * @return the acm meza card DTO
	 * @throws MezaCardExistException the meza card exist exception
	 */
	private AcmMezaCardDTO checkMezaCardStatut(CustomerDTO customerDTO, String action)
			throws MezaCardExistException {

		logger.info("START CHECK MEZA CARD STATUT");
		AcmMezaCardDTO oldAcmMezaCardDTO = null;
		AcmMezaCardDTO newAcmMezaCardDTO = new AcmMezaCardDTO();
		try {
			// find MEZA card data by ID or by given customer
			oldAcmMezaCardDTO = mezaCardservice.find(customerDTO.getMezaCardId());
			// find in in use mezaCards (and not saved in dataBase)
			if (ACMValidationUtils.isNullOrEmpty(oldAcmMezaCardDTO)) {
				throw new MezaCardExistException(
						new ExceptionResponseMessage(CommonErrorCode.MEZA_CARD_EXIST,
								CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
								new TechnicalException()),
						CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
			}
			if (action.equals(CommonConstants.ADD)) {
				if (oldAcmMezaCardDTO.getStatus().equals(MezaCardStatus.ASSIGN.name())) {
					throw new MezaCardExistException(
							new ExceptionResponseMessage(CommonErrorCode.MEZA_CARD_EXIST,
									CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
									new TechnicalException()),
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
				}
			}
			else if (action.equals(CommonConstants.UPDATE)) {
				if (oldAcmMezaCardDTO.getStatus().equals(MezaCardStatus.ASSIGN.name())
						&& !oldAcmMezaCardDTO.getCustomerDTO().getId()
								.equals(customerDTO.getId())) {
					throw new MezaCardExistException(
							new ExceptionResponseMessage(CommonErrorCode.MEZA_CARD_EXIST,
									CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
									new TechnicalException()),
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
				}
			}
		}
		catch (Exception e) {
			throw new MezaCardExistException(
					new ExceptionResponseMessage(CommonErrorCode.MEZA_CARD_EXIST,
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
							new TechnicalException()),
					CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
		}
		logger.info("CHECK MEZA CARD STATUT :: DONE");
		return newAcmMezaCardDTO;
	}

	/**
	 * Check fields configuration.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @throws CheckFieldsConfigurationException the check fields configuration exception
	 */
	@SuppressWarnings("unchecked")
	private void checkFieldsConfiguration(CustomerDTO customerDTO)
			throws CheckFieldsConfigurationException {

		logger.info("START CHECK FIELDS CONFIGURATION");
		List<String> errors = new ArrayList<>();
		// Find from by code_IHM by customer type
		String codeIHM = "ADD_CUSTOMER_INDIV";
		if (CustomerType.GRP.name().equals(customerDTO.getCustomerType())) {
			codeIHM = "ADD_CUSTOMER_GRP";
		}
		else if (CustomerType.ORG.name().equals(customerDTO.getCustomerType())) {
			codeIHM = "ADD_CUSTOMER_ORG";
		}
		// Find ihm form details
		List<AcmIhmFieldDTO> ihmFieldDTOs =
				parametrageClient.findIhmField(new AcmIhmFieldDTO(codeIHM));
		// Processing data if list not EMPTY
		if (!ACMValidationUtils.isNullOrEmpty(ihmFieldDTOs)) {
			for (AcmIhmFieldDTO acmIhmFieldDTO : ihmFieldDTOs) {
				// get list VALIDATORS
				List<AcmIhmValidatorDTO> validators =
						!ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getValidators())
								? acmIhmFieldDTO.getValidators()
								: new ArrayList<>();
				// processing list
				for (AcmIhmValidatorDTO validator : validators) {
					// Check if field is REQUIRED
					if (SettingIHMValidatorCode.REQUIRED.code()
							.equals(validator.getCodeValidator())) {
						try {
							Field field = CustomerDTO.class
									.getDeclaredField(acmIhmFieldDTO.getCodeField());
							field.setAccessible(true);
							if ("userDefinedFieldsLinksDTOs".equals(field.getName())) {
								// get list UDF
								List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs =
										(List<UserDefinedFieldsLinksDTO>) field.get(customerDTO);
								if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTOs)) {
									errors.add(acmIhmFieldDTO.getDescription());
								}
								else {
									for (UserDefinedFieldsLinksDTO udf : userDefinedFieldsLinksDTOs) {
										if (udf.getUserDefinedFieldsDTO().getName()
												.equals(acmIhmFieldDTO.getTitre())) {
											Field fieldUDF = UserDefinedFieldsLinksDTO.class
													.getDeclaredField("fieldValue");
											fieldUDF.setAccessible(true);
											logger.debug("fieldUDF = {} | Value = {} ",
													acmIhmFieldDTO.getTitre(), fieldUDF.get(udf));
											if (ACMValidationUtils.isNullOrEmpty(fieldUDF.get(udf))
													|| "".equals(fieldUDF.get(udf))) {
												errors.add(acmIhmFieldDTO.getDescription());
											}
										}
									}
								}
							}
							else if (ACMValidationUtils.isNullOrEmpty(field.get(customerDTO))
									|| "".equals(field.get(customerDTO))) {
								logger.info("fielName = {} | Value = {} ", field.getName(),
										field.get(customerDTO));
								errors.add(field.getName());
							}
						}
						catch (NoSuchFieldException | SecurityException | IllegalArgumentException
								| IllegalAccessException e) {
							logger.error("{}", e.getMessage());
							errors.add("Data missing or incorrect");
						}
					}
				}
			}
		}
		try {
			// get setting from table :ACM_ADDRESS_SETTING to check mandatory field
			List<AddressSettingAbacusDTO> addressSettingsDTOs =
					transversClient.findSettingsAddress();
			// get list ADDRESS
			List<AddressDTO> addressDTOs = customerDTO.getListAddress();
			if (ACMValidationUtils.isNullOrEmpty(addressDTOs)) {
				addressSettingsDTOs
						.forEach(settingAddress -> errors.add(settingAddress.getAddressField()));
			}
			else {
				for (AddressSettingAbacusDTO settingAbacusDTO : addressSettingsDTOs) {
					for (AddressDTO adr : addressDTOs) {
						// get matched field
						Field fieldAddress = CommonFunctions.findMatchedFieldIngoreCase(
								AddressDTO.class, settingAbacusDTO.getAddressField().toLowerCase());
						if (fieldAddress != null) {
							fieldAddress.setAccessible(true);
							logger.debug("fieldAddress = {} | Value = {} ", fieldAddress.getName(),
									fieldAddress.get(adr));
							if (Boolean.TRUE.equals(settingAbacusDTO.getRequired())
									&& (ACMValidationUtils.isNullOrEmpty(fieldAddress.get(adr))
											|| "".equals(fieldAddress.get(adr)))) {
								errors.add(settingAbacusDTO.getAddressField());
							}
						}
					}
				}
			}
		}
		catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
			logger.error("{}", e.getMessage());
			errors.add("Address Data missing or incorrect");
		}

		// Return list of errors if exist
		if (!errors.isEmpty()) {
			logger.error("Failed to save customer : {}", errors);
			ExceptionResponseMessage exceptionResponseMessage =
					new ExceptionResponseMessage(CommonErrorCode.CHECK_FIELD_CONFIGURATION, null,
							new TechnicalException("Failed to Save Customer"), errors);
			throw new CheckFieldsConfigurationException(exceptionResponseMessage);
		}
		logger.info("CHECK FIELDS CONFIGURATION :: DONE");
	}

	/**
	 * Creates the user and send mail to customer.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @throws ResourcesNotFoundException the ResourcesNotFoundException
	 */
	private void createUserAndSendMail(CustomerDTO customerDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// get acmEnvironmentDTO of INTERNET_BANKING
		AcmEnvironnementDTO acmEnvironmentDTO =
				parametrageClient.find(CommonConstants.INTERNET_BANKING);
		// create user for ORG members
		if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
			List<CustomerDTO> customerMembers = findCustomersRelationShip(customerDTO);
			for (CustomerDTO newCustomerDTO : customerMembers) {
				// create user IB in DB for the customer member
				UserDTO userDTO = new UserDTO(newCustomerDTO.getCustomerNumber(),
						newCustomerDTO.getFirstName(), newCustomerDTO.getLastName(),
						newCustomerDTO.getEmail(), newCustomerDTO.getBranchId(),
						newCustomerDTO.getBranchesName(), newCustomerDTO.getBranchesDescription(),
						newCustomerDTO.getId());
				try {
					UserDTO ibUserDTO = userClient.createForIB(userDTO);
					userDTO.setCustomerId(newCustomerDTO.getId());
					logger.info(" USER IB inserted = {}", newCustomerDTO);
					// send login email if IB is enabled in AcmEnvironment
					if (!ACMValidationUtils.isNullOrEmpty(acmEnvironmentDTO)
							&& acmEnvironmentDTO.getValue().equals("1")) {
						// send notification mail to customer member
						sendMail(new MailCustomerDTO(newCustomerDTO, ibUserDTO.getLogin(),
								ibUserDTO.getPwd(),
								new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
										(!ACMValidationUtils
												.isNullOrEmpty(newCustomerDTO.getEmail())
												&& Boolean.TRUE.equals(StringUtils
														.mailIsValid(newCustomerDTO.getEmail())))
																? newCustomerDTO.getEmail()
																: defaultACMReceiverMail,
										"Welcome to ACM-IB (Internet Banking)", ""),
								MailBuilderMethod.BUILD_NEW_CUSTOMER));
					}
				}
				catch (Exception e) {
					logger.error(" failed to send mail or create IB user = {}", e.getMessage());
				}
			}
		}
		else {
			try {
				// create user IB for INDIV && ORG customer in DB
				UserDTO userDTO =
						new UserDTO(customerDTO.getCustomerNumber(), customerDTO.getFirstName(),
								customerDTO.getLastName(), customerDTO.getEmail(),
								customerDTO.getBranchId(), customerDTO.getBranchesName(),
								customerDTO.getBranchesDescription(), customerDTO.getId());
				// mapping ib user with created customer
				userDTO.setCustomerId(customerDTO.getId());
				UserDTO customerUserDTO = userClient.createForIB(userDTO);
				logger.info("USER Customer inserted = {}", customerUserDTO);

				// send login email if IB is enabled in AcmEnvironment
				if (!ACMValidationUtils.isNullOrEmpty(acmEnvironmentDTO)
						&& acmEnvironmentDTO.getValue().equals("1")) {
					// send notification mail to customer
					sendMail(new MailCustomerDTO(customerDTO, customerUserDTO.getLogin(),
							customerUserDTO.getPwd(),
							new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
									(!ACMValidationUtils.isNullOrEmpty(customerDTO.getEmail())
											&& Boolean.TRUE.equals(StringUtils
													.mailIsValid(customerDTO.getEmail())))
															? customerDTO.getEmail()
															: defaultACMReceiverMail,
									"Welcome to ACM-IB (Internet Banking)", ""),
							MailBuilderMethod.BUILD_NEW_CUSTOMER));
				}
			}
			catch (Exception e) {
				logger.error(" failed to send mail or create IB user = {}", e.getMessage());
			}
		}
	}

	/**
	 * Send mail.
	 *
	 * @author HaythemBenizid
	 * @param mailCustomerDTO the mail customer DTO
	 */
	private void sendMail(MailCustomerDTO mailCustomerDTO) {

		try {
			mailSenderClient.sendEmail(mailCustomerDTO);
			logger.info("Sending Email to customer :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/**
	 * Update for application.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws MezaCardExistException the meza card exist exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#updateForApplication(com.acm.utils.dtos. CustomerDTO)
	 */
	@Override
	public CustomerDTO updateForApplication(CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CreditException, ApiAbacusException, IOException,
			CalculateAgeException, MezaCardExistException {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CustomerDTO updatedCustomerDTO = new CustomerDTO();
		List<AddressDTO> addressDTOs = new ArrayList<>();
		List<AddressDTO> addressToSaveACMDTOs = customerDTO.getListAddress();

		// SAVE DB ACM
		if (customerDTO.getCustomerType().equalsIgnoreCase(CustomerType.INDIV.name())
				&& !Boolean.FALSE.equals(customerDTO.getCheckCustomer())) {
			// check double with customer id and national id
			checkCustomer(customerDTO);
		}

		SupplierDTO isSupplierDTO = checkIsSupplier(customerDTO);
		SupplierDTO isNotSupplierDTO = checkIsNotSupplier(customerDTO);

		if (!ACMValidationUtils.isNullOrEmpty(isSupplierDTO)) {
			customerDTO.setIsSupplier(Boolean.TRUE);
		}
		else {
			customerDTO.setIsSupplier(Boolean.FALSE);
		}

		// Check meza card Exist
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardId())) {
			checkMezaCardStatut(customerDTO, CommonConstants.UPDATE);
		}
		// Add new address
		for (AddressDTO addressDTO : customerDTO.getListAddress()) {
			addressDTO.setCustomerId(customerDTO.getId());
			if (!CommonConstants.ACTION_DELETE.equals(addressDTO.getAction())) {
				addressDTOs.add(addressDTO);
			}
		}
		customerDTO.setListAddress(addressDTOs);
		// CALL API ABACUS TO UPDATE CUSTOMER
		if (Boolean.TRUE.equals(customerDTO.getIsCustomer())) {

			try {
				transversClient.updateCustomer(customerDTO);
			}
			catch (Exception e) {
				// Fire Exception
				logger.error("Error updating Customer In Abacus = {}", e.getMessage());
				throw new ApiAbacusException(CommonErrorCode.API_ABACUS, e.getMessage());
			}
			customerDTO.setListAddress(addressToSaveACMDTOs);
			try {

				// update data in ACM customer and address and relationship
				updatedCustomerDTO = updateACMCustomer(customerDTO);

				// saving or updating UDF data in DB if not exist
				userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
						customerDTO.getUserDefinedFieldsLinksDTOs(), customerDTO.getId(),
						CommonConstants.CUSTOMER_CATEGORY);
				userDefinedFieldsLinksService.updateAbacusUdfLinksByElementId(customerDTO.getId(),
						CommonConstants.CUSTOMER_CATEGORY, customerDTO.getCustomerIdExtern(), null);
				// saveOrUpdateOrDeleteUDFCustomer(customerDTO);

				// update customer in IB
				if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIbCustomerId())) {
					// update ib customer
					loadDataIBService.update(customerDTO);
				}
			}
			catch (CreditException e) {
				throw new CreditException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_EXIST,
								CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
								new TechnicalException()),
						CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
			}
			catch (Exception e) {
				logger.error("Error updating Customer In ACM = {} ", e.getMessage());
			}
		}
		else {
			// update udf link In ACM
			logger.info("saving and updating udf");
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : customerDTO
					.getUserDefinedFieldsLinksDTOs()) {
				userDefinedFieldsLinksDTO.setCustomerId(customerDTO.getId());
				// check if is not null
				if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getId())
						&& !ACMValidationUtils
								.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {
					// Update data In ACM DB
					userDefinedFieldsLinksService.save(userDefinedFieldsLinksDTO.getId(),
							userDefinedFieldsLinksDTO);
				}
				else if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getId())
						&& ACMValidationUtils
								.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {
					// delete UDF from ACM DB by ID
					userDefinedFieldsLinksService.delete(userDefinedFieldsLinksDTO);
				}
				else if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getId())
						&& !ACMValidationUtils
								.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {
					userDefinedFieldsLinksService.save(userDefinedFieldsLinksDTO);
				}
			}
		}

		if (!ACMValidationUtils.isNullOrEmpty(isNotSupplierDTO)) {
			supplierService.save(isNotSupplierDTO);
		}

		if (!ACMValidationUtils.isNullOrEmpty(isSupplierDTO)) {
			supplierService.save(isSupplierDTO);
		}

		return updatedCustomerDTO;

	}

	/**
	 * Update ACM customer.
	 *
	 * @author MOEZ
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	private CustomerDTO updateACMCustomer(CustomerDTO customerDTO)
			throws CalculateAgeException, ResourcesNotFoundException, CreditException {

		// Processing Customer Links
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerLinksRelationshipDTOs())) {
			// Get CustomerMembers
			List<CustomerLinksRelationshipDTO> customerMembers =
					customerDTO.getCustomerLinksRelationshipDTOs().stream()
							.filter(customerLink -> customerLink.getCategory()
									.equals(LinkRelationshipsCategory.MEMBERS.name()))
							.collect(Collectors.toList());
			// Get CustomerRelationShip
			List<CustomerLinksRelationshipDTO> customerRelationShips =
					customerDTO.getCustomerLinksRelationshipDTOs().stream()
							.filter(customerLink -> customerLink.getCategory()
									.equals(LinkRelationshipsCategory.RELATIONSHIP.name()))
							.collect(Collectors.toList());

			// Check if members exist incoming list
			if (!ACMValidationUtils.isNullOrEmpty(customerMembers)) {
				CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
						new CustomerLinksRelationshipDTO();
				customerLinksRelationshipDTO.setCustomerId(customerDTO.getId());

				customerLinksRelationshipService.deleteByIdCustomerAndCategory(customerDTO.getId(),
						LinkRelationshipsCategory.MEMBERS.name());

				// save CustomerMembers
				for (CustomerLinksRelationshipDTO customerMembersDTO : customerMembers) {
					customerMembersDTO.setCustomerId(customerDTO.getId());
					customerLinksRelationshipDTOs
							.add(customerLinksRelationshipService.save(customerMembersDTO));
					// update grp or org name of customerMember
					if (customerMembersDTO.getCategory()
							.equals(LinkRelationshipsCategory.MEMBERS.name())) {

						CustomerDTO newCustomerDTOMember = mapper.map(
								customerRepository.findById(customerMembersDTO.getMember().getId()),
								CustomerDTO.class);
						if (CustomerType.GRP.name().equals(customerDTO.getCustomerType())) {
							newCustomerDTOMember.setSolidarityName(customerDTO.getCustomerName());
						}
						else if (CustomerType.ORG.name().equals(customerDTO.getCustomerType())) {
							newCustomerDTOMember.setOrganizationName(customerDTO.getCustomerName());
						}
						save(customerMembersDTO.getMember().getId(), newCustomerDTOMember);
					}
				}
			}
			else {
				// Delete grp or org name of customerMember
				CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
						new CustomerLinksRelationshipDTO();
				customerLinksRelationshipDTO.setCustomerId(customerDTO.getId());
				List<CustomerLinksRelationshipDTO> linksRelationshipDTOs =
						customerLinksRelationshipService.find(customerLinksRelationshipDTO);
				for (CustomerLinksRelationshipDTO customerLinksRelationship : linksRelationshipDTOs) {
					if (customerLinksRelationship.getCategory()
							.equals(LinkRelationshipsCategory.MEMBERS.name())) {
						if (CustomerType.GRP.name().equals(customerDTO.getCustomerType())) {
							customerLinksRelationship.getMember().setSolidarityName(null);
						}
						else if (CustomerType.ORG.name().equals(customerDTO.getCustomerType())) {
							customerLinksRelationship.getMember().setOrganizationName(null);
						}
						save(customerLinksRelationship.getMember().getId(),
								customerLinksRelationship.getMember());
					}
				}
				// Delete customerMembers by id customer and category Members
				customerLinksRelationshipService.deleteByIdCustomerAndCategory(customerDTO.getId(),
						LinkRelationshipsCategory.MEMBERS.name());
			}
			if (!ACMValidationUtils.isNullOrEmpty(customerRelationShips)) {

				// Delete customerRelationShip by id customer and category RelationShip
				customerLinksRelationshipService.deleteByIdCustomerAndCategory(customerDTO.getId(),
						LinkRelationshipsCategory.RELATIONSHIP.name());
				// save RelationShip Customer
				for (CustomerLinksRelationshipDTO customerRelationshipDTO : customerRelationShips) {
					customerRelationshipDTO.setCustomerId(customerDTO.getId());
					customerRelationshipDTO.setDateDebut(new Date());
					customerLinksRelationshipDTOs
							.add(customerLinksRelationshipService.save(customerRelationshipDTO));
				}
			}
			else {
				// Delete customerRelationShip by id customer and category relationShip
				customerLinksRelationshipService.deleteByIdCustomerAndCategory(customerDTO.getId(),
						LinkRelationshipsCategory.RELATIONSHIP.name());
			}
			// update date fin for oldLink
			if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getCustomerLinksDTOs())) {
				Long idLoan = customerDTO.getCustomerLinksDTOs().get(0).getIdLoan();
				for (CustomerLinksRelationshipDTO customerLinkByLoan : customerDTO
						.getCustomerLinksDTOs()) {
					customerLinkByLoan.setDateFin(new Date());
					customerLinksRelationshipService.save(customerLinkByLoan.getId(),
							customerLinkByLoan);
				}
				// save new link of Customer
				for (CustomerLinksRelationshipDTO customerLinksDTO : customerMembers) {
					customerLinksDTO.setCustomerId(customerDTO.getId());
					customerLinksDTO.setCategory(LinkRelationshipsCategory.LINK.name());
					customerLinksDTO.setIdLoan(idLoan);
					customerLinksDTO.setDateDebut(new Date());
					customerLinksRelationshipDTOs
							.add(customerLinksRelationshipService.save(customerLinksDTO));
				}
			}
		}
		else {
			// Delete customerRelationShip by id customer and category relationShip
			customerLinksRelationshipService.deleteByIdCustomerAndCategory(customerDTO.getId(),
					LinkRelationshipsCategory.RELATIONSHIP.name());
			// Delete customerMembers by id customer and category Members
			customerLinksRelationshipService.deleteByIdCustomerAndCategory(customerDTO.getId(),
					LinkRelationshipsCategory.MEMBERS.name());
		}
		// add customer
		customerDTO.setUpdateCustomer(Boolean.TRUE);
		CustomerDTO updateCustomerDTO = new CustomerDTO();
		updateCustomerDTO = save(customerDTO.getId(), customerDTO);
		// Add new address
		List<AddressDTO> addressDTOs = new ArrayList<>();
		// save Address
		// Action Insert: insert new Address, Action Update: Update Address, Action
		// Delete: delete
		// Address
		for (AddressDTO addressDTO : customerDTO.getListAddress()) {
			addressDTO.setCustomerId(customerDTO.getId());
			if (!ACMValidationUtils.isNullOrEmpty(addressDTO.getAction())) {
				switch (addressDTO.getAction()) {
					case CommonConstants.ACTION_INSERT:
						addressDTOs.add(addressService.save(addressDTO));
						break;
					case CommonConstants.ACTION_UPDATE:
						addressDTOs.add(addressService.save(addressDTO.getId(), addressDTO));
						break;
					case CommonConstants.ACTION_DELETE:
						addressService.delete(addressDTO);
						break;
					default:
						addressDTOs.add(addressDTO);
						break;
				}
			}
			else {
				addressDTOs.add(addressDTO);
			}
		}
		// setting new data
		updateCustomerDTO.setCustomerLinksRelationshipDTOs(customerLinksRelationshipDTOs);
		updateCustomerDTO.setListAddress(addressDTOs);
		updateCustomerDTO.setIndustryCode(customerDTO.getIndustryCode());
		// set MEZA card status to 'Assign' (if Meza Card disbursement method is
		// selected)
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardId())) {
			mezaCardservice.update(new AcmMezaCardDTO(customerDTO.getMezaCardId(),
					MezaCardStatus.ASSIGN.toString(), customerDTO));
		}
		if (customerDTO.getDisbursementMethodUpdatedToOtherThanMezaCard().equals(Boolean.TRUE)
				&& !updateCustomerDTO.getMezaCardStatus()
						.equals(CustomerMezaCardStatus.UNTRUSTED.name())) {
			mezaCardservice.update(new AcmMezaCardDTO(null, MezaCardStatus.ACTIVATE.toString(),
					updateCustomerDTO));
		}
		return updateCustomerDTO;

	}

	/**
	 * Save or update or delete UDF customer.
	 *
	 * @param customerDTO the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void saveOrUpdateOrDeleteUDFCustomer(CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		logger.info("saving and updating udf");
		// loading list UDF customer from ABACUS DB
		List<UserDefinedFieldsLinksDTO> listUDFCustomer =
				transversClient.loadUDFByCustomer(customerDTO.getCustomerIdExtern());
		logger.info("List UDFs customer size = {}", listUDFCustomer.size());
		if (!ACMValidationUtils.isNullOrEmpty(listUDFCustomer)) {
			ArrayList<UserDefinedFieldsDTO> listUDFs = new ArrayList<UserDefinedFieldsDTO>();
			listUDFCustomer.forEach(udfLink -> listUDFs.add(udfLink.getUserDefinedFieldsDTO()));
			logger.info("List UDFs customer size = {}", listUDFs.size());
			// find && setting udf field object
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
					parametrageClient.findUDFFieldByListIds(listUDFs);
			logger.info("List UDFs customer size = {}", userDefinedFieldsDTOs.size());
			// delete all udfLinks by customer Id from ACM_UDF_LINKS
			if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getId())) {
				// set id customer CASE :EDIT CUSTOMER
				userDefinedFieldsLinksService.deleteAllByCustomer(customerDTO.getId());
			}
			// save udfLinks from ABACUS IN ACM
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : listUDFCustomer) {
				userDefinedFieldsLinksDTO.setCategory(CommonConstants.CUSTOMER_CATEGORY);
				userDefinedFieldsLinksDTO.setElementId(customerDTO.getId());
				for (UserDefinedFieldsDTO udfDTO : userDefinedFieldsDTOs) {
					if (userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getIdUDFField()
							.equals(udfDTO.getIdUDFField())) {
						userDefinedFieldsLinksDTO.setUserDefinedFieldsDTO(udfDTO);
						userDefinedFieldsLinksService.saveByBatch(userDefinedFieldsLinksDTO);
					}
				}
			}

		}
	}

	/**
	 * Find customers relation ship.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomersRelationShip(com.acm.utils.dtos.
	 * CustomerDTO)
	 */
	@Override
	public List<CustomerDTO> findCustomersRelationShip(CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
				new CustomerLinksRelationshipDTO();
		customerLinksRelationshipDTO.setCustomerId(customerDTO.getId());
		customerLinksRelationshipDTO.setCategory(LinkRelationshipsCategory.MEMBERS.name());
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs =
				customerLinksRelationshipService.find(customerLinksRelationshipDTO);
		List<CustomerDTO> customerMembersDTOs = new ArrayList<>();
		// find Link Customer
		for (CustomerLinksRelationshipDTO newCustomerLinksRelationshipDTO : customerLinksRelationshipDTOs) {
			customerMembersDTOs.add(newCustomerLinksRelationshipDTO.getMember());
		}
		return customerMembersDTOs;
	}

	/**
	 * Check is not supplier.
	 *
	 * @param customerDTO the customer DTO
	 * @return the supplier DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	// Remove is Supplier flag
	private SupplierDTO checkIsNotSupplier(CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException {

		// Check customer is Supplier in case of update RNE or identity ( Remove the
		// flag is
		// supplier in Supplier )
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getId())) {
			Customer customerIsSupplier =
					customerRepository.findById(customerDTO.getId()).orElse(null);

			if (!ACMValidationUtils.isNullOrEmpty(customerIsSupplier)) {
				if (Boolean.TRUE.equals(customerIsSupplier.getIsSupplier())) {
					if (!Objects.equals(customerIsSupplier.getRegisterNumber(),
							customerDTO.getRegisterNumber())
							|| !Objects.equals(customerIsSupplier.getIdentity(),
									customerDTO.getIdentity())) {

						SupplierPaginationDTO supplierPaginationDTO = new SupplierPaginationDTO();
						SupplierDTO params = new SupplierDTO();
						if (!ACMValidationUtils
								.isNullOrEmpty(customerIsSupplier.getRegisterNumber())) {
							params.setRegisterNumber(customerIsSupplier.getRegisterNumber());
						}
						if (!ACMValidationUtils.isNullOrEmpty(customerIsSupplier.getIdentity())) {
							params.setIdentity(customerIsSupplier.getIdentity());
						}
						supplierPaginationDTO.setParams(params);

						SupplierPaginationDTO result = supplierService.find(supplierPaginationDTO);

						if (result.getResultsSuppliers().size() == 1) {
							result.getResultsSuppliers().get(0).setIsCustomer(Boolean.FALSE);
							return result.getResultsSuppliers().get(0);
							// supplierService.save(result.getResultsSuppliers().get(0));
						}
					}
				}
			}
		}

		return null;
	}

	/**
	 * Check is supplier.
	 *
	 * @param customerDTO the customer DTO
	 * @return the supplier DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	// Add is supplier flag
	private SupplierDTO checkIsSupplier(CustomerDTO customerDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException {

		// Check Customer is Supplier
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIdentity())
				|| !ACMValidationUtils.isNullOrEmpty(customerDTO.getRegisterNumber())) {

			SupplierPaginationDTO supplierPaginationDTO = new SupplierPaginationDTO();
			SupplierDTO params = new SupplierDTO();
			if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getRegisterNumber())) {
				params.setRegisterNumber(customerDTO.getRegisterNumber());
			}
			if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIdentity())) {
				params.setIdentity(customerDTO.getIdentity());
			}
			supplierPaginationDTO.setParams(params);

			SupplierPaginationDTO result = supplierService.find(supplierPaginationDTO);

			if (result.getResultsSuppliers().size() == 1) {
				result.getResultsSuppliers().get(0).setIsCustomer(Boolean.TRUE);
				return result.getResultsSuppliers().get(0);

			}
			else if (result.getResultsSuppliers().size() > 1) {
				throw new CreditException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_ERROR,
								CommonExceptionsMessage.CUSTOMER_ERROR_MSG,
								new TechnicalException()),
						CommonExceptionsMessage.CUSTOMER_ERROR_MSG);
			}
		}

		return null;
	}

	/**
	 * Check exist customer.
	 *
	 * @author HaythemBenizid / MoezMhiri
	 * @param customerDTO the customer DTO
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void checkCustomer(CustomerDTO customerDTO)
			throws CreditException, CalculateAgeException, ResourcesNotFoundException {

		// checKExistCustomer by identity number from UDF
		// init group ID params
		UserDefinedFieldGroupDTO paramsUDFGroup = new UserDefinedFieldGroupDTO();
		paramsUDFGroup.setCode(CommonConstants.UDF_CODE_NATIONALITY);
		// load list UserDefinedFields by ID udfGroup
		List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
				parametrageClient.find(new UserDefinedFieldsDTO(paramsUDFGroup));

		// filter to get National_ID UDF object
		List<UserDefinedFieldsDTO> userDefinedFieldsDTOsNationalID = userDefinedFieldsDTOs.stream()
				.filter(udf -> udf.getName().equals("National ID")).collect(Collectors.toList());
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOsNationalID)) {

			// check && compare requested National_ID by existing National_ID in DB
			if (Boolean.TRUE.equals(
					checkAndCompare(customerDTO, userDefinedFieldsDTOsNationalID.get(0).getId()))) {
				throw new CreditException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_EXIST,
								CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
								new TechnicalException()),
						CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
			}

		}

		// filter to get Resident_ID UDF object
		List<UserDefinedFieldsDTO> userDefinedFieldsDTOsResidentID = userDefinedFieldsDTOs.stream()
				.filter(udf -> udf.getName().equals("Resident ID")).collect(Collectors.toList());
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOsResidentID)) {

			// check && compare requested Resident_ID by existing Resident_ID in DB
			if (Boolean.TRUE.equals(
					checkAndCompare(customerDTO, userDefinedFieldsDTOsResidentID.get(0).getId()))) {
				throw new CreditException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_EXIST,
								CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
								new TechnicalException()),
						CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
			}

		}

		// checKExistCustomer by Telephone number
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getTelephone2())) {
			List<Customer> customersByPhone = customerRepository.findByTelephone2AndCustomerType(
					customerDTO.getTelephone2(), customerDTO.getCustomerType());
			// checKExistCustomer add by Telephone number
			if (ACMValidationUtils.isNullOrEmpty(customerDTO.getId())) {
				if (customersByPhone.size() > 0) {
					throw new CreditException(
							new ExceptionResponseMessage(
									CommonErrorCode.CUSTOMER_EXIST_PHONE_NUMBER,
									CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
									new TechnicalException()),
							CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
				}
			}
			// checKExistCustomer Update by Telephone number
			else {
				if ((customersByPhone.size() > 1) || (customersByPhone.size() == 1
						&& !customersByPhone.get(0).getId().equals(customerDTO.getId()))) {
					throw new CreditException(
							new ExceptionResponseMessage(
									CommonErrorCode.CUSTOMER_EXIST_PHONE_NUMBER,
									CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
									new TechnicalException()),
							CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
				}
			}

		}
		// checKExistCustomer add by Mobile number
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getTelephone1())) {
			List<Customer> customersbyMobile = customerRepository.findByTelephone1AndCustomerType(
					customerDTO.getTelephone1(), customerDTO.getCustomerType());
			// checKExistCustomer add by Telephone number
			if (ACMValidationUtils.isNullOrEmpty(customerDTO.getId())) {
				if (customersbyMobile.size() > 0) {
					throw new CreditException(
							new ExceptionResponseMessage(
									CommonErrorCode.CUSTOMER_EXIST_MOBILE_NUMBER,
									CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
									new TechnicalException()),
							CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
				}
			}
			// checKExistCustomer Update by Mobile number
			else {
				if ((customersbyMobile.size() > 1) || (customersbyMobile.size() == 1
						&& !customersbyMobile.get(0).getId().equals(customerDTO.getId()))) {
					throw new CreditException(
							new ExceptionResponseMessage(
									CommonErrorCode.CUSTOMER_EXIST_MOBILE_NUMBER,
									CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY,
									new TechnicalException()),
							CommonExceptionsMessage.CUSTOMER_EXIST_ALREADY);
				}
			}

		}
		// calculate AGE if not exist
		if (customerDTO.getDateOfBirth() == null) {
			Integer age = DateUtil.calculateAge(customerDTO.getDateOfBirth());
			if (age == -1) {
				logger.error(CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH);
				throw new CalculateAgeException(
						new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_INVALID_DATE_BIRTH,
								CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH),
						CommonExceptionsMessage.CUSTOMER_INVALID_DATE_BIRTH);
			}
		}
	}

	/**
	 * Check and compare => throw exception if any match exist.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param udfFieldsId the udf fields id
	 * @return the boolean
	 */
	private Boolean checkAndCompare(CustomerDTO customerDTO, Long udfFieldsId) {

		List<UserDefinedFieldsLinksDTO> udfLinkID = customerDTO.getUserDefinedFieldsLinksDTOs()
				.stream().filter(udf -> udf.getUserDefinedFieldsDTO().getId().equals(udfFieldsId))
				.collect(Collectors.toList());

		for (UserDefinedFieldsLinksDTO linkDTO : udfLinkID) {
			// check national ID
			if (!ACMValidationUtils.isNullOrEmpty(linkDTO.getFieldValue())) {
				UserDefinedFieldsLinksDTO params = new UserDefinedFieldsLinksDTO();
				params.setUserDefinedFieldsDTO(new UserDefinedFieldsDTO(udfFieldsId));

				params.setFieldValue(linkDTO.getFieldValue());
				// find values
				List<UserDefinedFieldsLinksDTO> acmLinkDB =
						userDefinedFieldsLinksService.find(params);

				if (!ACMValidationUtils.isNullOrEmpty(acmLinkDB)) {

					// check requested National_ID/Resident_ID by existing National_ID/Resident_ID
					// in DB

					// // chekc if national ID exist in DB
					// List<UserDefinedFieldsLinksDTO> acmLinkDB =
					// userDefinedFieldsLinksDTOs.stream()
					// .filter(udflink -> udflink.getFieldValue().equals(linkDTO.getFieldValue()))
					// .collect(Collectors.toList());

					// fire exception if any match was found
					// CASE : ADD CUSTOMER
					if (ACMValidationUtils.isNullOrEmpty(customerDTO.getId())
							&& acmLinkDB.size() > 0) {
						return Boolean.TRUE;
					}
					// CASE :EDIT CUSTOMER
					if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getId())
							&& ((acmLinkDB.size() > 1) || (acmLinkDB.size() == 1 && (acmLinkDB
									.get(0).getCategory().equals(CommonConstants.CUSTOMER_CATEGORY)
									&& !acmLinkDB.get(0).getElementId()
											.equals(customerDTO.getId()))))) {
						return Boolean.TRUE;
					}

				}
			}
		}
		// check by identity number
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getIdentity())) {
			List<Customer> customerByIdentity = customerRepository.findByIdentityAndCustomerType(
					customerDTO.getIdentity(), customerDTO.getCustomerType());
			// ADD CUSTOMER
			if (ACMValidationUtils.isNullOrEmpty(customerDTO.getId())
					&& customerByIdentity.size() > 0) {
				return Boolean.TRUE;
			}
			// EDIT CUSTOMER
			if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getId())
					&& (customerByIdentity.size() > 1 || (customerByIdentity.size() == 1
							&& !customerByIdentity.get(0).getId().equals(customerDTO.getId())))) {
				return Boolean.TRUE;
			}

		}
		return Boolean.FALSE;
	}

	/**
	 * Find for link.
	 *
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findGuarantor(com.acm.utils.dtos.pagination.
	 * CustomerPaginationDTO)
	 */
	@Override
	public CustomerPaginationDTO findForLink(CustomerPaginationDTO customerPaginationDTO) {

		Preconditions.checkNotNull(customerPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getPageNumber())) {
			customerPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getPageSize())) {
			customerPaginationDTO.setPageSize(10);
		}
		// setting default data
		customerPaginationDTO.setResultsCustomers(new ArrayList<>());
		// setting default totals pages
		customerPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		customerPaginationDTO.setTotalPages(0);

		// init list possible value
		List<String> customerLinkCategoryList =
				Arrays.asList("GUARANTOR", "GRP", "ORG", "RELATIONSHIP");
		// check CustomerLinkCategory value to load the right list
		if (ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerLinkCategory())
				|| !(customerLinkCategoryList
						.contains(customerPaginationDTO.getParams().getCustomerLinkCategory()))) {
			// returning empty list
			customerPaginationDTO.setResultsCustomers(new ArrayList<>());
			return customerPaginationDTO;
		}
		// init QCustomer
		QCustomer qCustomer = QCustomer.customer;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCustomer.enabled.eq(Boolean.TRUE));

		// Common Query Clause for : RELATIONSHIP / ORG / GRP / GUARANTOR
		// Customer Type = INDIV
		predicate.and(qCustomer.customerType.eq(CustomerType.INDIV.name()));

		// find by date insertion
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getDateInsertion())) {
			Timestamp StartTimesTamp = DateUtil.dateToDateTime(
					customerPaginationDTO.getParams().getDateInsertion(), "00:00:01");
			Timestamp EndTimesTamp = DateUtil.dateToDateTime(
					customerPaginationDTO.getParams().getDateInsertion(), "23:59:59");
			predicate.and(qCustomer.dateInsertion.between(StartTimesTamp, EndTimesTamp));
		}

		// Query Clause for : GUARANTOR
		if ("GUARANTOR".equals(customerPaginationDTO.getParams().getCustomerLinkCategory())) {
			// ignore guarantors selected
			if (!ACMValidationUtils
					.isNullOrEmpty(customerPaginationDTO.getParams().getGuarantors())) {
				List<Long> idGuarantors = new ArrayList<>();
				for (CustomerDTO customerDTO : customerPaginationDTO.getParams().getGuarantors()) {
					idGuarantors.add(customerDTO.getId());
				}
			}
			if (!ACMValidationUtils
					.isNullOrEmpty(customerPaginationDTO.getParams().getBranchId())) {
				// find by given customer branch Id
				predicate.and(
						qCustomer.branchId.eq(customerPaginationDTO.getParams().getBranchId()));
			}
		}
		// Query Clause for : ORG && Query Clause for : GRP
		else if ("GRP".equals(customerPaginationDTO.getParams().getCustomerLinkCategory())
				|| "ORG".equals(customerPaginationDTO.getParams().getCustomerLinkCategory())) {
			if (!ACMValidationUtils
					.isNullOrEmpty(customerPaginationDTO.getParams().getBranchId())) {
				// find by given customer branch Id
				predicate.and(
						qCustomer.branchId.eq(customerPaginationDTO.getParams().getBranchId()));
				// CustomerNumber not empty
				predicate.and(qCustomer.customerNumber.isNotEmpty());
			}
			else {
				// returning empty list
				customerPaginationDTO.setResultsCustomers(new ArrayList<>());
				return customerPaginationDTO;
			}
			// additional query param for GRP
			if ("GRP".equals(customerPaginationDTO.getParams().getCustomerLinkCategory())) {
				// For Group find only Female gender
				predicate.and(qCustomer.gender.eq("F"));
				// CustomerNumber not empty
				predicate.and(qCustomer.customerNumber.isNotEmpty());
			}
		}

		// find LIKE CustomerNumber
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerNumber())) {
			predicate.and(qCustomer.customerNumber
					.like("%" + customerPaginationDTO.getParams().getCustomerNumber() + "%"));
		}

		// find LIKE customerName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerName())) {
			predicate.and(qCustomer.customerName
					.like("%" + customerPaginationDTO.getParams().getCustomerName() + "%"));
		}

		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getParams().getIdentity())) {
			predicate.and(qCustomer.identity
					.like("%" + customerPaginationDTO.getParams().getIdentity() + "%"));
		}

		// find LIKE solidarityName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getSolidarityName())) {
			predicate.and(qCustomer.solidarityName
					.like("%" + customerPaginationDTO.getParams().getSolidarityName() + "%"));
		}

		// find LIKE branchesName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getBranchesName())) {
			predicate.and(qCustomer.branchesName
					.like("%" + customerPaginationDTO.getParams().getBranchesName() + "%"));
		}

		// find LIKE accountPortfolioDescription
		if (!ACMValidationUtils.isNullOrEmpty(
				customerPaginationDTO.getParams().getAccountPortfolioDescription())) {
			predicate.and(qCustomer.accountPortfolioDescription.like("%"
					+ customerPaginationDTO.getParams().getAccountPortfolioDescription() + "%"));
		}

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(customerPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = customerPaginationDTO.getSortField();
			if (customerPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(customerPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = customerPaginationDTO.getSortField();
			if (customerPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by customerName : DESC
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}

		// load data
		Page<Customer> pagedResult = customerRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Customer> customers = pagedResult.getContent();

			logger.info("{} : Customer LINK was founded (PageNumber = {} / PageSize = {} )",
					customers.size(), customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize());
			List<CustomerDTO> customerDTOs = new ArrayList<>();
			customers
					.forEach(customer -> customerDTOs.add(mapper.map(customer, CustomerDTO.class)));
			// setting data
			customerPaginationDTO.setResultsCustomers(customerDTOs);
			// setting totals pages
			customerPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			customerPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return customerPaginationDTO;
	}

	/**
	 * Adds the guarantors.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#addGuarantors(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO addGuarantors(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin addGuarantor method method...");
		CustomerLinksRelationshipDTO customerLinksRelationshipDTO;
		CustomerLinksRelationshipDTO newCustomerLinksRelationshipDTO;
		List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs;
		for (CustomerDTO guarantor : loanDTO.getGuarantors()) {
			if (!ACMValidationUtils.isNullOrEmpty(guarantor.getAction())) {
				switch (guarantor.getAction()) {
					case (CommonConstants.ACTION_UPDATE):
						customerLinksRelationshipDTO = new CustomerLinksRelationshipDTO();
						customerLinksRelationshipDTO.setMember(guarantor);
						customerLinksRelationshipDTO
								.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						// find only the guarantor by a givan loan to update only his amount
						customerLinksRelationshipDTO.setIdLoan(loanDTO.getLoanId());
						// find old relationship
						customerLinksRelationshipDTOs =
								customerLinksRelationshipService.find(customerLinksRelationshipDTO);
						if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTOs)) {
							// update new relationship
							customerLinksRelationshipDTOs.get(0)
									.setAmountGuarantor(guarantor.getAmountGuarantor());
							newCustomerLinksRelationshipDTO = customerLinksRelationshipService.save(
									customerLinksRelationshipDTOs.get(0).getId(),
									customerLinksRelationshipDTOs.get(0));
							logger.info(" Links Relationship ADD LINKSRELATIONSHIP_ID = [{}]",
									newCustomerLinksRelationshipDTO.getId());
						}
						break;

					case CommonConstants.ACTION_INSERT:
						// insert new link relationship
						customerLinksRelationshipDTO = new CustomerLinksRelationshipDTO(null, null,
								guarantor, null, CommonConstants.RELATION_GUARANTOR, new Date(),
								null, loanDTO.getLoanId(), guarantor.getAmountGuarantor());
						newCustomerLinksRelationshipDTO =
								customerLinksRelationshipService.save(customerLinksRelationshipDTO);
						logger.info(" Links Relationship ADD LINKSRELATIONSHIP_ID = [{}]",
								newCustomerLinksRelationshipDTO.getId());
						break;

					case CommonConstants.ACTION_DELETE:
						customerLinksRelationshipDTO = new CustomerLinksRelationshipDTO();
						customerLinksRelationshipDTO.setMember(guarantor);
						customerLinksRelationshipDTO
								.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						// find old relationship
						customerLinksRelationshipDTOs =
								customerLinksRelationshipService.find(customerLinksRelationshipDTO);
						if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTOs)) {
							// update new relationship
							customerLinksRelationshipDTOs.get(0).setEnabled(Boolean.FALSE);
							newCustomerLinksRelationshipDTO = customerLinksRelationshipService.save(
									customerLinksRelationshipDTOs.get(0).getId(),
									customerLinksRelationshipDTOs.get(0));
							logger.info(" Links Relationship delete LINKSRELATIONSHIP_ID = [{}]",
									newCustomerLinksRelationshipDTO.getId());
						}
						break;
					default:
						break;

				}
			}
		}

		// update guarantors in IB
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getIdIbLoan())) {
			loanDTO.setGuarantors(loanDTO.getGuarantors().stream()
					.filter(g -> (CommonConstants.ACTION_INSERT.equals(g.getAction())
							|| CommonConstants.ACTION_UPDATE.equals(g.getAction())))
					.collect(Collectors.toList()));
			loadDataIBService.updateGurantors(loanDTO);
		}
		return loanDTO;
	}

	/**
	 * Load all.
	 *
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#loadAll()
	 */
	@Override
	public void loadAll() throws CalculateAgeException, CreditException {

		logger.info("############# START ################# ");
		// load all customer
		List<CustomerDTO> customerDTOs = transversClient.load();
		logger.info("############# customerDTOs  size = {} ################# ",
				customerDTOs.size());
		List<CustomerDTO> customerDTOsORGGRP = new ArrayList<>();
		// insert All customer
		for (CustomerDTO customerDTOAbacus : customerDTOs) {
			// check if customer if exist in ACM DB by customerIdExtern
			List<CustomerDTO> listByCustomerIdExtern =
					findCustomerIdExtern(customerDTOAbacus.getCustomerIdExtern(), null);
			if (!ACMValidationUtils.isNullOrEmpty(listByCustomerIdExtern)) {
				logger.info(
						"############################## customer with ID_EXTERN = {} / NUMBER = {} has been add in ACM. ############################## ",
						listByCustomerIdExtern.get(0).getCustomerIdExtern(),
						listByCustomerIdExtern.get(0).getCustomerNumber());
			}
			else {
				// setting CustomerName
				if (customerDTOAbacus.getCustomerType().equals(CustomerType.GRP.name())) {
					customerDTOAbacus.setCustomerName(customerDTOAbacus.getCorrespondanceName());
					customerDTOAbacus.setSolidarityName(customerDTOAbacus.getCorrespondanceName());
				}
				else if (customerDTOAbacus.getCustomerType().equals(CustomerType.ORG.name())) {
					customerDTOAbacus.setCustomerName(customerDTOAbacus.getCorrespondanceName());
					customerDTOAbacus
							.setOrganizationName(customerDTOAbacus.getCorrespondanceName());
				}
				// insert customer in ACM DB
				CustomerDTO newCustomerDTO = save(customerDTOAbacus);
				logger.info(
						"**************************** new customer with ID = {} / NUMBER = {} has been add in ACM.",
						newCustomerDTO.getId(), newCustomerDTO.getCustomerNumber());

				// getting all customer with type ORG / GRP
				if (CustomerType.GRP.name().equals(newCustomerDTO.getCustomerType())
						|| CustomerType.ORG.name().equals(newCustomerDTO.getCustomerType())) {
					customerDTOsORGGRP.add(newCustomerDTO);
				}

				// Add ADDRESS
				List<AddressDTO> addressDTOs = transversClient
						.loadAddressByCustomer(customerDTOAbacus.getCustomerIdExtern());
				// save Address
				for (AddressDTO addressDTO : addressDTOs) {
					addressDTO.setCustomerId(newCustomerDTO.getId());
					addressService.save(addressDTO);
				}

				// ADD / UPDATE if exist
				List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs =
						transversClient.loadUDFByCustomer(customerDTOAbacus.getCustomerIdExtern());
				for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : userDefinedFieldsLinksDTOs) {
					// find && setting udf field object
					List<UserDefinedFieldsDTO> userDefinedFieldsDTOs = parametrageClient
							.find(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO());
					if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOs)) {
						userDefinedFieldsLinksDTO
								.setUserDefinedFieldsDTO(userDefinedFieldsDTOs.get(0));
						userDefinedFieldsLinksDTO.setCustomerId(newCustomerDTO.getId());
						UserDefinedFieldsLinksDTO newUserDefinedFieldsLinksDTO =
								userDefinedFieldsLinksService.save(userDefinedFieldsLinksDTO);
						if (newUserDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getId() == 3) {
							newCustomerDTO
									.setIdentity(newUserDefinedFieldsLinksDTO.getFieldValue());
							try {
								save(newCustomerDTO.getId(), newCustomerDTO);
							}
							catch (ResourcesNotFoundException | CalculateAgeException
									| CreditException e) {
								logger.error("{}", e.getMessage());
							}
						}
					}
				}
			}
		}
		logger.info("############# customerDTOsORGGRP  size = {} ################# ",
				customerDTOsORGGRP.size());
		// Mapping Members in cas customer type is GRP / ORG
		for (CustomerDTO customerDTO : customerDTOsORGGRP) {

			// init list
			List<CustomerMemberDTO> customerMemberDTOs = new ArrayList<>();

			// setting members if customer type is GRP or ORG
			if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				// load members if customer type = GRP
				customerMemberDTOs = transversClient
						.findMembersGroupByCustomer(customerDTO.getCustomerIdExtern());
			}
			else if (customerDTO.getCustomerType().equals(CustomerType.ORG.name())) {
				// load members if customer type = ORG
				customerMemberDTOs = transversClient
						.findMembersOrganisationByCustomer(customerDTO.getCustomerIdExtern());
			}

			// inserting group members
			for (CustomerMemberDTO customerMemberDTO : customerMemberDTOs) {
				// check if customer if exist in ACM DB by customerIdExtern
				List<CustomerDTO> customerData =
						findCustomerIdExtern(customerMemberDTO.getCustomerId(), null);
				if (!ACMValidationUtils.isNullOrEmpty(customerData)) {
					CustomerDTO member = customerData.get(0);
					// saving member in DB
					CustomerLinksRelationshipDTO newMember = customerLinksRelationshipService
							.save(new CustomerLinksRelationshipDTO(customerDTO.getId(), member,
									null, LinkRelationshipsCategory.MEMBERS.name(), new Date(),
									null, null));
					logger.info("****************************  new Member = {}", newMember);
				}
			}
		}
		logger.info("############# DONE ################# ");
	}

	/**
	 * Find customer active account.
	 *
	 * @param idCustomer the id customer
	 * @param idproduct the idproduct
	 * @return the long
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomerActiveAccount(java.lang.Long,
	 * java.lang.Long)
	 */
	@Override
	public Long findCustomerActiveAccount(Long idCustomer, Long idproduct) {

		Long customerActiveAccount = 0L;
		try {
			customerActiveAccount =
					transversClient.findCustomerActiveAccount(idCustomer, idproduct);
		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS : {}", e.getMessage());
			return null;
		}
		return customerActiveAccount;
	}

	/**
	 * Find all active accounts for customer.
	 *
	 * @param idCustomer the id customer
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findAllActiveAccountsForCustomer(java.lang. Long)
	 */
	@Override
	public List<CustomerActiveAccountDTO> findAllActiveAccountsForCustomer(Long idCustomer) {

		List<CustomerActiveAccountDTO> customerActiveAccountDTOs = new ArrayList<>();
		try {
			customerActiveAccountDTOs =
					transversClient.findAllActiveAccountsForCustomer(idCustomer);
		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS : {}", e.getMessage());
			return new ArrayList<>();
		}
		return customerActiveAccountDTOs;
	}

	/**
	 * Find arrears customer.
	 *
	 * @param idCustomer the id customer
	 * @return the arrears DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findDetailsCustomer(java.lang.Long)
	 */
	@Override
	public ArrearsDTO findArrearsCustomer(Long idCustomer) {

		Preconditions.checkNotNull(idCustomer, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("findArrearsCustomer by idCustomerExtern = {}", idCustomer);
		ArrearsDTO arrearsDTO = new ArrearsDTO();
		List<CustomerDTO> customerDTO = findCustomerIdExtern(idCustomer, null);
		if (customerDTO != null) {
			arrearsDTO = transversClient.findArrearsByCustomer(idCustomer);
		}
		return arrearsDTO;
	}

	/**
	 * Resend login.
	 *
	 * @param customerDTO the customer DTO
	 * @return the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#resendLogin(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public UserDTO resendLogin(CustomerDTO customerDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		UserDTO userDTO = userClient.findByLogin(customerDTO.getCustomerNumber());
		if (ACMValidationUtils.isNullOrEmpty(userDTO)) {
			createUserAndSendMail(customerDTO);
		}
		else {
			// send notification mail to customer
			sendMail(new MailCustomerDTO(userDTO, userDTO.getLogin(), new MailDTO(
					CommonConstants.NO_REPLAY_EMAIL,
					(!ACMValidationUtils.isNullOrEmpty(customerDTO.getEmail())
							&& Boolean.TRUE.equals(StringUtils.mailIsValid(customerDTO.getEmail())))
									? customerDTO.getEmail()
									: defaultACMReceiverMail,
					"Resend login successfull", ""), MailBuilderMethod.BUILD_USER_RESEND_LOGIN));
		}
		return userDTO;
	}

	/**
	 * Upload customer photo.
	 *
	 * @param photo the photo
	 * @param idCustomer the id customer
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#uploadCustomerPhoto(org.springframework.web. multipart.
	 * MultipartFile, com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public byte[] uploadCustomerPhoto(MultipartFile photo, String idCustomer)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idCustomer, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Customer by ID : {}", idCustomer);
		Customer oldCustomer = customerRepository.findById(Long.parseLong(idCustomer)).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldCustomer)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Customer.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ idCustomer);
		}
		try {
			oldCustomer.setPhoto(photo.getBytes());
			return customerRepository.save(oldCustomer).getPhoto();
		}
		catch (IOException e) {
			logger.error("Error Saving PHOTO to customer : {}", idCustomer);
		}
		return null;
	}

	/**
	 * Find photo customer.
	 *
	 * @param idCustomer the id customer
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findPhotoCustomer(java.lang.String)
	 */
	@Override
	public byte[] findPhotoCustomer(String idCustomer) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idCustomer, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Customer by ID : {}", idCustomer);
		Customer oldCustomer = customerRepository.findById(Long.parseLong(idCustomer)).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldCustomer)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Customer.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ idCustomer);
		}

		return oldCustomer.getPhoto();
	}

	/**
	 * Check customer loan statuts.
	 *
	 * @param customerDTO the customer DTO
	 * @return the boolean
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#checkCustomerLoanStatuts(com.acm.utils.dtos.
	 * CustomerDTO)
	 */
	@Override
	public Boolean checkCustomerLoanStatuts(CustomerDTO customerDTO) {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Boolean issued = Boolean.FALSE;
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getId())) {
			issued = loanService.checkLoanStatus(new LoanDTO(customerDTO,
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey()));
		}
		return issued;
	}

	/**
	 * Find customer paid account.
	 *
	 * @param idCustomer the id customer
	 * @param idproduct the idproduct
	 * @return the double
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findCustomerPaidAccount(java.lang.Long, java.lang.Long)
	 */
	@Override
	public Double findCustomerPaidAccount(Long idCustomer, Long idproduct) {

		Preconditions.checkNotNull(idCustomer, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(idproduct, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Double customerPaidAccount = 0D;
		try {
			customerPaidAccount = transversClient.findCustomerPaidAccount(idCustomer, idproduct);
		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS : {}", e.getMessage());
			return null;
		}
		return customerPaidAccount;
	}

	/**
	 * Update meza card status.
	 *
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#updateMezaCardStatus(com.acm.utils.dtos. CustomerDTO)
	 */
	@Override
	public CustomerDTO updateMezaCardStatus(CustomerDTO customerDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(customerDTO.getId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update Customer with ID = {}", customerDTO.getId());
		Customer oldCustomer = customerRepository.findById(customerDTO.getId()).orElse(null);

		// check if object is null
		if (oldCustomer == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Customer.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + customerDTO.getId());
		}

		// setting MEZA-CARD status
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardStatus()) && EnumUtils
				.isValidEnum(CustomerMezaCardStatus.class, customerDTO.getMezaCardStatus())) {
			oldCustomer.setMezaCardStatus(customerDTO.getMezaCardStatus());
		}
		else {
			logger.warn(
					"Failed to update status Meza card : given MezaCardStatus ({}) is NULL or not in Enum (CustomerMezaCardStatus)",
					customerDTO.getMezaCardStatus());
			return customerDTO;
		}
		CommonFunctions.mapperToUpdate(oldCustomer, userClient, logger);

		// update & persist data in DB
		Customer newCustomer = customerRepository.save(oldCustomer);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Customer.class.getSimpleName());
		return mapper.map(newCustomer, CustomerDTO.class);
	}

	/**
	 * Find by mez card status.
	 *
	 * @param customerMezaCardStatus the customer meza card status
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findByMezCardStatus(java.lang.String)
	 */
	@Override
	public List<CustomerDTO> findByMezCardStatus(String customerMezaCardStatus) {

		logger.info("Find by MEZA CARD status {}", customerMezaCardStatus);
		// check given MEZA-CARD status
		if (ACMValidationUtils.isNullOrEmpty(customerMezaCardStatus)
				&& !EnumUtils.isValidEnum(CustomerMezaCardStatus.class, customerMezaCardStatus)) {
			// returning empty list
			return new ArrayList<>();
		}
		// find by MEZA CARD status
		List<Customer> customers = customerRepository
				.findByMezaCardStatusAndEnabled(customerMezaCardStatus, Boolean.TRUE);
		// mapping && returning data
		List<CustomerDTO> customerDTOs = new ArrayList<>();
		customers.forEach(customer -> customerDTOs.add(mapper.map(customer, CustomerDTO.class)));
		logger.info("Method = findByMezCardStatus() : {} : customer was founded",
				customerDTOs.size());
		return customerDTOs;
	}

	/**
	 * Find for meza card.
	 *
	 * @param customerPaginationDTO the customer pagination DTO
	 * @return the customer pagination DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findForMezaCard(com.acm.utils.dtos. pagination.
	 * CustomerPaginationDTO)
	 */
	@Override
	public CustomerPaginationDTO findForMezaCard(CustomerPaginationDTO customerPaginationDTO) {

		Preconditions.checkNotNull(customerPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getPageNumber())) {
			customerPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getPageSize())) {
			customerPaginationDTO.setPageSize(10);
		}
		// setting default data
		customerPaginationDTO.setResultsCustomers(new ArrayList<>());
		// setting default totals pages
		customerPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		customerPaginationDTO.setTotalPages(0);
		// if mezacardstatuts is null or empty return customerPaginationDTO
		if (ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getMezaCardStatus())) {
			return customerPaginationDTO;
		}
		// init QCustomer
		QCustomer qCustomer = QCustomer.customer;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCustomer.enabled.eq(Boolean.TRUE));
		// find by given meza card status
		predicate.and(
				qCustomer.mezaCardStatus.eq(customerPaginationDTO.getParams().getMezaCardStatus()));

		// find LIKE CustomerNumber
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerNumber())) {
			predicate.and(qCustomer.customerNumber
					.like("%" + customerPaginationDTO.getParams().getCustomerNumber() + "%"));
		}

		// find LIKE customerName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getCustomerName())) {
			StringTemplate convertedCustomerName = Expressions
					.stringTemplate("function('replace', {0}, '|', ' ')", qCustomer.customerName);
			predicate.and(convertedCustomerName
					.like("%" + customerPaginationDTO.getParams().getCustomerName() + "%"));
		}

		// find LIKE identity
		if (!ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getParams().getIdentity())) {
			predicate.and(qCustomer.identity
					.like("%" + customerPaginationDTO.getParams().getIdentity() + "%"));
		}

		// find LIKE branchesName
		if (!ACMValidationUtils
				.isNullOrEmpty(customerPaginationDTO.getParams().getBranchesName())) {
			predicate.and(qCustomer.branchesName
					.like("%" + customerPaginationDTO.getParams().getBranchesName() + "%"));
		}

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(customerPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = customerPaginationDTO.getSortField();
			if (customerPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(customerPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(customerPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = customerPaginationDTO.getSortField();
			if (customerPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by customerName : DESC
			pageable = PageRequest.of(customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize(), Sort.Direction.ASC, "customerName");
		}
		// load data
		Page<Customer> pagedResult = customerRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Customer> customers = pagedResult.getContent();
			logger.info(
					"Meza card pagination : {} : Customer was founded (PageNumber = {} / PageSize = {} )",
					customers.size(), customerPaginationDTO.getPageNumber(),
					customerPaginationDTO.getPageSize());
			List<CustomerDTO> customerDTOs = new ArrayList<>();
			// mapping data
			customers.forEach(customer -> {
				CustomerDTO dto = mapper.map(customer, CustomerDTO.class);
				// setting assigned MEZACARD data
				if (!ACMValidationUtils.isNullOrEmpty(customer.getAcmMezaCards())) {
					dto.setAcmMezaCardDTO(mapper.map(customer.getAcmMezaCards().iterator().next(),
							AcmMezaCardDTO.class));
				}
				else {
					dto.setAcmMezaCardDTO(new AcmMezaCardDTO());
				}
				customerDTOs.add(dto);
			});
			// setting data
			customerPaginationDTO.setResultsCustomers(customerDTOs);
			// setting totals pages
			customerPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			customerPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return customerPaginationDTO;
	}

	/**
	 * Count.
	 *
	 * @return the customer meza card statut DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#count()
	 */
	@Override
	public CustomerMezaCardStatutDTO count() {

		CustomerMezaCardStatutDTO customerMezaCardStatutDTO =
				new CustomerMezaCardStatutDTO(0L, 0L, 0L);
		try {
			// statut sent
			customerMezaCardStatutDTO.setSent(customerRepository.countByMezaCardStatusAndEnabled(
					CustomerMezaCardStatus.SENT.name(), Boolean.TRUE));
			// statut trusted
			customerMezaCardStatutDTO.setTrusted(customerRepository.countByMezaCardStatusAndEnabled(
					CustomerMezaCardStatus.TRUSTED.name(), Boolean.TRUE));
			// statut untrusted
			customerMezaCardStatutDTO
					.setUntrusted(customerRepository.countByMezaCardStatusAndEnabled(
							CustomerMezaCardStatus.UNTRUSTED.name(), Boolean.TRUE));
		}
		catch (Exception e) {
			logger.error("ERROR Returning count customerMezaCardStatut by status {}",
					e.getMessage());
			e.printStackTrace();
		}
		logger.info("Returning COUNT STATUT Meza card {}", customerMezaCardStatutDTO);
		return customerMezaCardStatutDTO;
	}

	/**
	 * Update all.
	 *
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#updateAll(com.acm.utils.dtos.CustomerDTO)
	 */
	@Override
	public void updateAll(List<CustomerDTO> customerDTOs) throws ResourcesNotFoundException {

		// update all meza card statuts for all given customers
		logger.info("START UPDATE ALL MEZA CARD STATUS FOR ALL GIVEN CUSTOMER");
		if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
			for (CustomerDTO customerDTO : customerDTOs) {
				// update meza card status
				updateMezaCardStatus(customerDTO);
			}
		}
		logger.info("DONE UPDATE ALL MEZA CARD STATUS");
	}

	/**
	 * Save ACM customer.
	 *
	 * @author MoezMhiri
	 * @param customerDTO the customer DTO
	 * @return the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	private CustomerDTO saveACMCustomer(CustomerDTO customerDTO) throws IOException,
			ApiAbacusException, CalculateAgeException, ResourcesNotFoundException, CreditException {

		logger.info("Method updateACMCustomer() :: START");

		CustomerDTO newCustomerDTO = save(customerDTO);
		logger.info("Customer Inserted to ACM : {} ABACUS : {})", newCustomerDTO.getId(),
				customerDTO.getCustomerIdExtern());

		// save Link Customer
		List<CustomerLinksRelationshipDTO> newCustomerLinksRelationshipDTOs = new ArrayList<>();
		for (CustomerLinksRelationshipDTO customerLinksRelationshipDTO : customerDTO
				.getCustomerLinksRelationshipDTOs()) {
			customerLinksRelationshipDTO.setCustomerId(newCustomerDTO.getId());
			customerLinksRelationshipDTO.setDateDebut(new Date());
			newCustomerLinksRelationshipDTOs
					.add(customerLinksRelationshipService.save(customerLinksRelationshipDTO));
			if (customerLinksRelationshipDTO.getCategory()
					.equals(LinkRelationshipsCategory.MEMBERS.name())) {
				CustomerDTO newCustomerDTOMember =
						findCustomer(customerLinksRelationshipDTO.getMember().getId());
				if (CustomerType.GRP.name().equals(newCustomerDTO.getCustomerType())) {
					newCustomerDTOMember.setSolidarityName(customerDTO.getCustomerName());
				}
				else if (CustomerType.ORG.name().equals(newCustomerDTO.getCustomerType())) {
					newCustomerDTOMember.setOrganizationName(customerDTO.getCustomerName());
				}
				save(customerLinksRelationshipDTO.getMember().getId(), newCustomerDTOMember);
			}
		}

		// save Address
		List<AddressDTO> newAddressDTOs = new ArrayList<>();
		for (AddressDTO addressDTO : customerDTO.getListAddress()) {
			addressDTO.setCustomerId(newCustomerDTO.getId());
			if (ACMValidationUtils.isNullOrEmpty(addressDTO.getIsPrimary())) {
				addressDTO.setIsPrimary(Boolean.TRUE);
			}
			newAddressDTOs.add(addressService.save(addressDTO));
		}
		// setting data
		newCustomerDTO.setCustomerLinksRelationshipDTOs(newCustomerLinksRelationshipDTOs);
		newCustomerDTO.setListAddress(newAddressDTOs);
		newCustomerDTO.setUserDefinedFieldsLinksDTOs(customerDTO.getUserDefinedFieldsLinksDTOs());
		// set MEZA card status to 'Assign' (if Meza Card disbursement method is
		// selected)
		if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardId())) {
			mezaCardservice.update(new AcmMezaCardDTO(customerDTO.getMezaCardId(),
					MezaCardStatus.ASSIGN.toString(), newCustomerDTO));
		}
		logger.info("Update customer :: DONE");
		// saving udf in acm
		userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
				customerDTO.getUserDefinedFieldsLinksDTOs(), newCustomerDTO.getId(),
				CommonConstants.CUSTOMER_CATEGORY);
		userDefinedFieldsLinksService.updateAbacusUdfLinksByElementId(newCustomerDTO.getId(),
				CommonConstants.CUSTOMER_CATEGORY, customerDTO.getCustomerIdExtern(), null);
		// saveOrUpdateOrDeleteUDFCustomer(customerDTO);
		logger.info("saving or Updating [{}] UDF Link for customer in ACM-DB :: DONE",
				customerDTO.getUserDefinedFieldsLinksDTOs().size());
		return newCustomerDTO;
	}

	/**
	 * Find for analytics.
	 *
	 * @param hasActiveLoan the has active loan
	 * @param firstDayMonth the first day month
	 * @param lastDayMonth the last day month
	 * @return the map
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findForAnalytics(java.lang.Boolean, java.time.LocalDate,
	 * java.time.LocalDate)
	 */
	@Override
	public Map<Integer, Integer> findForAnalytics(Boolean hasActiveLoan, LocalDate firstDayMonth,
			LocalDate lastDayMonth) {

		Map<Integer, Integer> resulatMap = new HashMap<>();
		// if no date is passed => return empty MAP
		if (ACMValidationUtils.isNullOrEmpty(firstDayMonth)
				|| ACMValidationUtils.isNullOrEmpty(lastDayMonth)) {
			resulatMap.put(0, 0);
			return resulatMap;
		}
		// init QCustomer
		QCustomer qCustomer = QCustomer.customer;
		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCustomer.enabled.eq(Boolean.TRUE));
		// find customer by Account Portfolio Id
		predicate.and(qCustomer.accountPortfolioID.eq(userDTO.getAccountPortfolioId()));
		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.OPERATION.name())) {
			// find all user COLLABORATOR
			List<Long> wheresAccountPortfolioID = new ArrayList<>();
			List<UserDTO> userDTOs = userClient.findUsers();
			userDTOs.forEach(user -> {
				if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
					wheresAccountPortfolioID.add(user.getAccountPortfolioId());
				}
			});
			// setting predicate to find by accountPortfolioID
			predicate.or(qCustomer.accountPortfolioID
					.in(new ArrayList<>(new HashSet<>(wheresAccountPortfolioID))));
		}
		// find loan by Access Branches for connected user
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// setting predicate to find by given branch Id
			predicate.or(qCustomer.branchId.in(parseAccessUserBranch(userDTO)));
		}
		else if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			// find by given branch Id
			predicate.or(qCustomer.branchId.eq(userDTO.getBranchID()));
		}
		// find by ApplyDate for given month
		java.sql.Date sqlDateMin = java.sql.Date.valueOf(firstDayMonth);
		java.sql.Date sqlDateMax = java.sql.Date.valueOf(lastDayMonth);
		predicate.and(qCustomer.customerOpenDate.goe(sqlDateMin));
		predicate.and(qCustomer.customerOpenDate.loe(sqlDateMax));
		logger.info("predicate = {}", predicate);
		// find data
		Iterable<Customer> iterableCustomer = customerRepository.findAll(predicate);
		// check if customer has active LOAN
		// init list status REJECTED / CANCELLED / DECLINE
		List<Integer> listStatutRejectedCanceled = new ArrayList<>();
		// loan canceled => StatutWorkflow in REJECTED or CANCELLED or DECLINE
		listStatutRejectedCanceled
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
		listStatutRejectedCanceled
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
		listStatutRejectedCanceled
				.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());
		List<Customer> customers = new ArrayList<>();
		iterableCustomer.forEach(customers::add);
		Integer countActiveCustomer = 0;
		for (Customer customer : customers) {
			logger.info("{}", customer.getLoans().size());
			if (!ACMValidationUtils.isNullOrEmpty(customer.getLoans())) {
				long count = customer.getLoans().stream().filter(
						loan -> listStatutRejectedCanceled.contains(loan.getStatutWorkflow()))
						.count();
				if (!ACMValidationUtils.isNullOrEmpty(count) && count == 0) {
					countActiveCustomer++;
				}
			}
		}
		resulatMap.put(customers.size(), countActiveCustomer);
		logger.info("Method = findForAnalytics() : size = {}", resulatMap);
		return resulatMap;
	}

	/**
	 * Update customers branches.
	 *
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#updateCustomersBranches(java.util.List)
	 */
	@Override
	public void updateCustomersBranches(List<CustomerDTO> customerDTOs)
			throws ResourcesNotFoundException {

		// init list of customers to be branch updated in ACM
		List<Customer> customersToBeUpdated = new ArrayList<>();
		// init list of id externs not found in ACM DB
		List<Long> idExternsNotFound = new ArrayList<>();
		// loop on customers who have their branch updated in Abacus
		customerDTOs.forEach(customerDTO -> {
			// get customerDTO object from ACM DB
			List<Customer> oldCustomer =
					customerRepository.findByCustomerIdExtern(customerDTO.getCustomerIdExtern());
			// check if object is null
			if (ACMValidationUtils.isNullOrEmpty(oldCustomer)) {
				idExternsNotFound.add(customerDTO.getCustomerIdExtern());
			}
			else {

				// update branchId
				oldCustomer.get(0).setBranchId(customerDTO.getBranchId());
				// update branch description
				oldCustomer.get(0).setBranchesDescription(customerDTO.getBranchesDescription());
				// update branch name
				oldCustomer.get(0).setBranchesName(customerDTO.getBranchesName());

				CommonFunctions.mapperToUpdate(oldCustomer.get(0), userClient, logger);
				// add customer object to the list of customers that will be updated in ACM
				customersToBeUpdated.add(oldCustomer.get(0));

			}
		});
		try {
			// update customers in ACM
			customerRepository.saveAll(customersToBeUpdated);
		}
		catch (Exception e) {
			logger.error("Error while saving customers in acm_db = {}", e.getMessage());
		}
		if (!ACMValidationUtils.isNullOrEmpty(idExternsNotFound)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Customer.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID_EXTERN + idExternsNotFound);
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Customer.class.getSimpleName());

	}

	/**
	 * Update all customer.
	 *
	 * @param customerDTOs the customer DT os
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#updateAllCustomer(java.util.List)
	 */
	@Override
	public void updateAllCustomer(List<CustomerDTO> customerDTOs)
			throws ResourcesNotFoundException {

		logger.info("START UPDATE FOR ALL GIVEN CUSTOMER");

		List<Customer> customers = new ArrayList<>();

		customerDTOs.forEach(customerDTO -> {
			List<Customer> customerList =
					customerRepository.findByCustomerIdExtern(customerDTO.getCustomerIdExtern());
			if (!ACMValidationUtils.isNullOrEmpty(customerList)) {
				Customer customer = customerList.get(0);
				customer.setAccountPortfolioID(customerDTO.getAccountPortfolioID());
				customer.setAccountPortfolioCode(customerDTO.getAccountPortfolioCode());
				customer.setAccountPortfolioDescription(
						customerDTO.getAccountPortfolioDescription());

				CommonFunctions.mapperToUpdate(customer, userClient, logger);
				customers.add(customer);
			}
		});

		customerRepository.saveAll(customers);

		logger.info("DONE UPDATE ALL CUSTOMER");

	}

	/**
	 * Gets the guarantors details.
	 *
	 * @param idLoan the id loan
	 * @return the guarantors details
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#getGuarantorsDetails(java.lang.Long)
	 */
	@Override
	public CustomerDetailsReportsDTO getGuarantorsDetails(Long idLoan) {

		Preconditions.checkNotNull(idLoan, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Guarantor from Link Relationship by id loan");
		CustomerDetailsReportsDTO guarantorDetailsDTO = new CustomerDetailsReportsDTO();

		// get the guarantor by id loan
		CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
		params.setIdLoan(idLoan);
		params.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
		// find guarantor by id loan
		List<CustomerLinksRelationshipDTO> existGuarantors =
				customerLinksRelationshipService.find(params);
		// check guarantor exist in ABACUS DB
		if (!ACMValidationUtils.isNullOrEmpty(existGuarantors)
				&& existGuarantors.get(0).getMember() != null) {
			// set guarantor name without pipe
			existGuarantors.get(0).getMember().setCustomerNameNoPipe(
					existGuarantors.get(0).getMember().getCustomerName().replace("|", " "));
			// set customer Name
			guarantorDetailsDTO
					.setCustomerName(existGuarantors.get(0).getMember().getCustomerNameNoPipe());
			// set guarantor mobile number
			guarantorDetailsDTO.setMobileNumber(existGuarantors.get(0).getMember().getTelephone());
			guarantorDetailsDTO.setPhoneNumber(existGuarantors.get(0).getMember().getTelephone2());
			// set guarantor identity number
			guarantorDetailsDTO.setIdentity(existGuarantors.get(0).getMember().getIdentity());
			// set two addresses if exist
			List<AddressDTO> listofFirstTowAddresses = existGuarantors.get(0).getMember()
					.getListAddress().stream().limit(2).collect(Collectors.toList());
			guarantorDetailsDTO.setAddressDTO(listofFirstTowAddresses);

			// get udf of guarantor
			UserDefinedFieldsLinksDTO param = new UserDefinedFieldsLinksDTO();
			// set id customer
			param.setCustomerId(existGuarantors.get(0).getMember().getId());
			// find the udfs grouped by group udf : with the value of each fieldValue
			List<UDFLinksGroupeFieldsDTO> listUdfGroupeFieldsDTOs =
					userDefinedFieldsLinksService.findUDFGroupBy(param);
			// get group of nationality
			UDFLinksGroupeFieldsDTO udfLinksGroupeFieldsNationality = listUdfGroupeFieldsDTOs
					.stream().filter(element -> element.getUserDefinedFieldGroupID() == 2)
					.findFirst().orElse(null);
			// get profession group
			UDFLinksGroupeFieldsDTO udfLinksGroupeFieldsProfession = listUdfGroupeFieldsDTOs
					.stream().filter(element -> element.getUserDefinedFieldGroupID() == 1)
					.findFirst().orElse(null);
			// filter the list of nationality : get the place of issue and the issue date
			for (UDFLinksGroupeFieldsModelDTO udf : udfLinksGroupeFieldsNationality
					.getUdfGroupeFieldsModels()) {
				if (udf.getFieldName().equals(CommonConstants.UDF_PLACE_OF_ISSUE)) {
					guarantorDetailsDTO.setPlaceOfIssue(udf.getValue());
				}
				else if (udf.getFieldName().equals(CommonConstants.UDF_ISSUE_DATE)) {
					guarantorDetailsDTO.setIssueDate(udf.getValue());
				}
				else if (udf.getFieldName().equals(CommonConstants.UDF_FAMILY_SITUATION)) {
					guarantorDetailsDTO.setFamilySituation(udf.getValue());
				}
			}
			// get the profession description
			if (!ACMValidationUtils.isNullOrEmpty(udfLinksGroupeFieldsProfession)) {
				UDFLinksGroupeFieldsModelDTO profession =
						udfLinksGroupeFieldsProfession.getUdfGroupeFieldsModels().stream()
								.filter(udfG -> udfG.getFieldName().equals(CommonConstants.UDF_JOB))
								.findFirst().orElse(null);
				if (!ACMValidationUtils.isNullOrEmpty(profession)) {
					guarantorDetailsDTO.setProfession(profession.getValue());
				}

			}

		}
		return guarantorDetailsDTO;
	}

	/**
	 * Find account schedule by customer id.
	 *
	 * @param idExternCustomer the id extern customer
	 * @param accountNumberExtern the account number extern
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerService#findAccountScheduleByCustomerId(java.lang. Long)
	 */
	@Override
	public List<ScheduleDTO> findAccountScheduleByCustomerId(Long idExternCustomer,
			String accountNumberExtern) {

		List<ScheduleDTO> scheduleDTOs = new ArrayList<>();
		try {
			scheduleDTOs = transversClient.findAccountScheduleByCustomerId(idExternCustomer,
					accountNumberExtern);
		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS : {}", e.getMessage());
			return null;
		}
		return scheduleDTOs;
	}

}
