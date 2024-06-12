/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import javax.persistence.EntityManager;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

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
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.IBLoanRepository;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.service.CustomerService;
import com.acm.service.IBLoanService;
import com.acm.service.LoanService;
import com.acm.service.NotificationsServices;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.service.api_ib.LoadDataIBService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.IBLoanDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.LoanStatutDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailIBLoanDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.pagination.IBLoanPaginationDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.models.IBLoan;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QIBLoan;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Projections;
import com.querydsl.jpa.impl.JPAQueryFactory;

import feign.FeignException;

/**
 * {@link IBLoanServiceImpl} IBLoanServiceImpl.
 *
 * @author MoezMhiri
 * @since 1.0.3
 */
@Service
public class IBLoanServiceImpl implements IBLoanService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(IBLoanServiceImpl.class);

	/** The loan ib repository. */
	@Autowired
	private IBLoanRepository loanIbRepository;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The customer links relationship service. */
	@Autowired
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The load data IB service. */
	@Autowired
	private LoadDataIBService loadDataIBService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#find(java.lang.Long)
	 */
	@Override
	public IBLoanDTO find(Long id) throws ResourcesNotFoundException {

		return loadDataIBService.find(id);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#find(com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public List<IBLoanDTO> find(IBLoanDTO ibLoanDTO) {

		Preconditions.checkNotNull(ibLoanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QHabilitation
		QIBLoan qIbLoan = QIBLoan.iBLoan;

		// init Predicate
		BooleanBuilder predicate = buildQuery(ibLoanDTO, qIbLoan);

		// find by username
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		predicate.and(qIbLoan.owner.eq(userDTO.getLogin()));
		// find by statut = 0
		predicate.and(qIbLoan.statut.eq(ibLoanDTO.getStatut()));
		// QueryDSL using springDATA
		Iterable<IBLoan> iterable = loanIbRepository.findAll(predicate);
		List<IBLoan> ibLoans = new ArrayList<>();
		iterable.forEach(ibLoans::add);
		logger.info("{} : ib loans was founded", ibLoans.size());

		// mapping returned list
		List<IBLoanDTO> ibLoanDTOs = new ArrayList<>();
		ibLoans.forEach(ibLoan -> ibLoanDTOs.add(mapper.map(ibLoan, IBLoanDTO.class)));
		return ibLoanDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#find(com.acm.utils.dtos.pagination.IBLoanPaginationDTO)
	 */
	@Override
	public IBLoanPaginationDTO find(IBLoanPaginationDTO loanIbPaginationDTO) {

		return loadDataIBService.find(loanIbPaginationDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#save(com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public LoanDTO saveAcmLoanInIB(LoanDTO loanDTO) {

		return loadDataIBService.saveAcmLoanInIB(loanDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#save(java.lang.Long, com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public IBLoanDTO save(Long id, IBLoanDTO ibLoanDTO) throws ResourcesNotFoundException {

		return loadDataIBService.update(ibLoanDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#assignedAll(com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public List<IBLoanDTO> assignedAll(List<IBLoanDTO> ibLoanDTOs)
			throws ResourcesNotFoundException, CalculateAgeException, CreditException {

		for (IBLoanDTO ibLoanDTO : ibLoanDTOs) {
			Preconditions.checkNotNull(ibLoanDTO.getId(),
					CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
			logger.info("Update ib loan with ID = {}", ibLoanDTO.getId());
			IBLoan oldIbLoan = loanIbRepository.findById(ibLoanDTO.getId()).orElse(null);
			// check if object is null
			if (oldIbLoan == null) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, IBLoanDTO.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment.getProperty("exception.message.not.found")
								+ IBLoanDTO.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ ibLoanDTO.getId());
			}
			// mapping new data with existing data (oldIbLoan)
			mapper.map(ibLoanDTO, oldIbLoan);
			// update & persist data in DB
			CommonFunctions.mapperToUpdateWithEnabled(oldIbLoan, userClient, logger);
			IBLoan newIbLoan = loanIbRepository.save(oldIbLoan);

			CustomerDTO customerDTO =
					customerService.findCustomer(ibLoanDTO.getCustomerDTO().getId());

			// update account portfolio code
			customerDTO.setAccountPortfolioCode(newIbLoan.getPortfolioCode());
			// update account portfolio description
			customerDTO.setAccountPortfolioDescription(newIbLoan.getPortfolioDescription());
			// update account portfolio ID
			customerDTO.setAccountPortfolioID(newIbLoan.getPortfolioId());
			// Update portfolio information for the customer
			customerService.save(customerDTO.getId(), customerDTO);

			logger.info("{}", newIbLoan);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, IBLoanDTO.class.getSimpleName());

			// Notification
			String actionDescription = "Loan has been assigned at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ newIbLoan.getUpdatedBy();
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(ibLoanDTO.getOwner(),
							NotificationCategory.LOAN_IB.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_ASSIGN_IB,
							actionDescription, null, null));
			logger.info("New assigned Notification  [{}] has been inserted.", notificationsDTO);
		}
		return ibLoanDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author MoezMhiri
	 * @param ibLoanDTO the ib loan DTO
	 * @param qIbLoan the q ib loan
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(IBLoanDTO ibLoanDTO, QIBLoan qIbLoan) {

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// FIND BY : STATUT TAB
		if (!ACMValidationUtils.isNullOrEmpty(ibLoanDTO.getStatut())
				&& ibLoanDTO.getStatut() == CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.INITIAL_ONLINE_APPLICATION)
						.getKey()) {
			// find by given status
			predicate.and(qIbLoan.statut.eq(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.INITIAL_ONLINE_APPLICATION)
					.getKey()));
			// add conncted user
			List<String> wheresOwners = new ArrayList<>();
			wheresOwners.add(userDTO.getLogin());
			// setting predicate to find by Id
			predicate.and(qIbLoan.owner.in(new ArrayList<>(new HashSet<>(wheresOwners))));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(ibLoanDTO.getStatut())
				&& ibLoanDTO.getStatut() == CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.REJECT_ONLINE_APPLICATION)
						.getKey()) {
			// find loan only for connected user (owner) if statut=-1
			predicate.and(qIbLoan.owner.eq(userDTO.getLogin()));
			predicate.and(qIbLoan.statut.eq(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.REJECT_ONLINE_APPLICATION).getKey()));
		}

		// find loan by customerType
		if (!ACMValidationUtils.isNullOrEmpty(ibLoanDTO.getCustomerDTO())) {
			if (!ACMValidationUtils.isNullOrEmpty(ibLoanDTO.getCustomerDTO().getCustomerType())) {
				predicate.and(qIbLoan.customer.customerType
						.eq(ibLoanDTO.getCustomerDTO().getCustomerType()));
			}
			// find loan by customerAddress
			if (!ACMValidationUtils
					.isNullOrEmpty(ibLoanDTO.getCustomerDTO().getCustomerAddress())) {
				predicate.and(qIbLoan.customer.customerAddress
						.like("%" + ibLoanDTO.getCustomerDTO().getCustomerAddress() + "%"));
			}
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#loadFilterProduct(com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public List<IBLoanDTO> loadFilterProduct(IBLoanDTO ibLoanDTO) {

		// init QLoan
		QIBLoan qIbLoan = QIBLoan.iBLoan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(ibLoanDTO, qIbLoan);

		// SELECT distinct productDescription
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<IBLoan> loansIb =
				queryFactory.select(Projections.bean(IBLoan.class, qIbLoan.productDescription))
						.distinct().from(qIbLoan).where(predicate).fetch();

		// mapping && returning data
		List<IBLoanDTO> loanIbDTOs = new ArrayList<>();
		loansIb.forEach(loanIb -> loanIbDTOs.add(mapper.map(loanIb, IBLoanDTO.class)));
		logger.info("{} : Products to use in Filter dashboard was founded", loanIbDTOs.size());
		return loanIbDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#count()
	 */
	@Override
	public LoanStatutDTO count() {

		// statut 0 : New
		Long statutNew = Long.valueOf(find(new IBLoanDTO(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.INITIAL_ONLINE_APPLICATION).getKey()))
						.size());
		// statut 1 : Accepted
		Long statutAccepted = Long.valueOf(find(new IBLoanDTO(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.ACCEPT_ONLINE_APPLICATION).getKey()))
						.size());
		// statut -1 : Rejected
		Long statutRejected = Long.valueOf(find(new IBLoanDTO(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECT_ONLINE_APPLICATION).getKey()))
						.size());
		LoanStatutDTO loanStatutDTO = new LoanStatutDTO(statutNew, null, null, statutAccepted,
				statutRejected, null, null, null);

		logger.info("Returning STATUT {}", loanStatutDTO);
		return loanStatutDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#loadFilterStatusWorkflow(com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public List<IBLoanDTO> loadFilterStatus(IBLoanDTO ibLoanDTO) {

		// init QLoan
		QIBLoan qIbLoan = QIBLoan.iBLoan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(ibLoanDTO, qIbLoan);
		// SELECT distinct statut
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<IBLoan> ibLoans = queryFactory.select(Projections.bean(IBLoan.class, qIbLoan.statut))
				.distinct().from(qIbLoan).where(predicate).fetch();

		// mapping && returning data
		List<IBLoanDTO> ibLoanDTOs = new ArrayList<>();
		ibLoans.forEach(ibLoan -> ibLoanDTOs.add(mapper.map(ibLoan, IBLoanDTO.class)));
		logger.info("{} : Status to use in Filter dashboard was founded", ibLoanDTOs.size());
		return ibLoanDTOs;
	}

	/**
	 * Creates the loan via API abacus INDIV.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	private LoanDTO createLoanViaAPIAbacusINDIV(LoanDTO loanDTO)
			throws IOException, ApiAbacusException {

		try {
			// Send create Loan INDIV API to ABACUS
			return transversClient.createLoanINDIV(loanDTO);
		}
		catch (Exception e) {
			logger.error("Failed to add loan INDIV {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e != null && e.getMessage() != null && e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
	}

	/**
	 * Creates the loan via API abacus ORG.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	private LoanDTO createLoanViaAPIAbacusORG(LoanDTO loanDTO)
			throws IOException, ApiAbacusException {

		try {
			// Send create Loan ORG API to ABACUS
			return transversClient.createLoanORG(loanDTO);
		}
		catch (Exception e) {
			logger.error("Failed to add loan ORG {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e != null && e.getMessage() != null && e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
	}

	/**
	 * Creates the loan via API abacus GRP.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @return the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	private LoanDTO createLoanViaAPIAbacusGRP(List<LoanDTO> loanDTOs)
			throws IOException, ApiAbacusException {

		try {
			// Send create Loan GRP API to ABACUS
			return transversClient.createLoanGRP(loanDTOs).get(0);
		}
		catch (Exception e) {
			logger.error("Failed to add loan GRP {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e != null && e.getMessage() != null && e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
	}

	/**
	 * Creates the user and send mail to customer.
	 * 
	 * @author HaythemBenizid
	 * @param ibLoanDTO the ib loan DTO
	 * @throws ResourcesNotFoundException the ResourcesNotFoundException
	 */
	private void createUserAndSendMail(IBLoanDTO ibLoanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(ibLoanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(ibLoanDTO.getCustomerDTO(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// create user for GRP members
		if (ibLoanDTO.getCustomerDTO().getCustomerType().equals(CustomerType.GRP.name())) {
			List<CustomerDTO> customerMembers =
					customerService.findCustomersRelationShip(ibLoanDTO.getCustomerDTO());
			for (CustomerDTO newCustomerDTO : customerMembers) {
				// create user IB in DB for the customer member
				UserDTO userDTO = new UserDTO(newCustomerDTO.getCustomerNumber(),
						newCustomerDTO.getFirstName(), newCustomerDTO.getLastName(),
						newCustomerDTO.getEmail(), newCustomerDTO.getBranchId(),
						newCustomerDTO.getBranchesName(), newCustomerDTO.getBranchesDescription(),
						newCustomerDTO.getId());
				UserDTO ibUserDTO = userClient.createForIB(userDTO);
				ibUserDTO.setCustomerId(newCustomerDTO.getId());
				logger.info(" USER IB inserted = {}", ibUserDTO);

				// send notification mail to customer member
				sendMail(new MailCustomerDTO(newCustomerDTO, ibUserDTO.getLogin(),
						ibUserDTO.getPwd() != null ? ibUserDTO.getPwd()
								: "Use your current password.",
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
								(!ACMValidationUtils.isNullOrEmpty(newCustomerDTO.getEmail())
										&& Boolean.TRUE.equals(
												StringUtils.mailIsValid(newCustomerDTO.getEmail())))
														? newCustomerDTO.getEmail()
														: defaultACMReceiverMail,
								"Loan Application Accepted", ""),
						MailBuilderMethod.BUILD_CUSTOMER_APPLICATION_ACCEPTED));
			}
		}
		else {
			// create user IB for INDIV && ORG customer in DB
			UserDTO userDTO = new UserDTO(ibLoanDTO.getCustomerDTO().getCustomerNumber(),
					ibLoanDTO.getCustomerDTO().getFirstName(),
					ibLoanDTO.getCustomerDTO().getLastName(), ibLoanDTO.getCustomerDTO().getEmail(),
					ibLoanDTO.getCustomerDTO().getBranchId(),
					ibLoanDTO.getCustomerDTO().getBranchesName(),
					ibLoanDTO.getCustomerDTO().getBranchesDescription(),
					ibLoanDTO.getCustomerDTO().getIbCustomerId());
			// mapping ib user with created customer
			// TO DO userDTO.setCustomerId(ibLoanDTO.getCustomerDTO().getId());
			// save new user in IB
			userDTO = loadDataIBService.createUser(userDTO);
			logger.info(" USER IB inserted = {}", userDTO);

			// send notification mail to customer
			sendMail(new MailCustomerDTO(ibLoanDTO.getCustomerDTO(), userDTO.getLogin(),
					userDTO.getPwd() != null ? userDTO.getPwd() : "Use your current password.",
					new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
							(!ACMValidationUtils
									.isNullOrEmpty(ibLoanDTO.getCustomerDTO().getEmail())
									&& Boolean.TRUE.equals(StringUtils
											.mailIsValid(ibLoanDTO.getCustomerDTO().getEmail())))
													? ibLoanDTO.getCustomerDTO().getEmail()
													: defaultACMReceiverMail,
							"Loan Application Accepted", ""),
					MailBuilderMethod.BUILD_CUSTOMER_APPLICATION_ACCEPTED));
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
	 * Send mail.
	 * 
	 * @author Salmen Fatnassi
	 * @param mailIBLoanDTO MailIBLoanDTO
	 */
	private void sendMail(MailIBLoanDTO mailIBLoanDTO) {

		try {
			mailSenderClient.sendEmail(mailIBLoanDTO);
			logger.info("Sending Email to customer :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#countByStatutAndBranchIDsAndOwners(java.lang.Integer,
	 * java.util.List, java.util.List)
	 */
	@Override
	public Long countByStatutAndBranchIDsAndOwners(Integer statut, List<Integer> branchIds,
			List<String> owners) {

		Preconditions.checkNotNull(statut, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(branchIds, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("count IBLoan by statut : {} and branchIds = {}", statut, branchIds);
		return loanIbRepository.countByStatutAndEnabledAndBranchIDInOrOwnerInAndStatut(statut,
				Boolean.TRUE, branchIds, owners, statut);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#countByProductId(java.lang.Integer)
	 */
	@Override
	public Long countByProductId(Integer productId) {

		Preconditions.checkNotNull(productId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("count IBLoan by productId : {} ", productId);
		return loanIbRepository.countByProductIdAndEnabled(productId, Boolean.TRUE);
	}

	/**
	 * Update ACM customer.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private void updateACMCustomer(CustomerDTO customerDTO) throws IOException {

		try {

			customerService.save(customerDTO.getId(), customerDTO);
			logger.info("Update customer :: DONE");
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
				userDefinedFieldsLinksService.deleteAllByCustomer(customerDTO.getId());
				// save udfLinks from ABACUS IN ACM
				for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : listUDFCustomer) {
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
		catch (Exception e) {
			logger.error("Failed update acm customer after accepting loan {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IBLoanService#accept(java.lang.Long, com.acm.utils.dtos.IBLoanDTO)
	 */
	@Override
	public IBLoanDTO accept(Long id, IBLoanDTO ibLoanDTO) throws Exception {

		// get current data of IBLoan by ID
		IBLoanDTO currentIBLoanDTO = find(id);
		// find customer in IB
		CustomerDTO ibCustomerDTO = loadDataIBService
				.findCustomerById(currentIBLoanDTO.getCustomerDTO().getIbCustomerId());

		// find customer in acm
		CustomerDTO acmCustomerDTO = new CustomerDTO();
		Boolean isSavedInAbacus = Boolean.FALSE;
		List<CustomerDTO> acmCustomerDTOs = new ArrayList<>();
		if (currentIBLoanDTO.getCustomerDTO().getCustomerIdExtern() != 0) {
			acmCustomerDTOs = customerService.findCustomerIdExtern(
					currentIBLoanDTO.getCustomerDTO().getCustomerIdExtern(),
					Optional.of(Boolean.TRUE));
		}

		if (ACMValidationUtils.isNullOrEmpty(acmCustomerDTOs)) {
			if (ibCustomerDTO.getCustomerLinksRelationshipDTOs() == null) {
				ibCustomerDTO.setCustomerLinksRelationshipDTOs(new ArrayList<>());
			}
			// save customer in acm and abacus
			try {
				acmCustomerDTO = customerService.saveForApplication(ibCustomerDTO);
			}
			catch (CustomerMaxActiveAccountException e) {
				throw new CustomerMaxActiveAccountException(
						new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
								CommonExceptionsMessage.CUSTOMER_MAX_ACTIVE_ACCOUNT + " : "
										+ e.getMessage()),
						CommonExceptionsMessage.CUSTOMER_MAX_ACTIVE_ACCOUNT + " : "
								+ e.getMessage());
			}
			currentIBLoanDTO.setCustomerDTO(acmCustomerDTO);
			isSavedInAbacus = Boolean.TRUE;
		}
		else {
			acmCustomerDTO = acmCustomerDTOs.get(0);
		}
		if (Boolean.FALSE.equals(isSavedInAbacus)) {
			// find customer in abacus
			CustomerDTO abacusCustomerDTO =
					transversClient.findCustomerById(acmCustomerDTO.getCustomerIdExtern());
			if (abacusCustomerDTO.getCustomerIdExtern() == 0) {
				// init list UDF to be deleted from ACM DB
				List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOsToDelete =
						new ArrayList<>();
				// Loading && Setting customer data

				// find UDF by customer ID
				UserDefinedFieldsLinksDTO paramsUDFLinksDTO = new UserDefinedFieldsLinksDTO();
				paramsUDFLinksDTO.setCustomerId(acmCustomerDTO.getId());
				List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs =
						userDefinedFieldsLinksService.find(paramsUDFLinksDTO);
				acmCustomerDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksDTOs);
				// list UDF for INDIV / GRP Parent / ORG Parent
				userDefinedFieldsLinksDTOsToDelete.addAll(userDefinedFieldsLinksDTOs);

				// delete UDF customer from ACM DB by ID
				for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : userDefinedFieldsLinksDTOsToDelete) {
					userDefinedFieldsLinksService.delete(userDefinedFieldsLinksDTO);
				}

				try {
					// CALL API ABACUS TO SAVE CAUSTOMER
					abacusCustomerDTO = transversClient.addCustomer(acmCustomerDTO);
				}
				catch (Exception e) {
					logger.error("Failed to add Customer {}", e.getMessage());
					logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
							e.getMessage());
					// INIT error message
					String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
					if (e.getMessage().contains("errorMessage")) {
						String msgFromTransversApi =
								e.getMessage().substring(e.getMessage().indexOf('{'));
						final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
						messageError = jsonNode.get("errorMessage").asText();
					}
					// Fire Exception
					throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
				}
				// update id extern
				acmCustomerDTO.setCustomerIdExtern(abacusCustomerDTO.getCustomerIdExtern());
				// update number
				acmCustomerDTO.setCustomerNumber(abacusCustomerDTO.getCustomerNumber());
				// update personne ID
				acmCustomerDTO.setPersonIdExtern(abacusCustomerDTO.getPersonIdExtern());
				// enable the customer
				acmCustomerDTO.setIsCustomer(Boolean.TRUE);
				acmCustomerDTO.setUpdateCustomer(Boolean.FALSE);
				acmCustomerDTO.setIbCustomerId(id);
				// update customer in acm
				CustomerDTO updatedCustomerDTO =
						customerService.save(acmCustomerDTO.getId(), acmCustomerDTO);
				updatedCustomerDTO.setListAddress(acmCustomerDTO.getListAddress());
				updatedCustomerDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksDTOs);
				// update customer in IB
				loadDataIBService.update(updatedCustomerDTO);
				// setting customer
				currentIBLoanDTO.setCustomerDTO(updatedCustomerDTO);
			}
		}

		// find product object by ID
		currentIBLoanDTO.setProductDTO(
				parametrageClient.findProductById(currentIBLoanDTO.getProductId().longValue()));

		LoanDTO loanDTO = new LoanDTO(currentIBLoanDTO.getApplyAmountTotal(),
				currentIBLoanDTO.getApplyDate(), currentIBLoanDTO.getGracePeriod(),
				currentIBLoanDTO.getIssueDate(), currentIBLoanDTO.getPortfolioId(),
				currentIBLoanDTO.getProductId(), currentIBLoanDTO.getProductDTO(), 1, new Date(),
				currentIBLoanDTO.getTermPeriodNum(), currentIBLoanDTO.getPaymentFreq(),
				currentIBLoanDTO.getIssueFeeAmount(), currentIBLoanDTO.getProductDTO().getCode(),
				currentIBLoanDTO.getProductDTO().getDescription(),
				currentIBLoanDTO.getCustomerDTO().getCustomerName(),
				currentIBLoanDTO.getPortfolioCode(), currentIBLoanDTO.getPortfolioDescription(),
				currentIBLoanDTO.getCurrencySymbol(), currentIBLoanDTO.getCurrencyDecimalPlaces(),
				currentIBLoanDTO.getOwner(), currentIBLoanDTO.getInitialPaymentDate(),
				currentIBLoanDTO.getNormalPayment(), currentIBLoanDTO.getPeriodsDeferred(),
				currentIBLoanDTO.getTermPeriodID(), currentIBLoanDTO.getBranchID(),
				currentIBLoanDTO.getBranchName(), currentIBLoanDTO.getBranchDescription(),
				currentIBLoanDTO.getCustomerDTO(), currentIBLoanDTO.getAmountWord(),
				currentIBLoanDTO.getLoanType(), currentIBLoanDTO.getApplyAmountTotal());

		// set project description udf
		if (!ACMValidationUtils.isNullOrEmpty(currentIBLoanDTO.getProjectDescription())) {
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs = new ArrayList<>();
			// find UserDefinedFields with code = 'Project Description'
			// UserDefinedFields object
			List<String> names = new ArrayList<>();
			names.add("Project Description");
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
					parametrageClient.find(new UserDefinedFieldsDTO(names));
			// set UDF field "project information"
			if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOs)) {
				// add userDefinedFieldsLinks
				userDefinedFieldsLinksDTOs
						.add(new UserDefinedFieldsLinksDTO(userDefinedFieldsDTOs.get(0), null, null,
								null, currentIBLoanDTO.getProjectDescription()));
			}
			// setting list UDF in loanDTO object
			loanDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksDTOs);
		}
		// default mode REPAYMENT AMOUNT => 0
		loanDTO.setLoanCalculationMode(0);
		// The application is add in the CBS via API (Add API)
		// default value 5 for BRJMF
		loanDTO.setPeriodsDeferredType(5);
		LoanDTO abacusLoanDTO = null;
		Integer maxIssueDate = Integer.parseInt(parametrageClient.find("ISSUE_DATE").getValue());
		Integer dateRepaymentDate =
				Integer.parseInt(parametrageClient.find("INITIAL_PAYMENT").getValue());
		if (maxIssueDate != 0 && dateRepaymentDate != 0) {
			loanDTO.setInitialPaymentDate(Date
					.from(loanService.calculateFirstRepaymentDate(maxIssueDate, dateRepaymentDate)
							.atStartOfDay(ZoneId.systemDefault()).toInstant()));
		}
		if (loanDTO.getCustomerType().equals(CustomerType.INDIV.name())) {
			// create loan in acm and abacus
			// abacusLoanDTO = loanService.saveToAbacus(loanDTO);
			checkMaxActiveAccount(loanDTO);
			// Send create Loan INDIV API to ABACUS
			abacusLoanDTO = transversClient.createLoanINDIV(loanDTO);

			// save to ACM
			abacusLoanDTO.setLoanApplicationStatus("NEW_APPLICATION");
			abacusLoanDTO.setInterestFreq(1);
			abacusLoanDTO = loanService.save(abacusLoanDTO);
			abacusLoanDTO = saveAcmLoanInIB(abacusLoanDTO);

			LoanScheduleDTO loanScheduleDTO =
					new LoanScheduleDTO(abacusLoanDTO, ibLoanDTO.getLoanSchedules());
			loadDataIBService.saveLoanScheduleInIb(loanScheduleDTO);

			// update acm_loan in acm with new ib loan id
			loanService.save(abacusLoanDTO.getLoanId(), abacusLoanDTO);

		}

		// check returned object to complete process on acceptance Application
		if (abacusLoanDTO != null && abacusLoanDTO.getIdLoanExtern() != null) {
			logger.info("{}  has been successfully created in ABACUS with ID extern = [{}].",
					Loan.class.getSimpleName(), abacusLoanDTO.getIdLoanExtern());

			// setting idLoanExtern in IB_LOAN
			currentIBLoanDTO.setIdLoanExtern(abacusLoanDTO.getIdLoanExtern());
			currentIBLoanDTO.setStatut(ibLoanDTO.getStatut());
			currentIBLoanDTO.setAcmIdLoan(abacusLoanDTO.getLoanId());
			currentIBLoanDTO.setAccountNumber(abacusLoanDTO.getAccountNumber());
			// update IB loan data
			IBLoanDTO newIbLoanDTO = save(currentIBLoanDTO.getId(), currentIBLoanDTO);

			// send api to ib : create User && Send Mail to customer
			createUserAndSendMail(newIbLoanDTO);
			return newIbLoanDTO;
		}
		return ibLoanDTO;

	}

	/**
	 * Check max active account.
	 *
	 * @param loanDTO the loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 */
	private void checkMaxActiveAccount(LoanDTO loanDTO)
			throws ApiAbacusException, CustomerMaxActiveAccountException {

		// Get Max Active Account By Product
		Long customerActiveAccount = 0L;
		logger.info("send loan to ABACUS via API");
		try {
			customerActiveAccount = transversClient.findCustomerActiveAccount(
					loanDTO.getCustomerDTO().getCustomerIdExtern(),
					loanDTO.getProductDTO().getProductIdAbacus());
		}
		catch (Exception e) {
			logger.error("Error will calling transvers-service : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
							new TechnicalException()),
					CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
		}
		if (customerActiveAccount >= loanDTO.getProductDTO().getMaxAccounts()) {
			throw new CustomerMaxActiveAccountException(
					new ExceptionResponseMessage(CommonErrorCode.CUSTOMER_LIMIT_MAX_ACTIVE_ACCOUNT,
							CommonExceptionsMessage.CUSTOMER_MAX_ACTIVE_ACCOUNT),
					CommonExceptionsMessage.CUSTOMER_MAX_ACTIVE_ACCOUNT);
		}
	}
}
