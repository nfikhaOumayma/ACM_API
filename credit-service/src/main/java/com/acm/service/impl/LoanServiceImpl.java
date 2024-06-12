/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistoryLoan;
import com.acm.client.CrmClient;
import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CancelIssuedLoanException;
import com.acm.exceptions.type.CheckAppL1NotFoundException;
import com.acm.exceptions.type.CheckAppL2NotFoundException;
import com.acm.exceptions.type.CheckAppL3NotFoundException;
import com.acm.exceptions.type.CheckAppL4NotFoundException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CollateralNotFoundException;
import com.acm.exceptions.type.ConditionalApproveException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerContactException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.FieldVisitNotFoundException;
import com.acm.exceptions.type.GuarantorsNotFoundException;
import com.acm.exceptions.type.InformCustomerNotFoundException;
import com.acm.exceptions.type.InitialCheckException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.UploadSignedDocNotFoundExepction;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.repository.AcmLoanInstanceAcmGroupeApprovalRepository;
import com.acm.repository.AssetLoanRepository;
import com.acm.repository.LoanRepository;
import com.acm.repository.ThirdPartyHistoriqueRepository;
import com.acm.service.AcmLoanInstanceAcmGroupeApprovalService;
import com.acm.service.ChargeFeesService;
import com.acm.service.ConditionalApproveService;
import com.acm.service.CustomerContactService;
import com.acm.service.CustomerDecisionService;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.service.CustomerService;
import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.service.LoanCalendarSyncService;
import com.acm.service.LoanInstanceService;
import com.acm.service.LoanParticipantsService;
import com.acm.service.LoanService;
import com.acm.service.LoanWorkflowUserActionService;
import com.acm.service.MezaCardService;
import com.acm.service.NotificationsServices;
import com.acm.service.SimahClassDetailsService;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.service.api_ib.LoadDataIBService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AccountChildTrn;
import com.acm.utils.dtos.AccountFeeTrn;
import com.acm.utils.dtos.AccountLoanTrn;
import com.acm.utils.dtos.AcmDamagedDataDTO;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AcmLoanInstanceAcmGroupeApprovalDTO;
import com.acm.utils.dtos.AcmMezaCardDTO;
import com.acm.utils.dtos.AcmTemplateSMSDTO;
import com.acm.utils.dtos.ApplicationDetailsDTO;
import com.acm.utils.dtos.AssetLoanDTO;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.ChargeFeeDTO;
import com.acm.utils.dtos.ChargeFeesDTO;
import com.acm.utils.dtos.ConfirmPurchaseOrSaleRequestApiDTO;
import com.acm.utils.dtos.ConfirmPurchaseResponseApiDTO;
import com.acm.utils.dtos.ContactRequestSimahApiDTO;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.DemographicInfoDTO;
import com.acm.utils.dtos.DemographicInfoRequestSimahApiDTO;
import com.acm.utils.dtos.DisburseDTO;
import com.acm.utils.dtos.DisburseResponse;
import com.acm.utils.dtos.EmploymentStatusEntryMasdrAPIDTO;
import com.acm.utils.dtos.EmploymentStatusInfoMasdrAPIDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.IdentityInfoDTO;
import com.acm.utils.dtos.JournalEnteriesInformationDTO;
import com.acm.utils.dtos.JournalEnteriesToAbacusDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanInstanceDTO;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.dtos.MessageDetailsDTO;
import com.acm.utils.dtos.OccupationRequestSimahApiDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiRequestDTO;
import com.acm.utils.dtos.PurchaseMurabhaApiResponseDTO;
import com.acm.utils.dtos.ReportDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingListDTO;
import com.acm.utils.dtos.ReportingListGroupByDTO;
import com.acm.utils.dtos.RequestAddressSimahApiDTO;
import com.acm.utils.dtos.RequestApplicantSimahApiDTO;
import com.acm.utils.dtos.RequestEnquiryNewCustomerSimahApiDTO;
import com.acm.utils.dtos.RequestGetScoreDTO;
import com.acm.utils.dtos.RespInfoDakhliApiDTO;
import com.acm.utils.dtos.ResponseChargeFeeDTO;
import com.acm.utils.dtos.ResponseIncomeDakhliApiDTO;
import com.acm.utils.dtos.SaleMurabhaApiRequestDTO;
import com.acm.utils.dtos.SettingChargeFeeDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingJournalEnteriesDTO;
import com.acm.utils.dtos.SettingJournalEntryTypeDTO;
import com.acm.utils.dtos.Transaction;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.dtos.pagination.LoanPaginationDTO;
import com.acm.utils.enums.CustomerMezaCardStatus;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.MezaCardStatus;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.AcmLoanInstanceAcmGroupeApproval;
import com.acm.utils.models.AssetLoan;
import com.acm.utils.models.Customer;
import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanInstance;
import com.acm.utils.models.QLoan;
import com.acm.utils.models.QLoanParticipants;
import com.acm.utils.models.ThirdPartyHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.StringTemplate;
import com.querydsl.jpa.JPAExpressions;
import com.querydsl.jpa.impl.JPAQueryFactory;
import com.vneuron.utils.dtos.CustomerVneuron;
import com.vneuron.utils.dtos.SearchPersonCustomerResponse;

import feign.FeignException;

/**
 * {@link LoanServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class LoanServiceImpl implements LoanService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanServiceImpl.class);

	/** The loan repository. */
	@Autowired
	private LoanRepository loanRepository;
	/** The asset loan repository. */
	@Autowired
	private AssetLoanRepository assetLoanRepository;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The loan workflow user action service. */
	@Autowired
	private LoanWorkflowUserActionService loanWorkflowUserActionService;

	/** The loan participants service. */
	@Autowired
	private LoanParticipantsService loanParticipantsService;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The customer links relationship service. */
	@Autowired
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/** The loan instance service. */
	@Autowired
	private LoanInstanceService loanInstanceService;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/** The LoanApprovalHistorique service. */
	@Autowired
	private LoanApprovalHistoriqueService loanApprovalHistoriqueService;

	/** The customer contact service. */
	@Autowired
	private CustomerContactService customerContactService;

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

	/** The Constant SUBJECT_MAIL. */
	private static final String SUBJECT_MAIL = "Complete loan application : ";

	/** The meza card service. */
	@Autowired
	private MezaCardService mezaCardService;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The acm loan instance groupe association service. */
	@Autowired
	private AcmLoanInstanceAcmGroupeApprovalService acmLoanInstanceGroupeAssociationService;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The acm loan instance groupe association repository. */
	@Autowired
	private AcmLoanInstanceAcmGroupeApprovalRepository acmLoanInstanceGroupeAssociationRepository;

	/** The conditional approve service. */
	@Autowired
	ConditionalApproveService conditionalApproveService;

	/** The crm client. */
	@Autowired
	private CrmClient crmClient;

	/** The load data IB service. */
	@Autowired
	private LoadDataIBService loadDataIBService;

	/** The reporting client. */
	@Autowired
	private ReportingClient reportingClient;

	/** The class details service. */
	@Autowired
	private SimahClassDetailsService simahClassDetailsService;

	/** The load data ib service. */
	@Autowired
	private LoadDataIBService loadDataIbService;

	/** The third party historique repository. */
	@Autowired
	private ThirdPartyHistoriqueRepository thirdPartyHistoriqueRepository;

	/** The loan historique service. */
	@Autowired
	private CustomerDecisionService customerDecisionService;

	/** The charge fees service. */
	@Autowired
	private ChargeFeesService chargeFeesService;

	/**
	 * Find.
	 *
	 * @param id the id
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#find(java.lang.Integer)
	 */
	@Override
	public LoanDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loan by ID : {}", id);
		Loan loan = loanRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loan)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(loan, LoanDTO.class);
	}

	/**
	 * Find by id extern.
	 *
	 * @param idIbLoan the id ib loan
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByIdIbLoan(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> findByIdIbLoan(Long idIbLoan) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idIbLoan, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loan by ID : {}", idIbLoan);
		List<Loan> loans = loanRepository.findByIdIbLoan(idIbLoan);
		List<LoanDTO> loanDTOs = new ArrayList<>();
		// check if object is null
		if (!ACMValidationUtils.isNullOrEmpty(loans)) {

			loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		}

		return loanDTOs;
	}

	/**
	 * Find by id extern.
	 *
	 * @param idExtern the id extern
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByIdExtern(java.lang.Long)
	 */
	@Override
	public LoanDTO findByIdExtern(Long idExtern) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idExtern, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loan by ID Extern : {}", idExtern);
		// check if loan exist in ACM
		List<Loan> existLoans = loanRepository.findByIdLoanExternAndEnabled(idExtern, Boolean.TRUE);

		// check if object is null
		if (!ACMValidationUtils.isNullOrEmpty(existLoans)) {
			LoanDTO loanDTO = mapper.map(existLoans.get(0), LoanDTO.class);
			return loanDTO;
		}
		else {
			return null;
		}
	}

	/**
	 * Count.
	 *
	 * @param statusTab the status tab
	 * @return the long
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#count(java.lang.String)
	 */
	@Override
	public Long count(String statusTab) {

		logger.info("START calculate COUNT For TAB : {}", statusTab);
		Long count = 0L;
		if (ACMValidationUtils.isNullOrEmpty(statusTab)) {
			return count;
		}
		// INIT QLoan
		QLoan qLoan = QLoan.loan;
		// INIT params
		LoanDTO loanDTO = new LoanDTO(
				"myTask".equals(statusTab) ? 0 : CommonFunctions.mappingStatus(statusTab).getKey(),
				0L);
		// execute query
		count = loanRepository.count(buildQuery(loanDTO, qLoan));
		logger.info("Returning COUNT = {} For TAB : {}", count, statusTab);
		return count;
	}

	/**
	 * Find by id account extern.
	 *
	 * @param cuAccountId the cu account id
	 * @return the loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByIdAccountExtern(java.lang.Long)
	 */
	@Override
	public LoanDTO findByIdAccountExtern(Long cuAccountId) {

		Preconditions.checkNotNull(cuAccountId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loan by cuAccountId : {}", cuAccountId);
		List<Loan> loans =
				loanRepository.findByIdAccountExternAndEnabled(cuAccountId, Boolean.TRUE);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loans)) {
			return null;
		}
		return mapper.map(loans.get(0), LoanDTO.class);
	}

	/**
	 * Find by parent id.
	 *
	 * @param parentId the parent id
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByParentId(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> findByParentId(Long parentId) {

		Preconditions.checkNotNull(parentId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loans by parentId : {}", parentId);
		List<Loan> loans = loanRepository.findByParentIdAndEnabled(parentId, Boolean.TRUE);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loans)) {
			return new ArrayList<>();
		}
		// mapping founded data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("METHOD : findByParentId : {} : Loan was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Find by id customer.
	 *
	 * @param customerId the customer id
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByIdCustomer(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> findByIdCustomer(Long customerId) {

		Preconditions.checkNotNull(customerId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loan by customer : {}", customerId);
		List<Loan> loans =
				loanRepository.findByCustomerAndEnabled(new Customer(customerId), Boolean.TRUE);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loans)) {
			return new ArrayList<>();
		}
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("METHOD : findByIdCustomer : {} : Loan was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Find by statut workflow.
	 *
	 * @param statutWorkflow the statut workflow
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByIdAccountExtern(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> findByStatutWorkflow(Integer statutWorkflow) {

		Preconditions.checkNotNull(statutWorkflow, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Loans by statutWorkflow : {}", statutWorkflow);
		List<Loan> loans =
				loanRepository.findByStatutWorkflowAndEnabled(statutWorkflow, Boolean.TRUE);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loans)) {
			return new ArrayList<>();
		}
		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("METHOD : findByStatutWorkflow : {} : Loan was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Find.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#find(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> find(LoanDTO loanDTO) {

		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(loanDTO, qLoan);

		// find data
		Iterable<Loan> iterable = loanRepository.findAll(predicate);
		List<Loan> loans = new ArrayList<>();
		iterable.forEach(loans::add);
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> {
			LoanDTO loanResultat = mapper.map(loan, LoanDTO.class);
			// find product object by ID
			loanResultat.setProductDTO(
					parametrageClient.findProductById(loanResultat.getProductId().longValue()));
			loanDTOs.add(loanResultat);
		});

		logger.info("METHOD : find : {} : Loan was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Find.
	 *
	 * @param loanPaginationDTO the loan pagination DTO
	 * @return the loan pagination DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#find(com.acm.utils.dtos.pagination. LoanPaginationDTO)
	 */
	@Override
	public LoanPaginationDTO find(LoanPaginationDTO loanPaginationDTO) {

		Preconditions.checkNotNull(loanPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getPageNumber())) {
			loanPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getPageSize())) {
			loanPaginationDTO.setPageSize(10);
		}
		// setting default data
		loanPaginationDTO.setResultsLoans(new ArrayList<>());
		// setting default totals pages
		loanPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		loanPaginationDTO.setTotalPages(0);
		// setting default totalAmount
		loanPaginationDTO.setTotalAmount(BigDecimal.ZERO);
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(loanPaginationDTO.getParams(), qLoan);

		// setting pagination filter
		// find loan by accountNumber
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getAccountNumber())) {
			predicate.and(qLoan.accountNumberExtern
					.like("%" + loanPaginationDTO.getParams().getAccountNumber() + "%"));
		}

		// loanParams.productDescription
		if (!ACMValidationUtils
				.isNullOrEmpty(loanPaginationDTO.getParams().getProductDescription())) {
			predicate.and(qLoan.productDescription
					.like("%" + loanPaginationDTO.getParams().getProductDescription() + "%"));
		}

		// loanParams.customerName
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getCustomerName())) {
			List<Long> wheresIdCustomer = new ArrayList<>();
			CustomerDTO params = new CustomerDTO();
			params.setCustomerNumber(loanPaginationDTO.getParams().getCustomerName());
			List<CustomerDTO> customerDTOs = customerService.find(params);
			customerDTOs.forEach(customer -> wheresIdCustomer.add(customer.getId()));
			// find like CUSTOMERNAME OR like CUSTOMERNUMBER
			if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
				predicate.and(qLoan.customer.id.in(wheresIdCustomer));
			}
			else {
				StringTemplate convertedCustomerName = Expressions
						.stringTemplate("function('replace', {0}, '|', ' ')", qLoan.customerName);
				predicate.and(convertedCustomerName
						.like("%" + loanPaginationDTO.getParams().getCustomerName() + "%"));
			}
		}

		// loanParams.applyAmountTotal
		if (!ACMValidationUtils
				.isNullOrEmpty(loanPaginationDTO.getParams().getApplyAmountTotal())) {
			predicate.and(
					qLoan.applyAmountTotal.eq(loanPaginationDTO.getParams().getApplyAmountTotal()));
		}

		// loanParams.applyDate
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getApplyDate())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(DateUtil
					.convertToLocalDateViaInstant(loanPaginationDTO.getParams().getApplyDate()));
			predicate.and(qLoan.applyDate.eq(sqlDate));
		}

		// loanParams.portfolioDescription
		if (!ACMValidationUtils
				.isNullOrEmpty(loanPaginationDTO.getParams().getPortfolioDescription())) {
			predicate.and(qLoan.portfolioDescription
					.like("%" + loanPaginationDTO.getParams().getPortfolioDescription() + "%"));
		}

		// loanParams.owner
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getOwner())) {
			predicate.and(qLoan.owner.like("%" + loanPaginationDTO.getParams().getOwner() + "%"));
		}

		// loanParams.ownerName
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getOwnerName())) {
			predicate.and(
					qLoan.ownerName.like("%" + loanPaginationDTO.getParams().getOwnerName() + "%"));
		}

		// loanParams.groupOwner
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getGroupOwner())) {
			predicate.and(qLoan.groupOwner
					.like("%" + loanPaginationDTO.getParams().getGroupOwner() + "%"));
		}

		// loanParams.groupOwnerName
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getGroupOwnerName())) {
			predicate.and(qLoan.groupOwnerName
					.like("%" + loanPaginationDTO.getParams().getGroupOwnerName() + "%"));
		}

		// loanParams.statutWorkflow
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getStatutWorkflow())) {
			predicate.and(
					qLoan.statutWorkflow.eq(loanPaginationDTO.getParams().getStatutWorkflow()));
		}

		// loanParams.statutLibelle
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getStatutLibelle())) {
			predicate.and(qLoan.statutLibelle.eq(loanPaginationDTO.getParams().getStatutLibelle()));
		}

		// loanParams.dateLastUpdate
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getDateLastUpdate())) {
			Timestamp StartTimesTamp = DateUtil
					.dateToDateTime(loanPaginationDTO.getParams().getDateLastUpdate(), "00:00:01");
			Timestamp EndTimesTamp = DateUtil
					.dateToDateTime(loanPaginationDTO.getParams().getDateLastUpdate(), "23:59:59");
			predicate.and(qLoan.dateLastUpdate.between(StartTimesTamp, EndTimesTamp));
		}
		// loanParams.branchName
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getBranchName())) {
			predicate.and(qLoan.branchName.eq(loanPaginationDTO.getParams().getBranchName()));
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(loanPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getSortField())) {
			// NB : le test est fait sur le field "accountNumber" puisque le nom de field
			// n'est le
			// meme dans LoanDTO <=> Loan
			// customerNameNoPipe => customerName
			String sortedField = loanPaginationDTO.getSortField();
			if (loanPaginationDTO.getSortField().equals("accountNumber")) {
				sortedField = "accountNumberExtern";
			}
			if (loanPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(loanPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getSortField())) {

			String sortedField = loanPaginationDTO.getSortField();
			if (loanPaginationDTO.getSortField().equals("accountNumber")) {
				sortedField = "accountNumberExtern";
			}
			if (loanPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by applyDate : DESC
			pageable = PageRequest.of(loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateLastUpdate");
		}

		// load data
		Page<Loan> pagedResult = loanRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Loan> loans = pagedResult.getContent();
			logger.info(
					"find by pagination method : {} : Loan was founded (PageNumber = {} / PageSize = {} )",
					loans.size(), loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize());
			List<LoanDTO> loanDTOs = new ArrayList<>();
			loans.forEach(loan -> {
				LoanDTO loanResultat = mapper.map(loan, LoanDTO.class);
				// find product object by ID
				loanResultat.setProductDTO(
						parametrageClient.findProductById(loanResultat.getProductId().longValue()));
				loanDTOs.add(loanResultat);
			});
			// setting data
			loanPaginationDTO.setResultsLoans(loanDTOs);
			// setting totals pages
			loanPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			loanPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		// load total amount of all loans in the selected Tab
		JPAQueryFactory query = new JPAQueryFactory(entityManager);
		BigDecimal total = query.select(qLoan.applyAmountTotal.sum().as("somme")).from(qLoan)
				.where(predicate).fetchFirst();
		loanPaginationDTO
				.setTotalAmount(ACMValidationUtils.isNullOrEmpty(total) ? BigDecimal.ZERO : total);
		return loanPaginationDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param qLoan the q loan
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(LoanDTO loanDTO, QLoan qLoan) {

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// FIND BY : STATUT TAB
		// TODO : Change to filter by text !
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getStatut()) && loanDTO.getStatut() != 0) {

			predicate.and(qLoan.statut.eq(loanDTO.getStatut()));

			// find all user responsable && collaborator
			List<String> wheresOwners = new ArrayList<>();
			List<UserDTO> userDTOs = userClient.findUsers();
			userDTOs.forEach(user -> {
				if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
					wheresOwners.add(user.getLogin());
				}
			});
			BooleanBuilder subOwnerPredicate = new BooleanBuilder();
			// setting predicate to find by Id
			subOwnerPredicate.and(qLoan.owner.isNull());
			subOwnerPredicate.and(qLoan.branchID.eq(userDTO.getBranchID()));
			if (userDTO.getCategory() != null
					&& userDTO.getCategory().equals(UserCategory.OPERATION.name())) {
				subOwnerPredicate.or(qLoan.owner.in(new ArrayList<>(new HashSet<>(wheresOwners))));
			}
			predicate.and(subOwnerPredicate);
			// find loan by Access Branches for connected user
			if (userDTO.getCategory() != null
					&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
					&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
				int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(","))
						.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
				List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
				for (int i : arrayBranchIds) {
					listBranchIds.add(Integer.valueOf(i));
				}
				// setting predicate to find by given branch Id
				BooleanBuilder subPredicate = new BooleanBuilder();
				subPredicate.and(qLoan.branchID.in(listBranchIds));
				// find by given status

				subPredicate.and(qLoan.statut.eq(loanDTO.getStatut()));
				predicate.or(subPredicate);
				// if the connected user belong to(BRANCH_OPERATION/ CENTRAL_REVISION/
				// RISK_MANAGER
				// /COMPLIANCE_GROUP) the unassigned loans should not appears on dashboard
				// GroupeDTO groupeDTO = userDTO.getGroupes().iterator().next();
				// if (!ACMValidationUtils.isNullOrEmpty(groupeDTO)
				// && !ACMValidationUtils.isNullOrEmpty(groupeDTO.getCode())) {
				// BooleanBuilder secondSubPredicate = buildGroupeSubPredicate(groupeDTO,
				// qLoan);
				// predicate.and(secondSubPredicate);
				// }
			}
			else {
				// load loan by branch ID for SUPERVISOR
				boolean isManagerBranch = userDTOs.stream().anyMatch(user -> user.getTypeUser()
						.equals(UserHierarchicalType.COLLABORATORS.name()));
				if (isManagerBranch) {
					// setting predicate to find by given branch Id
					BooleanBuilder subPredicate = new BooleanBuilder();
					subPredicate.and(qLoan.branchID.eq(userDTO.getBranchID()));
					// find by given status
					subPredicate.and(qLoan.statut.eq(loanDTO.getStatut()));
					predicate.or(subPredicate);
				}
				// setting subPredicate to filter list participant by Id & status
				BooleanBuilder subPredicate = new BooleanBuilder();
				QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;
				List<LoanParticipantsDTO> loanParticipantsDTOs = loanParticipantsService
						.find(new LoanParticipantsDTO(null, userDTO.getLogin()));

				if (!ACMValidationUtils.isNullOrEmpty(loanParticipantsDTOs)
						&& loanParticipantsDTOs.size() <= 1000) {
					List<Long> wheresIds = new ArrayList<>();
					loanParticipantsDTOs.forEach(
							loanParticipantsDTO -> wheresIds.add(loanParticipantsDTO.getIdLoan()));
					subPredicate.and(qLoan.idLoan.in(new ArrayList<>(new HashSet<>(wheresIds))));
					subPredicate.and(qLoan.statut.eq(loanDTO.getStatut()));
					predicate.or(subPredicate);
				}
				else if (!ACMValidationUtils.isNullOrEmpty(loanParticipantsDTOs)
						&& loanParticipantsDTOs.size() > 1000) {
					subPredicate.and(qLoan.idLoan.in(JPAExpressions.selectFrom(qLoanParticipants)
							.select(qLoanParticipants.idLoan)
							.where(qLoanParticipants.username.eq(userDTO.getLogin()))));
					subPredicate.and(qLoan.statut.eq(loanDTO.getStatut()));
					predicate.or(subPredicate);
				}
			}
		}
		else if (ACMValidationUtils.isNullOrEmpty(loanDTO.getStatut())
				|| (!ACMValidationUtils.isNullOrEmpty(loanDTO.getStatut())
						&& loanDTO.getStatut() == 0)) {

			// find loan only for connected user (owner) if statut=0

			BooleanBuilder subOwnerPredicate = new BooleanBuilder();
			subOwnerPredicate.and(qLoan.owner.eq(userDTO.getLogin()));

			// Get the Loan assigned to connected user on the Association table
			List<Long> loanIds = acmLoanInstanceGroupeAssociationRepository
					.findAssaingedApprovalLoansByOwner(userDTO.getLogin());
			if (!ACMValidationUtils.isNullOrEmpty(loanIds)) {
				subOwnerPredicate.or(qLoan.idLoan.in(loanIds));
				// subOwnerPredicate.or(qLoan.idLoan.in(
				// loanIds.stream().map(BigInteger::longValue).collect(Collectors.toList())));
			}

			predicate.and(subOwnerPredicate);

			// for the TAB "My Tasks" Excluded statutWorkflow :
			// REJECTED / CANCELLED or DECLINE and exclude finished loans
			predicate.and(qLoan.workflowCompleted.isNull());
			predicate.and(qLoan.statutWorkflow.ne(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()));
			predicate.and(qLoan.statutWorkflow.ne(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey()));
			predicate.and(qLoan.statutWorkflow.ne(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey()));
			predicate.and(qLoan.statutWorkflow
					.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey()));
		}

		// find loan by IdLoanExtern
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getIdLoanExtern())) {
			predicate.and(qLoan.idLoanExtern.eq(loanDTO.getIdLoanExtern()));
		}
		// find loan by customerType
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerType())) {
			predicate.and(qLoan.customerType.eq(loanDTO.getCustomerType()));
		}
		// find loan by parentId
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getParentId())) {
			predicate.and(qLoan.parentId.eq(loanDTO.getParentId()));
		}
		// find loan by customerIdExtern
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerId())) {
			predicate.and(qLoan.customerId.eq(loanDTO.getCustomerId()));
		}
		// find loan by customerId
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getId())) {
			predicate.and(qLoan.customer.eq(new Customer(loanDTO.getCustomerDTO().getId())));
		}

		// find loan by accountNumber
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getAccountNumber())) {
			predicate.and(qLoan.accountNumberExtern.like(loanDTO.getAccountNumber() + "%"));
		}

		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));
		logger.info("*** Predicate = {}", predicate);
		return predicate;
	}

	/**
	 * Builds the groupe sub predicate.
	 *
	 * @author idridi
	 * @param groupeDTO the groupe DTO
	 * @param qLoan the q loan
	 * @return the boolean builder
	 */
	private BooleanBuilder buildGroupeSubPredicate(GroupeDTO groupeDTO, QLoan qLoan) {

		BooleanBuilder secondSubPredicate = new BooleanBuilder();
		BooleanBuilder thirdSubPredicate = new BooleanBuilder();
		List<Integer> listStatuts = Arrays.asList(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey());
		// find loans not affected to groups
		if (groupeDTO.getCode().equals(CommonConstants.BRANCH_OPERATION)) {
			secondSubPredicate.or(qLoan.groupOwner.ne(CommonConstants.BRANCH_OPERATION));
			thirdSubPredicate.and(qLoan.groupOwner.eq(CommonConstants.BRANCH_OPERATION));
		}
		else if (groupeDTO.getCode().equals(CommonConstants.CENTRAL_REVISION)) {
			secondSubPredicate.or(qLoan.groupOwner.ne(CommonConstants.CENTRAL_REVISION));
			thirdSubPredicate.and(qLoan.groupOwner.eq(CommonConstants.CENTRAL_REVISION));
		}
		else if (groupeDTO.getCode().equals(CommonConstants.RISK_MANAGER)) {
			secondSubPredicate.or(qLoan.groupOwner.ne(CommonConstants.RISK_MANAGER));
			thirdSubPredicate.and(qLoan.groupOwner.eq(CommonConstants.RISK_MANAGER));
		}
		else if (groupeDTO.getCode().equals(CommonConstants.COMPLIANCE_GROUP)) {
			secondSubPredicate.or(qLoan.groupOwner.ne(CommonConstants.COMPLIANCE_GROUP));
			thirdSubPredicate.and(qLoan.groupOwner.eq(CommonConstants.COMPLIANCE_GROUP));
		}
		else if (groupeDTO.getCode().equals(CommonConstants.BRANCH_AUDITOR)) {
			secondSubPredicate.or(qLoan.groupOwner.ne(CommonConstants.BRANCH_AUDITOR));
			thirdSubPredicate.and(qLoan.groupOwner.eq(CommonConstants.BRANCH_AUDITOR));
		}
		thirdSubPredicate.and(qLoan.statutWorkflow.in(listStatuts));
		secondSubPredicate.or(qLoan.groupOwner.isNull());
		secondSubPredicate.or(thirdSubPredicate);
		return secondSubPredicate;
	}

	/**
	 * Save.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#save(com.acm.utils.dtos.LoanDTO)
	 */
	@ProcessHistoryLoan(action = CommonAOPConstants.SAVE_LOAN)
	@Override

	public LoanDTO save(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// SET par default TRUE
		loanDTO.setAssignCustomer(Boolean.FALSE);
		/*
		 * // check if loan exist in ACM List<Loan> existLoans = loanRepository
		 * .findByIdLoanExternAndEnabled(loanDTO.getIdLoanExtern(), Boolean.TRUE); if
		 * (!ACMValidationUtils.isNullOrEmpty(existLoans)) {
		 * logger.warn("LOAN with the given ID ABACUS = {} exist in ACM DB.",
		 * loanDTO.getIdLoanExtern()); // update limite if loan exit
		 * parametrageClient.updateLimite("LIMITE_ID_LOAN_EXTERNE",
		 * String.valueOf(loanDTO.getIdLoanExtern())); return null; }
		 */

		// setting customer (by customer id extern)
		loanDTO.setCustomerDTO(loadCustomer(loanDTO.getCustomerId()));

		loanDTO.setApprovelAmount(loanDTO.getApplyAmountTotal());
		loanDTO.setUpdateLoan(Boolean.FALSE);
		// setting process name
		loanDTO.setProcessName(ACMConstantWorkflowStatuts.PROCESS_VALIDATION_LOAN_TAMKEEN);

		// INIT STATUS && LOAD PROCCESS FOR GIVEN LOAN BY GIVEN BPMN PROCESS.
		// find workflow process
		// List<SettingStatutWorkflowDTO> settingStatutWorkflowDTOs = parametrageClient
		// .findLoanProcess(new SettingStatutWorkflowDTO(loanDTO,
		// CommonConstants.APP_CLIENT,
		// ACMConstantWorkflowStatuts.PROCESS_VALIDATION_LOAN_TAMKEEN));
		//
		// if (ACMValidationUtils.isNullOrEmpty(settingStatutWorkflowDTOs)) {
		// logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
		// SettingStatutWorkflow.class.getSimpleName());
		// throw new
		// ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
		// CommonExceptionsMessage.NOT_FOUND +
		// SettingStatutWorkflow.class.getSimpleName()
		// + "for process name : "
		// + ACMConstantWorkflowStatuts.PROCESS_VALIDATION_LOAN_TAMKEEN
		// + " and for client : " + CommonConstants.APP_CLIENT);
		// }

		List<WorkFlowStepDTO> workFlowStepDTOs = findSettingWFSteps(loanDTO);

		if (ACMValidationUtils.isNullOrEmpty(workFlowStepDTOs)) {
			logger.error("Workflow Setting Not Found for product with id :{} , and amount :{}",
					loanDTO.getProductId(), loanDTO.getApprovelAmount());
		}
		// setting Statut (get(0) puisque la liste est deja ordonne)
		loanDTO.setStatutWorkflow(workFlowStepDTOs.get(0).getIdWorkFlowStep().intValue());
		loanDTO.setStatutLibelle(workFlowStepDTOs.get(0).getStepName());
		loanDTO.setEtapeWorkflow(workFlowStepDTOs.get(0).getIdWorkFlowStep().intValue());
		loanDTO.setStatut(workFlowStepDTOs.get(0).getCodeStatutLoan().intValue());
		loanDTO.setDateLastUpdate(new Date());
		// mapping data
		Loan loan = mapper.map(loanDTO, Loan.class);
		CommonFunctions.mapperToSave(loan, userClient, logger);

		loan.setCategory(CommonConstants.CATEGORY_INSTANCE);
		loan.setApprovelAmount(loanDTO.getApplyAmountTotal());

		// CHECK COMMUNITYCULOANID AND SET IN PARENTID OF MEMBER LOAN BY IDLOAN OF LOAN
		// PARENT
		if (loanDTO.getCommunityCULoanID() != null && loanDTO.getCommunityCULoanID() != 0) {
			List<Loan> loanCommunityParent = loanRepository
					.findByIdLoanExternAndEnabled(loanDTO.getCommunityCULoanID(), Boolean.TRUE);
			if (ACMValidationUtils.isNullOrEmpty(loanCommunityParent)) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
								+ " with ID CommunityCULoan " + loanDTO.getCommunityCULoanID());
			}
			loan.setParentId(loanCommunityParent.get(0).getIdLoan());
		}
		else {
			loan.setParentId(0L);
		}

		// insert LOAN
		Loan newLoan = loanRepository.save(loan);
		// insert LoanAssest

		List<AssetLoanDTO> loanAssetDtos = loanDTO.getLoanAssetsDtos();
		if (!ACMValidationUtils.isNullOrEmpty(loanAssetDtos)) {
			loanAssetDtos.forEach(assetDTO -> {
				assetDTO.setIdLoan(newLoan.getIdLoan());
				assetLoanRepository.save(mapper.map(assetDTO, AssetLoan.class));
			});
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Loan.class.getSimpleName());
		LoanDTO newLoanDTO = mapper.map(newLoan, LoanDTO.class);

		// setting members
		if (!loanDTO.getCustomerDTO().getCustomerType().equals(CustomerType.INDIV.name())) {
			loadAndSettingMembers(loanDTO.getCustomerDTO(), newLoanDTO);
		}
		// INIT LOAN PARTICIPANTS
		LoanParticipantsDTO loanParticipantsDTO = loanParticipantsService
				.save(new LoanParticipantsDTO(newLoanDTO.getLoanId(), newLoanDTO.getOwner()));

		logger.info("init Loan Participants with ID = [{}] :: DONE", loanParticipantsDTO.getId());

		// // INIT LOAN WORKFLOW
		// try {
		// init(newLoanDTO, ACMConstantWorkflowStatuts.PROCESS_VALIDATION_LOAN_TAMKEEN);
		// }
		// catch (Exception e) {
		// logger.error("Error has been occurred => Failed to lunch workflow");
		// throw new CreditException("WORKFLOW_ERROR", "Failed to lunch workflow");
		// }

		// INIT LOAN PROCESS
		newLoanDTO.setLoanInstancesDtos(initProcessWorkflow(newLoanDTO, workFlowStepDTOs));

		// saving or updating UDF data in DB if not exist
		userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
				loanDTO.getUserDefinedFieldsLinksDTOs(), newLoanDTO.getLoanId(),
				CommonConstants.LOAN_CATEGORY);
		userDefinedFieldsLinksService.updateAbacusUdfLinksByElementId(newLoanDTO.getLoanId(),
				CommonConstants.LOAN_CATEGORY, newLoanDTO.getIdLoanExtern(), newLoanDTO);

		// assign loan to user
		newLoanDTO = assginLoanToOwner(newLoanDTO.getLoanInstancesDtos().get(0),
				workFlowStepDTOs.get(0), newLoanDTO, newLoanDTO.getLoanInstancesDtos());
		// update loan
		newLoanDTO = save(newLoanDTO.getLoanId(), newLoanDTO);

		// newLoanDTO = automaticStepLoan(newLoanDTO);
		return newLoanDTO;
	}

	/**
	 * Save or update or delete UDF loan.
	 *
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private List<UserDefinedFieldsLinksDTO> saveOrUpdateOrDeleteUDFLoan(LoanDTO loanDTO)
			throws ResourcesNotFoundException {

		// Loading list UDF Loan from ABACUS DB
		List<UserDefinedFieldsLinksDTO> listUDFLoan = transversClient.loadUDFByLoan(
				loanDTO.getIdAccountExtern() != null ? loanDTO.getIdAccountExtern() : 0L);
		logger.info("List UDFs Loan size = {}", listUDFLoan.size());
		// saving or updating UDF data in DB if not exist
		if (!ACMValidationUtils.isNullOrEmpty(listUDFLoan)) {
			// find UDF Loan From ACM
			ArrayList<UserDefinedFieldsDTO> listUDFs = new ArrayList<UserDefinedFieldsDTO>();
			listUDFLoan.forEach(udfLink -> listUDFs.add(udfLink.getUserDefinedFieldsDTO()));
			logger.info("List UDFs customer size = {}", listUDFs.size());
			// find && setting udf field object
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
					parametrageClient.findUDFFieldByListIds(listUDFs);
			logger.info("List UDFs Loan size = {}", userDefinedFieldsDTOs.size());
			// delete all udfLinks by customer Id from ACM_UDF_LINKS
			userDefinedFieldsLinksService.deleteAllByLoan(loanDTO.getLoanId());
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : listUDFLoan) {
				for (UserDefinedFieldsDTO udfDTO : userDefinedFieldsDTOs) {
					if (userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getIdUDFField()
							.equals(udfDTO.getIdUDFField())) {
						userDefinedFieldsLinksDTO.setUserDefinedFieldsDTO(udfDTO);
						// if loanDTO is Topup or Refinance and action is 'Save Loan' then create
						// udfLink for loanDTO same as the original loan's udfLink
						if (loanDTO.getLoanApplicationStatus() != null
								&& (loanDTO.getLoanApplicationStatus().equals(CommonConstants.TOPUP)
										|| loanDTO.getLoanApplicationStatus()
												.equals(CommonConstants.REFINANCE))) {
							userDefinedFieldsLinksDTO.setLoanId(loanDTO.getLoanId());
							userDefinedFieldsLinksDTO = userDefinedFieldsLinksService
									.saveForTopup(userDefinedFieldsLinksDTO);

						}
						else {
							userDefinedFieldsLinksDTO = userDefinedFieldsLinksService
									.saveByBatch(userDefinedFieldsLinksDTO);

						}

					}

				}

			}

		}
		return listUDFLoan;

	}

	/**
	 * Inits the process workflow.
	 *
	 * @param loanDTO the loan DTO
	 * @param workFlowStepDTOs the work flow step DT os
	 * @return the list
	 */
	private List<LoanInstanceDTO> initProcessWorkflow(LoanDTO loanDTO,
			List<WorkFlowStepDTO> workFlowStepDTOs) {

		List<LoanInstanceDTO> loanInstanceDTOs = new ArrayList<>();
		for (int i = 0; i < workFlowStepDTOs.size(); i++) {

			WorkFlowStepDTO workFlowStepDTO = workFlowStepDTOs.get(i);
			LoanInstanceDTO newLoanInstanceDTO = loanInstanceService.save(new LoanInstanceDTO(
					loanDTO.getLoanId(), workFlowStepDTOs.get(i).getIdWorkFlowStep().intValue(),
					workFlowStepDTO.getStepName(), "", workFlowStepDTO.getCodeStatutLoan(),
					workFlowStepDTO.getProcess(), workFlowStepDTO.getScreen(), null, null, i, null,
					null, workFlowStepDTO.getEnabled(), workFlowStepDTO.getIbScreen()));

			loanInstanceDTOs.add(newLoanInstanceDTO);
			logger.debug("{}", newLoanInstanceDTO);
			logger.info(
					"Loan process for loan with AccountNumber = [{}] was successfully inserted :: DONE",
					loanDTO.getAccountNumber());

			if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getApprovers())) {
				for (int k = 0; k < workFlowStepDTO.getApprovers().size(); k++) {

					AcmLoanInstanceAcmGroupeApprovalDTO loanInsGp =
							new AcmLoanInstanceAcmGroupeApprovalDTO();

					GroupeDTO groupeDTO = parametrageClient
							.findGroupeByCode(workFlowStepDTO.getApprovers().get(k).getCode());
					// get the list of users that have loanBranch in their accessBranch
					List<UserDTO> userDTOParam =
							userClient.getUsersWithLoanBranchInTheirAccessBranches(
									loanDTO.getBranchID(), groupeDTO.getUserDTOs());
					// if the group has only one user(base on access branch)
					// then assign to this user
					if (userDTOParam.size() == 1) {
						loanInsGp.setOwner(userDTOParam.get(0).getLogin());
						loanInsGp.setOwnerName(userDTOParam.get(0).getSimpleName());
					}

					else {
						loanInsGp.setOwner(null);
						loanInsGp.setOwnerName(null);
					}

					loanInsGp.setLoanInstance(newLoanInstanceDTO);
					loanInsGp.setGroupe(groupeDTO);
					loanInsGp.setGroupeName(groupeDTO.getLibelle());
					loanInsGp.setGroupeCode(groupeDTO.getCode());
					loanInsGp.setValidation(false);
					acmLoanInstanceGroupeAssociationService.save(loanInsGp);
				}
			}
		}
		return loanInstanceDTOs;
	}

	/**
	 * Load customer by given ID. if exist in ACM DB (table Customer) => returning founded data.
	 * ELSE get all data by ID from ABACUS DB and inserting the new customer in ACM DB
	 *
	 * @author HaythemBenizid
	 * @param customerIdExtern the customer id extern
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	private CustomerDTO loadCustomer(Long customerIdExtern)
			throws ResourcesNotFoundException, CalculateAgeException {

		Preconditions.checkNotNull(customerIdExtern, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// check if customer if exist in ACM DB by customerIdExtern
		List<CustomerDTO> customerDTOs =
				customerService.findCustomerIdExtern(customerIdExtern, null);
		if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
			return customerDTOs.get(0);
		}
		else {
			// load Customer from ABACUS Data
			CustomerDTO customerDTOAbacus = transversClient.findCustomerById(customerIdExtern);
			logger.info("{}", customerDTOAbacus);
			if (ACMValidationUtils.isNullOrEmpty(customerDTOAbacus)
					|| ACMValidationUtils.isNullOrEmpty(customerDTOAbacus.getCustomerIdExtern())) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + Customer.class.getSimpleName()
								+ CommonExceptionsMessage.WITH_ID + customerIdExtern);
			}
			// setting CustomerName
			if (customerDTOAbacus.getCustomerType().equals(CustomerType.GRP.name())) {
				customerDTOAbacus.setCustomerName(customerDTOAbacus.getCorrespondanceName());
				customerDTOAbacus.setSolidarityName(customerDTOAbacus.getCorrespondanceName());
			}
			else if (customerDTOAbacus.getCustomerType().equals(CustomerType.ORG.name())) {
				customerDTOAbacus.setCustomerName(customerDTOAbacus.getCorrespondanceName());
				customerDTOAbacus.setOrganizationName(customerDTOAbacus.getCorrespondanceName());
			}
			// insert customer in ACM DB
			CustomerDTO newCustomerDTO = customerService.save(customerDTOAbacus);
			logger.info("new customer with ID = {} / NUMBER = {} has been add in ACM.",
					newCustomerDTO.getId(), newCustomerDTO.getCustomerNumber());
			return newCustomerDTO;
		}
	}

	/**
	 * Load and setting members.
	 *
	 * @author HaythemBenizid
	 * @param customerDTO the customer DTO
	 * @param loanDTO the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	private void loadAndSettingMembers(CustomerDTO customerDTO, LoanDTO loanDTO)
			throws ResourcesNotFoundException, CalculateAgeException {

		// init list
		List<CustomerMemberDTO> customerMemberDTOs = new ArrayList<>();

		// setting members if customer type is GRP or ORG
		if (customerDTO.getCustomerType().equals(CustomerType.GRP.name())) {
			// load members if customer type = GRP
			customerMemberDTOs =
					transversClient.findMembersGroupByCustomer(customerDTO.getCustomerIdExtern());
		}
		else if (customerDTO.getCustomerType().equals(CustomerType.ORG.name())) {
			// load members if customer type = ORG
			customerMemberDTOs = transversClient
					.findMembersOrganisationByCustomer(customerDTO.getCustomerIdExtern());
		}

		// inserting group members
		for (CustomerMemberDTO customerMemberDTO : customerMemberDTOs) {
			// check member if exist before saving link
			CustomerDTO member = loadCustomer(customerMemberDTO.getCustomerId());

			// check link customer members if exist
			CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
			params.setCustomerId(customerDTO.getId());
			params.setMember(member);
			params.setCategory(LinkRelationshipsCategory.MEMBERS.name());
			List<CustomerLinksRelationshipDTO> existMembre =
					customerLinksRelationshipService.find(params);

			// check if member is not registered
			if (ACMValidationUtils.isNullOrEmpty(existMembre)) {
				// saving member in DB
				CustomerLinksRelationshipDTO newMember = customerLinksRelationshipService
						.save(new CustomerLinksRelationshipDTO(customerDTO.getId(), member, null,
								LinkRelationshipsCategory.MEMBERS.name(), new Date(), null, null));
				logger.info("newMember = {}", newMember);
			}

			// saving link in DB
			CustomerLinksRelationshipDTO newLink =
					customerLinksRelationshipService.save(new CustomerLinksRelationshipDTO(
							customerDTO.getId(), member, customerMemberDTO.getCustomerRole(),
							LinkRelationshipsCategory.LINK.name(), new Date(), loanDTO.getLoanId(),
							existMembre.get(0).getPercentageOwned()));
			logger.info("newLink = {}", newLink);
		}

		// load Relationship for Customer
		List<CustomerMemberDTO> customerRelationship =
				transversClient.findRelationshipByCustomer(customerDTO.getCustomerIdExtern());
		// inserting Relationship
		for (CustomerMemberDTO customerMemberDTO : customerRelationship) {
			// check Relationship if exist before saving link
			CustomerDTO relationship = loadCustomer(customerMemberDTO.getRelationshipId());

			// check link customer relationship if exist
			CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
			params.setCustomerId(customerDTO.getId());
			params.setMember(relationship);
			params.setCategory(LinkRelationshipsCategory.RELATIONSHIP.name());
			List<CustomerLinksRelationshipDTO> existRelationship =
					customerLinksRelationshipService.find(params);
			if (!ACMValidationUtils.isNullOrEmpty(existRelationship)) {
				// update existing relationship => Add ID_ACM_LOAN
				CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
						existRelationship.get(0);
				customerLinksRelationshipDTO.setIdLoan(loanDTO.getLoanId());
				// update data
				customerLinksRelationshipService.save(customerLinksRelationshipDTO.getId(),
						customerLinksRelationshipDTO);
			}
			else {
				// saving Relationship in DB
				CustomerLinksRelationshipDTO newRelationship = customerLinksRelationshipService
						.save(new CustomerLinksRelationshipDTO(customerDTO.getId(), relationship,
								customerMemberDTO.getCustomerRole(),
								LinkRelationshipsCategory.RELATIONSHIP.name(), new Date(),
								loanDTO.getLoanId(), null));
				logger.info("newRelationship = {}", newRelationship);
			}
		}
	}

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#save(java.lang.Integer, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO save(Long id, LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update loan  with ID = {}", id);
		Loan oldLoan = loanRepository.findById(id).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldLoan)
		mapper.map(loanDTO, oldLoan);
		CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
		Loan newLoan = loanRepository.save(oldLoan);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return mapper.map(newLoan, LoanDTO.class);
	}

	/**
	 * Update child and group.
	 *
	 * @param id the id
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateChildAndGroup(java.lang.Integer,
	 * com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO updateChildAndGroup(Long id, LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("update Child And Group  with ID = {}", id);
		Loan oldLoan = loanRepository.findById(id).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		BigDecimal oldLoanAmountChild = oldLoan.getApplyAmountTotal();
		// mapping new data with existing data (oldLoan)
		mapper.map(loanDTO, oldLoan);
		CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
		Loan newLoan = loanRepository.save(oldLoan);
		LoanDTO oldLoanGrp = find(loanDTO.getParentId());
		if (oldLoanGrp != null) {
			BigDecimal oldLoanAmountGroupe = oldLoanGrp.getApplyAmountTotal();
			BigDecimal newLoanAmountGroupe = (oldLoanAmountGroupe.subtract(oldLoanAmountChild))
					.add(newLoan.getApplyAmountTotal());
			oldLoanGrp.setApprovelAmount(newLoanAmountGroupe);
			oldLoanGrp.setApplyAmountTotal(newLoanAmountGroupe);
			save(oldLoanGrp.getLoanId(), oldLoanGrp);
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return mapper.map(newLoan, LoanDTO.class);
	}

	/**
	 * Delete.
	 *
	 * @param loanDTO the loan DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#delete(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void delete(LoanDTO loanDTO) {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete loan  with ID = {}", loanDTO.getLoanId());
		// delete object by id
		loanRepository.deleteById(loanDTO.getLoanId());
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, Loan.class.getSimpleName());
	}

	/**
	 * Rejected.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#rejected(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.REJECT_LOAN)
	public LoanDTO rejected(LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException, IOException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// load loan data by ID
		Loan loan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (loan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		String oldStatutLibelle = loan.getStatutLibelle();
		// set statut tab dashboard to rejected
		loan.setStatut(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REJECTED)
				.getKey());
		loan.setStatutLibelle(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getValue());
		// set statut workflow loan to rejected
		loan.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
		// setting date change status
		loan.setChangeDateStatusWorkflow(new Date());
		loan.setCategory(CommonConstants.CATEGORY_INSTANCE);
		CommonFunctions.mapperToUpdate(loan, userClient, logger);
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getIsNotFromWorkflow())
				&& Boolean.TRUE.equals(loanDTO.getIsNotFromWorkflow())) {
			try {
				transversClient.cancelLoan(loanDTO);
			}
			catch (Exception e) {
				logger.error("Failed to rejected loan {}", e.getMessage());
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
		}
		Loan updatedLoan = loanRepository.save(loan);
		if (loanDTO.getParentId() == 0 && loanDTO.getIsNotFromWorkflow() == Boolean.TRUE) {

			saveLoanApprovalHistorique(loanDTO,
					CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REJECTED)
							.getKey(),
					oldStatutLibelle);
		}

		// mapping data
		LoanDTO updatedLoanDTO = mapper.map(updatedLoan, LoanDTO.class);

		// update statut for loan child
		updateStatutChild(updatedLoanDTO, ACMConstantWorkflowStatuts.REJECTED,
				ACMConstantWorkflowStatuts.STATUS_TAB_REJECTED);

		// update card number status & delete from UDF
		updateMezaCardStatus(updatedLoanDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());

		// reverse all journal entry
		for (LoanInstanceDTO loanInstanceDTO : updatedLoanDTO.getLoanInstancesDtos()) {
			// check if journal entry exist than we reverse it
			if (!ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getJournalEntry())) {
				reverseJournalEntry(loanInstanceDTO);
				// after reverse update journal entry in loan instance to NULL
				loanInstanceDTO.setJournalEntry(null);
				loanInstanceService.save(loanInstanceDTO.getId(), loanInstanceDTO);
			}

		}

		// close task
		CalendarEventDTO calenderEventDTO = new CalendarEventDTO();
		calenderEventDTO.setIdLoanExtern(updatedLoanDTO.getIdLoanExtern());
		crmClient.createLoanTaskAndCloseOldTask(calenderEventDTO, Boolean.FALSE, Boolean.FALSE,
				Boolean.TRUE);

		// Update in IB
		if (!ACMValidationUtils.isNullOrEmpty(updatedLoanDTO.getIdIbLoan())) {
			updatedLoanDTO.setStepPath("Rejected");
			loadDataIbService.updateAcmLoanInIB(updatedLoanDTO);
		}

		return updatedLoanDTO;
	}

	/**
	 * Save loan approval historique.
	 *
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @param statusId the status id
	 * @param statutLibelle the statut libelle
	 */
	private void saveLoanApprovalHistorique(LoanDTO loanDTO, Integer statusId,
			String statutLibelle) {

		// save historique approvel data
		LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO =
				new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
						statusId, loanDTO.getNote(), loanDTO.getStatutWorkflow());
		loanApprovalHistoriqueDTO.setApprovalLevelLabel(statutLibelle);
		LoanApprovalHistoriqueDTO newLoanApprovalHistoriqueDTO =
				loanApprovalHistoriqueService.saveAndSetApprovalLabel(loanApprovalHistoriqueDTO);

		logger.info(
				"Loan Approval Historique with status : {} was successfully inserted for loan :  {}",
				newLoanApprovalHistoriqueDTO.getApprovalDesicionLabel(),
				loanDTO.getAccountNumber());
	}

	/**
	 * Cancelled.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CancelIssuedLoanException the cancel issued loan exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#cancelled(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.CANCEL_LOAN)
	public LoanDTO cancelled(LoanDTO loanDTO) throws ResourcesNotFoundException, IOException,
			ApiAbacusException, CancelIssuedLoanException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// load loan data by ID
		Loan loan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (loan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		// check if the loan issued in abacus
		if (checkLoanStatusIssued(loan.getIdLoanExtern())) {
			throw new CancelIssuedLoanException(
					new ExceptionResponseMessage(CommonErrorCode.CANNOT_CANCEL_ISSUED_LOAN,
							CommonExceptionsMessage.CANNOT_CANCEL_ISSUED_LOAN),
					CommonExceptionsMessage.CANNOT_CANCEL_ISSUED_LOAN);
		}

		// set statut tab dashboard to cancelled
		loan.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED).getKey());

		// set statut workflow loan to cancelled
		loan.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
		loan.setStatutLibelle(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getValue());
		// setting date change status
		loan.setChangeDateStatusWorkflow(new Date());
		loan.setCategory(CommonConstants.CATEGORY_INSTANCE);
		CommonFunctions.mapperToUpdate(loan, userClient, logger);
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getIsNotFromWorkflow())
				&& Boolean.TRUE.equals(loanDTO.getIsNotFromWorkflow())) {
			try {
				transversClient.cancelLoan(loanDTO);
			}
			catch (Exception e) {
				logger.error("Error : {}", e.getMessage());
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
		}
		Loan updatedLoan = loanRepository.save(loan);
		// mapping data
		LoanDTO updatedLoanDTO = mapper.map(updatedLoan, LoanDTO.class);
		// update statut for loan child
		updateStatutChild(updatedLoanDTO, ACMConstantWorkflowStatuts.CANCELLED,
				ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED);

		// update card number status & delete from UDF
		updateMezaCardStatus(updatedLoanDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		Integer key = CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_CANCELLED).getKey();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getNote())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())) {
			loanApprovalHistoriqueService.saveAndSetApprovalLabel(
					new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
							key, loanDTO.getNote(), loanDTO.getStatutWorkflow()));
		}

		// reverse all journal entry
		for (LoanInstanceDTO loanInstanceDTO : updatedLoanDTO.getLoanInstancesDtos()) {
			// check if journal entry exist than we reverse it
			if (!ACMValidationUtils.isNullOrEmpty(loanInstanceDTO.getJournalEntry())) {
				reverseJournalEntry(loanInstanceDTO);
				// after reverse update journal entry in loan instance to NULL
				loanInstanceDTO.setJournalEntry(null);
				loanInstanceService.save(loanInstanceDTO.getId(), loanInstanceDTO);
			}
		}

		// close task
		CalendarEventDTO calenderEventDTO = new CalendarEventDTO();
		calenderEventDTO.setIdLoanExtern(updatedLoanDTO.getIdLoanExtern());
		crmClient.createLoanTaskAndCloseOldTask(calenderEventDTO, Boolean.FALSE, Boolean.FALSE,
				Boolean.TRUE);
		// Update in IB
		if (!ACMValidationUtils.isNullOrEmpty(updatedLoanDTO.getIdIbLoan())) {
			updatedLoanDTO.setStepPath("Cancelled");
			loadDataIbService.updateAcmLoanInIB(updatedLoanDTO);
		}
		return updatedLoanDTO;
	}

	/**
	 * Update MEZA CARD status.
	 *
	 * @author ManelLamloum
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	private void updateMezaCardStatus(LoanDTO loanDTO) throws IOException, ApiAbacusException {

		// lunch process Delete assign Mezacard from UDF Link and from ABACUS (if exist)
		try {
			// find udf customer
			CustomerDTO customerDTO = customerService.find(loanDTO.getCustomerDTO().getId());
			if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getMezaCardStatus())
					&& customerDTO.getMezaCardStatus().equals(CustomerMezaCardStatus.NEW.name())) {
				// set status none for meza card customer
				customerDTO.setMezaCardStatus(CustomerMezaCardStatus.NONE.name());
				// get list udf
				List<UserDefinedFieldsLinksDTO> backupUDFFieldsLinksDTOs =
						customerDTO.getUserDefinedFieldsLinksDTOs();
				Long idUdfAccountNumber = 0L;
				Long idUdfDisbursmentMethod = 0L;
				UserDefinedFieldsDTO param = new UserDefinedFieldsDTO();
				param.setNames(Arrays.asList("Account number", "Disbursment Method"));
				List<UserDefinedFieldsDTO> udfFields = parametrageClient.find(param);
				if (!ACMValidationUtils.isNullOrEmpty(udfFields)) {
					for (UserDefinedFieldsDTO udf : udfFields) {
						if (udf.getName().equals("Account number")) {
							idUdfAccountNumber = udf.getId();
						}
						if (udf.getName().equals("Disbursment Method")) {
							idUdfDisbursmentMethod = udf.getId();
						}
					}

					// check if Disbursment Method=MezaCard
					Boolean udfDisbursmentMethodLink = Boolean.FALSE;
					// find from udfListValue id of meza card value
					UserDefinedFieldListValuesDTO definedFieldListValuesDTO =
							new UserDefinedFieldListValuesDTO();
					definedFieldListValuesDTO.setName("MezaCard Internal");
					List<UserDefinedFieldListValuesDTO> udfLinksMizaCard =
							parametrageClient.find(definedFieldListValuesDTO);
					String idMezaCardValue = "0";
					if (!ACMValidationUtils.isNullOrEmpty(udfLinksMizaCard)) {
						idMezaCardValue = udfLinksMizaCard.get(0).getIdUDFListValue().toString();
					}
					for (UserDefinedFieldsLinksDTO udflink : customerDTO
							.getUserDefinedFieldsLinksDTOs()) {
						// MezaCard ([ACM_UDF_LIST_VALUES] )
						if (udflink.getUserDefinedFieldsDTO().getId() == idUdfDisbursmentMethod
								&& udflink.getFieldValue().equals(idMezaCardValue)) {

							udfDisbursmentMethodLink = Boolean.TRUE;
							break;
						}
					}
					if (Boolean.TRUE.equals(udfDisbursmentMethodLink)) {
						Long idUdfLinkACMAccountNumber = 0L;
						UserDefinedFieldsDTO definedFieldsDTOAccountNumber =
								new UserDefinedFieldsDTO();
						Long idUdfLinkACMDisbursmentMethod = 0L;
						UserDefinedFieldsDTO definedFieldsDTODisbursmentMethod =
								new UserDefinedFieldsDTO();
						// check && delete MezaCard value if exist
						for (UserDefinedFieldsLinksDTO udflink : customerDTO
								.getUserDefinedFieldsLinksDTOs()) {
							// setting value to null => to be deleted in ABACUS DB
							if (udflink.getUserDefinedFieldsDTO().getId() == idUdfAccountNumber) {
								logger.warn("************************* UDF ID ACCOUNT NUMBER = {}",
										idUdfAccountNumber);
								udflink.setFieldValue("");
								idUdfLinkACMAccountNumber = udflink.getId();
								definedFieldsDTOAccountNumber = udflink.getUserDefinedFieldsDTO();
								break;
							}
							if (udflink.getUserDefinedFieldsDTO()
									.getId() == idUdfDisbursmentMethod) {
								logger.warn(
										"************************* UDF ID DISBURSMENT METHODE = {}",
										idUdfDisbursmentMethod);
								udflink.setFieldValue("");
								idUdfLinkACMDisbursmentMethod = udflink.getId();
								definedFieldsDTODisbursmentMethod =
										udflink.getUserDefinedFieldsDTO();
								break;
							}
						}

						try {
							// CALL API ABACUS TO UPDATE CUSTOMER
							logger.warn("************************* UDF = {}",
									customerDTO.getUserDefinedFieldsLinksDTOs());
							transversClient.updateCustomer(customerDTO);
							// Set Meza card status to 'ACTIVATE' (if Meza Card disbursement method
							// is
							// selected)
							try {
								// update statuts none for the customer meza card status
								customerService.updateMezaCardStatus(customerDTO);
								mezaCardService.update(
										new AcmMezaCardDTO(null, MezaCardStatus.ACTIVATE.toString(),
												loanDTO.getCustomerDTO()));
							}
							catch (Exception e) {
								e.printStackTrace();
							}
							if (idUdfLinkACMAccountNumber != 0) {
								// check and update udf in ACM AccountNumber
								UserDefinedFieldsLinksDTO definedFieldsLinksDTOAccountNumber =
										new UserDefinedFieldsLinksDTO();
								definedFieldsLinksDTOAccountNumber.setId(idUdfLinkACMAccountNumber);
								definedFieldsLinksDTOAccountNumber.setEnableData(Boolean.FALSE);
								definedFieldsLinksDTOAccountNumber
										.setCustomerId(customerDTO.getId());
								definedFieldsLinksDTOAccountNumber.setFieldValue("");
								definedFieldsLinksDTOAccountNumber
										.setUserDefinedFieldsDTO(definedFieldsDTOAccountNumber);
								userDefinedFieldsLinksService.save(idUdfLinkACMAccountNumber,
										definedFieldsLinksDTOAccountNumber);
							}
							if (idUdfLinkACMDisbursmentMethod != 0) {
								// check and update udf in ACM DisbursmentMethod
								UserDefinedFieldsLinksDTO definedFieldsLinksDTODisbursmentMethod =
										new UserDefinedFieldsLinksDTO();
								definedFieldsLinksDTODisbursmentMethod
										.setId(idUdfLinkACMDisbursmentMethod);
								definedFieldsLinksDTODisbursmentMethod.setEnableData(Boolean.FALSE);
								definedFieldsLinksDTODisbursmentMethod
										.setCustomerId(customerDTO.getId());
								definedFieldsLinksDTODisbursmentMethod.setFieldValue("");
								definedFieldsLinksDTODisbursmentMethod
										.setUserDefinedFieldsDTO(definedFieldsDTODisbursmentMethod);
								userDefinedFieldsLinksService.save(idUdfLinkACMDisbursmentMethod,
										definedFieldsLinksDTODisbursmentMethod);
							}
						}
						catch (Exception e) {
							logger.error("Failed to update Customer {}", e.getMessage());
							logger.error(
									CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
									e.getMessage());
							// INIT error message
							String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
							if (e.getMessage().contains("errorMessage")) {
								String msgFromTransversApi =
										e.getMessage().substring(e.getMessage().indexOf('{'));
								final JsonNode jsonNode =
										new ObjectMapper().readTree(msgFromTransversApi);
								messageError = jsonNode.get("errorMessage").asText();
							}
							logger.info("Rollback data in ACM UDF");
							// delete UDF
							logger.info("save old UDF");
							userDefinedFieldsLinksService.deleteAllByCustomer(customerDTO.getId());
							// save old udf in ACM
							for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : backupUDFFieldsLinksDTOs) {
								userDefinedFieldsLinksDTO.setCustomerId(customerDTO.getId());
								userDefinedFieldsLinksService.save(userDefinedFieldsLinksDTO);
							}
							// Fire Exception
							throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
						}
					}
				}
			}
		}
		catch (ResourcesNotFoundException e) {
			logger.error("Error finding customer with given ID {}",
					loanDTO.getCustomerDTO().getId());
		}
	}

	/**
	 * Update statut child.
	 *
	 * @author Ines Dridi
	 * @param loanParent the loan parent
	 * @param statutworkflow the statutworkflow
	 * @param statutworkflowTab the statutworkflow tab
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void updateStatutChild(LoanDTO loanParent, String statutworkflow,
			String statutworkflowTab) throws ResourcesNotFoundException {

		logger.info("updateStatutChild() method :: START");
		// if Loan is GRP : settingLoanData for group child
		if (loanParent.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
			// load child
			List<LoanDTO> loansChilds = findByParentId(loanParent.getLoanId());
			// update data for child
			for (LoanDTO child : loansChilds) {
				// setting owner
				child.setOwner(loanParent.getOwner());
				child.setOwnerName(loanParent.getOwnerName());

				// setting list participants
				loanParticipantsService
						.save(new LoanParticipantsDTO(child.getLoanId(), child.getOwner()));
				// setting data for loan
				child.setStatutWorkflow(CommonFunctions.mappingStatus(statutworkflow).getKey());
				child.setStatutLibelle(CommonFunctions.mappingStatus(statutworkflow).getValue());
				child.setStatut(CommonFunctions.mappingStatus(statutworkflowTab).getKey());
				// setting date change status
				child.setChangeDateStatusWorkflow(new Date());
				child.setCategory(CommonConstants.CATEGORY_INSTANCE);
				// update loan data
				save(child.getLoanId(), child);
			}
		}
		logger.info("updateStatutChild() method :: DONE");
	}

	/**
	 * Declined.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#cancelled(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.DECLINE_LOAN)
	public LoanDTO declined(LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// load loan data by ID
		Loan loan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (loan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		// set statut tab dashboard to cancelled
		loan.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED).getKey());
		loan.setStatutLibelle(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getValue());
		// set statut workflow loan to cancelled
		loan.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());

		CommonFunctions.mapperToUpdate(loan, userClient, logger);
		Loan updatedLoan = loanRepository.save(loan);
		// mapping data
		LoanDTO updatedLoanDTO = mapper.map(updatedLoan, LoanDTO.class);

		// update statut for loan child
		updateStatutChild(updatedLoanDTO, ACMConstantWorkflowStatuts.DECLINE,
				ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED);

		// update card number status & delete from UDF
		try {
			updateMezaCardStatus(updatedLoanDTO);
		}
		catch (ApiAbacusException | IOException e) {
			logger.error("Failed to Re-ACTIVATE MEZA CARD");
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return updatedLoanDTO;
	}

	/**
	 * Validate.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InitialCheckException the initial check exception
	 * @throws FieldVisitNotFoundException the field visit not found exception
	 * @throws GuarantorsNotFoundException the guarantors not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#validate(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO validate(LoanDTO loanDTO)
			throws ResourcesNotFoundException, InitialCheckException, FieldVisitNotFoundException,
			GuarantorsNotFoundException, CollateralNotFoundException, CheckAppL1NotFoundException,
			InformCustomerNotFoundException, UploadSignedDocNotFoundExepction,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, EnableCriticalDataException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (loanDTO.getStatutWorkflow() != null) {
			logger.info("##### Current Status = {} :: {}", loanDTO.getStatutWorkflow(),
					loanDTO.getStatutLibelle());
			// processing loan status
			loanDTO = executeProcessByGivenStatut(loanDTO);

			// LoanDTO returnedLoanDTO = find(loanDTO.getLoanId());
			logger.info("##### New Status After processing Loan Workflow = {} :: {}",
					loanDTO.getStatutWorkflow(), loanDTO.getStatutLibelle());
			// setting Missing Data list if exist
			logger.debug("List missing data with missing data = [{}]",
					loanDTO.getListMissingData());
			return loanDTO;
		}
		return loanDTO;
	}

	/**
	 * Validate all.
	 *
	 * @param loanDTOs the loan DT os
	 * @return the list
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#validateAll(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> validateAll(List<LoanDTO> loanDTOs) throws ApiAbacusException, IOException,
			CreditException, UploadDocumentNotFoundException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		logger.info("validate All LOANS of given loan Group :: START");
		Preconditions.checkNotNull(loanDTOs, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<LoanDTO> returnedLoanDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTOs)) {
			for (LoanDTO loan : loanDTOs) {
				try {
					LoanDTO validatedLoan = validate(loan);
					returnedLoanDTOs.add(validatedLoan);
				}
				catch (ResourcesNotFoundException | InitialCheckException
						| FieldVisitNotFoundException | GuarantorsNotFoundException
						| CollateralNotFoundException | CheckAppL1NotFoundException
						| InformCustomerNotFoundException | UploadSignedDocNotFoundExepction
						| CheckAppL2NotFoundException | CheckAppL3NotFoundException
						| CheckAppL4NotFoundException | CheckApprovelLevelException
						| EnableCriticalDataException e) {
					logger.error("Error will validating loans : {}", e.getMessage());
				}
			}
		}
		logger.info("validate All LOANS :: DONE");
		return returnedLoanDTOs;
	}

	/**
	 * Complete workflow for childs.
	 *
	 * @param loanDTO the loan DTO
	 * @param loansChild the loans child
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#completeWorkflowForChilds(com.acm.utils.dtos. LoanDTO,
	 * java.util.List)
	 */
	@Override
	public void completeWorkflowForChilds(LoanDTO loanDTO, List<LoanDTO> loansChild)
			throws ResourcesNotFoundException, CheckApprovelLevelException, ApiAbacusException,
			IOException, CreditException, UploadDocumentNotFoundException, DisbursementException,
			WorkFlowSettingException, CheckMezaCardException, CheckMezaCardUntrustException,
			CheckFeesException {

		logger.info("Method completeWorkflowForChilds() :: START");
		Preconditions.checkNotNull(loansChild, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		for (LoanDTO loanDTOChild : loansChild) {
			logger.info("{}", loanDTOChild);
			// in review case
			loanDTOChild.setConfirm(Boolean.TRUE);
			if (loanDTOChild.getStatutWorkflow() != null
					&& loanDTOChild.getProcessInstanceId() != null) {
				try {
					// checking NEXT Action
					if (loanDTOChild.getWorkflowNextAction() == null
							&& loanDTO.getWorkflowNextAction() != null) {
						loanDTOChild.setWorkflowNextAction(loanDTO.getWorkflowNextAction());
					}
					else if (loanDTOChild.getWorkflowNextAction() == null
							&& loanDTO.getWorkflowNextAction() == null) {
						loanDTOChild.setWorkflowNextAction(
								ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
					}
					// processing loan by status
					executeProcessByGivenStatut(loanDTOChild);
				}
				catch (InitialCheckException | FieldVisitNotFoundException
						| GuarantorsNotFoundException | CollateralNotFoundException
						| CheckAppL1NotFoundException | CheckAppL2NotFoundException
						| CheckAppL3NotFoundException | CheckAppL4NotFoundException
						| InformCustomerNotFoundException | UploadSignedDocNotFoundExepction
						| ConditionalApproveException | EnableCriticalDataException e) {
					logger.error("Failed to completeWorkflowForChilds {} !!!", e.getMessage());
				}
			}
		}
		logger.info(
				"All [{}] loans childs workflow process was updated for the principal LOAN with id = [{}]",
				loansChild.size(), loanDTO.getLoanId());
	}

	/**
	 * Execute process by given statut.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InitialCheckException the initialCheck not found exception
	 * @throws FieldVisitNotFoundException the fieldVisit not found exception
	 * @throws GuarantorsNotFoundException the guarantors not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception
	 * @throws InformCustomerNotFoundException the informCustome not found exception
	 * @throws UploadSignedDocNotFoundExepction the uploadSignedDoc not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	private LoanDTO executeProcessByGivenStatut(LoanDTO loanDTO)
			throws ResourcesNotFoundException, InitialCheckException, FieldVisitNotFoundException,
			GuarantorsNotFoundException, CollateralNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			InformCustomerNotFoundException, UploadSignedDocNotFoundExepction,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, EnableCriticalDataException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		// init workflowNextActions list with possible action executed by user
		List<String> workflowNextActions =
				Arrays.asList(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_ISSUED,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REJECT,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_ASK_FOR_REVIEW,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_AGREED,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_DECLINED,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_CANCELED,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_SUBMIT,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_SCREENING,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_AUDIT,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_RISK,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW_AGREEMENT,
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_CUSTOMER_DECISION);

		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())
				&& workflowNextActions.contains(loanDTO.getWorkflowNextAction())) {

			switch (loanDTO.getStatutWorkflow()) {
				// ACMConstantWorkflowStatuts.INITIAL_CHECK
				case 1:
					return loanWorkflowUserActionService.actionInitialCheck(loanDTO);

				// ACMConstantWorkflowStatuts.FIELD_VISIT
				case 2:
					return loanWorkflowUserActionService.actionFieldVisit(loanDTO);

				// ACMConstantWorkflowStatuts.GUARANTOR
				case 3:
					return loanWorkflowUserActionService.actionCheckGuarantor(loanDTO);

				// ACMConstantWorkflowStatuts.COLLATERAL
				case 4:
					return loanWorkflowUserActionService.actionCheckCollateral(loanDTO);

				// ACMConstantWorkflowStatuts.ADD_DOCUMENTS
				case 5:
					return loanWorkflowUserActionService.actionUploadDocuments(loanDTO);

				// ACMConstantWorkflowStatuts.FINANCIAL_ANALYSIS
				case 6:
					return loanWorkflowUserActionService.actionAddFinancialAnalysis(loanDTO);

				// TODO 7 -> 10
				// ACMConstantWorkflowStatuts.APPROVAL_L1
				case 7:
					return loanWorkflowUserActionService.actionCheckL1(loanDTO);

				// ACMConstantWorkflowStatuts.APPROVAL_L2
				case 8:
					return loanWorkflowUserActionService.actionCheckL2(loanDTO);

				// ACMConstantWorkflowStatuts.APPROVAL_L3
				case 9:
					return loanWorkflowUserActionService.actionCheckL3(loanDTO);

				// ACMConstantWorkflowStatuts.APPROVAL_L4
				case 10:
					return loanWorkflowUserActionService.actionCheckL4(loanDTO);

				// ACMConstantWorkflowStatuts.CUSTOMER_DECISION
				case 11:
					return loanWorkflowUserActionService.actionInformCustomer(loanDTO);

				// ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT
				case 12:
					return loanWorkflowUserActionService.actionUploadSignedAgreements(loanDTO);

				// ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE
				case 13:
					// if loan is old loan (with static workflow)
					if (loanDTO.getEtapeWorkflow() <= 23) {
						return loanWorkflowUserActionService
								.actionInformCustomerAfterDocsSigne(loanDTO);
					}
					else {
						return stepWorkFlow(loanDTO);
					}

				case 18:
					return loanWorkflowUserActionService.actionScreening(loanDTO);

				// TO 19 20
				// ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_AUDIT
				case 19:
					return loanWorkflowUserActionService.actionAudit(loanDTO);

				// ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_RISK
				case 20:
					return loanWorkflowUserActionService.actionRisk(loanDTO);

				// ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_COMPLET_DATA
				case 21:
					return loanWorkflowUserActionService.actionCompletData(loanDTO);

				// ACMConstantWorkflowStatuts.CENTRAL_REVISION
				case 23:
					return loanWorkflowUserActionService.actionCentralRevision(loanDTO);

				default:
					return stepWorkFlow(loanDTO);
			}
		}
		return loanDTO;
	}

	/**
	 * Save for timer.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#saveForTimer(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO saveForTimer(LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Loan oldLoan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		// setting category
		oldLoan.setCategory(loanDTO.getCategory());
		CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
		Loan newLoan = loanRepository.save(oldLoan);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return mapper.map(newLoan, LoanDTO.class);
	}

	/**
	 * Load filter status workflow.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanDetailsService#loadFilterStatusWorkflow(com.acm.utils. dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> loadFilterStatusWorkflow(LoanDTO loanDTO) {

		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(loanDTO, qLoan);
		predicate.and(qLoan.statutLibelle.isNotNull());
		// SELECT distinct statutWorkflow
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<Loan> loans = queryFactory.select(Projections.bean(Loan.class, qLoan.statutLibelle))
				.distinct().from(qLoan).where(predicate).fetch();

		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("{} : Status Workflow to use in Filter dashboard was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Load filter product.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanDetailsService#loadFilterProduct(com.acm.utils.dtos. LoanDTO)
	 */
	@Override
	public List<LoanDTO> loadFilterProduct(LoanDTO loanDTO) {

		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(loanDTO, qLoan);

		// SELECT distinct productDescription
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<Loan> loans =
				queryFactory.select(Projections.bean(Loan.class, qLoan.productDescription))
						.distinct().from(qLoan).where(predicate).fetch();

		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("{} : Products to use in Filter dashboard was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Reassigned.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#reassigned(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.REASSIGN_LOAN)
	public LoanDTO reassigned(LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// load loan data by ID
		Loan loan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (loan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		// update list participants
		loanParticipantsService
				.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

		// set new owner from reassign
		loan.setOwner(loanDTO.getOwner());
		loan.setOwnerName(userClient.findByLogin(loanDTO.getOwner()).getSimpleName());

		// setting list participants
		LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
				.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
		logger.info("setting Loan Participants with ID = [{}] :: DONE",
				newLoanParticipantsDTO.getId());

		CommonFunctions.mapperToUpdate(loan, userClient, logger);
		Loan updatedLoan = loanRepository.save(loan);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		Integer key =
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LOAN_REASSIGN).getKey();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getNote())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())) {
			loanApprovalHistoriqueService.saveAndSetApprovalLabel(
					new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
							key, loanDTO.getNote(), loanDTO.getStatutWorkflow()));
		}
		return mapper.map(updatedLoan, LoanDTO.class);
	}

	/**
	 * Save to abacus.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#createToAbacus(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO saveToAbacus(LoanDTO loanDTO)
			throws CustomerMaxActiveAccountException, ApiAbacusException {

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
		else {
			logger.info("send loan to ABACUS via API");
			try {
				// find product object by ID
				loanDTO.setProductDTO(
						parametrageClient.findProductById(loanDTO.getProductId().longValue()));
				Integer maxIssueDate =
						Integer.parseInt(parametrageClient.find("ISSUE_DATE").getValue());
				Integer dateRepaymentDate =
						Integer.parseInt(parametrageClient.find("INITIAL_PAYMENT").getValue());
				if (maxIssueDate != 0 && dateRepaymentDate != 0) {
					loanDTO.setInitialPaymentDate(
							Date.from(calculateFirstRepaymentDate(maxIssueDate, dateRepaymentDate)
									.atStartOfDay(ZoneId.systemDefault()).toInstant()));
				}
				// find all customer data if any are missing (address, UDF, link,...)
				if (ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO()) || ACMValidationUtils
						.isNullOrEmpty(loanDTO.getCustomerDTO().getListAddress())) {
					loanDTO.setCustomerDTO(customerService.find(loanDTO.getCustomerDTO().getId()));
				}
				// before save to Abacus store the list of Assets
				List<AssetLoanDTO> loanAssets = loanDTO.getLoanAssetsDtos();
				// The application is add in the CBS via API (Add API)
				LoanDTO abacusLoanDTO = transversClient.createLoanINDIV(loanDTO);

				logger.info("{}  has been successfully created in ABACUS with ID extern = [{}].",
						Loan.class.getSimpleName(),
						abacusLoanDTO != null ? abacusLoanDTO.getIdLoanExtern() : "NULL");
				// set the status of the loan application : NEW_APPLICATION
				abacusLoanDTO.setLoanApplicationStatus(CommonConstants.NEW_APPLICATION);
				abacusLoanDTO.setTermPeriodNum(loanDTO.getTermPeriodNum());
				abacusLoanDTO.setInterestFreq(loanDTO.getInterestFreq());
				abacusLoanDTO.setPaymentFreq(loanDTO.getPaymentFreq());
				abacusLoanDTO.setPeriodsDeferredType(loanDTO.getPeriodsDeferredType());
				abacusLoanDTO.setInstallmentNumber(abacusLoanDTO.getTermPeriodNum());
				abacusLoanDTO.setTotalInterest(loanDTO.getTotalInterest());
				abacusLoanDTO.setIdIbLoan(loanDTO.getIdIbLoan());
				abacusLoanDTO.setBranchName(loanDTO.getBranchName());
				// save to ACM
				abacusLoanDTO.setLoanAssetsDtos(loanAssets);
				abacusLoanDTO.setPersonalContribution(loanDTO.getPersonalContribution());
				abacusLoanDTO.setOpeningBalance(loanDTO.getOpeningBalance());
				abacusLoanDTO
						.setUserDefinedFieldsLinksDTOs(loanDTO.getUserDefinedFieldsLinksDTOs());
				abacusLoanDTO = save(abacusLoanDTO);

				// call method to generate task for the first step if generation task is active
				// in
				// setting WF
				LoanCalendarSyncService loanCalendarSyncService =
						new LoanCalendarSyncServiceImpl(acmLoanInstanceGroupeAssociationService,
								notificationsServices, userClient, parametrageClient, crmClient);
				loanCalendarSyncService.generationTaskForStepLoan(abacusLoanDTO,
						abacusLoanDTO.getEtapeWorkflow().longValue(), null);

				return abacusLoanDTO;
			}
			catch (Exception e) {
				throw new ApiAbacusException(
						new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
								CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
								new TechnicalException()),
						CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
			}
		}
	}

	/**
	 * Save loan group to abacus.
	 *
	 * @param loanDTOs the loan DT os
	 * @return the list
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#createToAbacus(com.acm.utils.dtos.LoanDTOs)
	 */
	@Override
	public List<LoanDTO> saveLoanGroupToAbacus(List<LoanDTO> loanDTOs) throws ApiAbacusException {

		logger.info("send LOAN GROUP to ABACUS via API");
		try {
			// find && sett customer data by ID
			for (LoanDTO loanDTO : loanDTOs) {
				Long idCustomer = loanDTO.getCustomerDTO().getId();
				// find customer by id
				loanDTO.setCustomerDTO(customerService.find(idCustomer));
				// find link relationship for group customer
				if (loanDTO.getCustomerDTO().getCustomerType().equals(CustomerType.GRP.name())) {
					CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
							new CustomerLinksRelationshipDTO();
					customerLinksRelationshipDTO.setCustomerId(idCustomer);
					customerLinksRelationshipDTO
							.setCategory(LinkRelationshipsCategory.MEMBERS.name());
					List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs =
							customerLinksRelationshipService.find(customerLinksRelationshipDTO);
					loanDTO.getCustomerDTO()
							.setCustomerLinksRelationshipDTOs(customerLinksRelationshipDTOs);
				}
			}
			try {
				// call API Create Loan for GRP
				List<LoanDTO> abacusLoanDTOs = transversClient.createLoanGRP(loanDTOs);
				// save grp loan and child to ACM
				for (LoanDTO loanDTO : loanDTOs) {
					for (LoanDTO loanDTOAba : abacusLoanDTOs) {
						if (loanDTOAba.getCustomerId()
								.equals(loanDTO.getCustomerDTO().getCustomerIdExtern())
								&& loanDTOAba.getCustomerName()
										.equals(loanDTO.getCustomerDTO().getCustomerName())) {
							save(loanDTOAba);
						}
					}
				}
			}
			catch (Exception e) {
				logger.error("Failed to add loan grp {}", e.getMessage());
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
		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
							new TechnicalException()),
					CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
		}
		return loanDTOs;
	}

	/**
	 * Update for application.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateForApplication(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO updateForApplication(LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException {

		try {
			logger.info("complete loan data");
			// update loan
			loanDTO.setUpdateLoan(Boolean.TRUE);

			// find existing loan by ID
			Loan oldLoan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
			// check if object is null
			if (oldLoan == null) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
								+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
			}
			// check if amount is change
			// in new WF we will don't do the check in case of amount is changed
			if (loanDTO.getApplyAmountTotal().compareTo(oldLoan.getApplyAmountTotal()) != 0
					&& oldLoan.getEtapeWorkflow() <= 23) {
				// updating the process for given loan
				// List<LoanInstanceDTO> newLoanInstanceDTOs =
				// loanInstanceService.updateForWorkflow(loanDTO);
				// loanDTO.setLoanInstancesDtos(newLoanInstanceDTOs);

				// setting approval amount
				loanDTO.setApprovelAmount(loanDTO.getApplyAmountTotal());
			}
			// mapping new data with existing data (oldLoan)
			mapper.map(loanDTO, oldLoan);
			CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
			Loan newLoan = loanRepository.save(oldLoan);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
			LoanDTO updateLoanDTO = mapper.map(newLoan, LoanDTO.class);

			// Update UDF
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : loanDTO
					.getUserDefinedFieldsLinksDTOs()) {
				userDefinedFieldsLinksDTO.setLoanId(updateLoanDTO.getLoanId());
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
			}
			logger.info("send LOAN to ABACUS via API");
			// SET Product
			updateLoanDTO.setProductDTO(loanDTO.getProductDTO());
			// SET UDF
			updateLoanDTO.setUserDefinedFieldsLinksDTOs(loanDTO.getUserDefinedFieldsLinksDTOs());
			// setting loan reason
			updateLoanDTO.setLoanReasonId(
					loanDTO.getLoanReasonId() != null ? loanDTO.getLoanReasonId() : "2");
			// default mode REPAYMENT AMOUNT => 0
			updateLoanDTO.setLoanCalculationMode(0);
			Integer maxIssueDate =
					Integer.parseInt(parametrageClient.find("ISSUE_DATE").getValue());
			Integer dateRepaymentDate =
					Integer.parseInt(parametrageClient.find("INITIAL_PAYMENT").getValue());
			if (maxIssueDate != 0 && dateRepaymentDate != 0) {
				updateLoanDTO.setInitialPaymentDate(
						Date.from(calculateFirstRepaymentDate(maxIssueDate, dateRepaymentDate)
								.atStartOfDay(ZoneId.systemDefault()).toInstant()));
			}
			// The application is add in the CBS via API (Add API)

			try {
				// update to abacus via API
				transversClient.updateLoanINDIVORG(updateLoanDTO);
			}
			catch (Exception e) {
				logger.error("Failed to add loan {}", e.getMessage());
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

			// saving or updating UDF data in DB if not exist
			userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
					loanDTO.getUserDefinedFieldsLinksDTOs(), loanDTO.getLoanId(),
					CommonConstants.LOAN_CATEGORY);
			userDefinedFieldsLinksService.updateAbacusUdfLinksByElementId(loanDTO.getLoanId(),
					CommonConstants.LOAN_CATEGORY, loanDTO.getIdLoanExtern(), loanDTO);
			// saveOrUpdateOrDeleteUDFLoan(updateLoanDTO);

			if (!Boolean.FALSE.equals(loanDTO.getSendToIb())
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getIdIbLoan())) {
				// update ib loan
				loadDataIBService.updateAcmLoanAndCustomerInIB(updateLoanDTO);
			}
			logger.info("updateForApplication() method :: DONE");
			return updateLoanDTO;
		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS for update LOAN: {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
							new TechnicalException()),
					CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
		}
	}

	/**
	 * Assign to customer.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CustomerContactException the customer contact exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#assignToCustomer(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO assignToCustomer(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CustomerContactException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("assign loan with ID = {} ToCustomer", loanDTO.getLoanId());
		Loan oldLoan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		oldLoan.setAssignCustomer(loanDTO.getAssignCustomer());
		CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
		Loan newLoan = loanRepository.save(oldLoan);
		// update loan in IB
		loadDataIBService.updateAcmLoanAndCustomerInIB(loanDTO);
		// mapping data
		LoanDTO newLoanDTO = mapper.map(newLoan, LoanDTO.class);
		// Send Mail and communication message if loan assign to customer.
		String value = CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_ASSIGNED_TO_CUSTOMER).getValue();
		if (Boolean.TRUE.equals(newLoanDTO.getAssignCustomer())) {
			// send mail
			if (!ACMValidationUtils.isNullOrEmpty(newLoanDTO)
					&& !ACMValidationUtils.isNullOrEmpty(newLoanDTO.getCustomerDTO())
					&& !ACMValidationUtils.isNullOrEmpty(newLoanDTO.getCustomerDTO().getEmail())
					&& !ACMValidationUtils.isNullOrEmpty(newLoanDTO.getAccountNumber())
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getNote())) {
				sendMail(new MailLoanDTO(newLoanDTO,
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
								newLoanDTO.getCustomerDTO().getEmail(),
								SUBJECT_MAIL + newLoanDTO.getAccountNumber(), loanDTO.getNote()),
						MailBuilderMethod.BUILD_CLIENT_LOAN_ASSIGNED, newLoan.getUpdatedBy()));
			}

			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
			// send message
			if (!ACMValidationUtils.isNullOrEmpty(
					userClient.findByLogin(loanDTO.getCustomerDTO().getCustomerNumber()))
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO())
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getEmail())
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getNote())
					&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getId())) {
				customerContactService.saveMail(new CustomerContactDTO(
						loanDTO.getCustomerDTO().getEmail(), value, loanDTO.getNote(),
						loanDTO.getCustomerDTO().getId(), Boolean.FALSE));
			}
		}
		Integer key = CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.UPDATE_ASSIGNED_TO_CUSTOMER).getKey();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getNote())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())) {
			loanApprovalHistoriqueService.saveAndSetApprovalLabel(
					new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
							key, loanDTO.getNote(), loanDTO.getStatutWorkflow()));
		}
		return newLoanDTO;
	}

	/**
	 * Send mail.
	 * 
	 * @author Salmen Fatnassi
	 * @author AbdelkarimTurki
	 * @author HaythemBenizid
	 * @param mailLoanDTO the loanMail DTO
	 */
	private void sendMail(MailLoanDTO mailLoanDTO) {

		try {
			mailSenderClient.sendEmail(mailLoanDTO);
			logger.info("Sending Email Notification :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/**
	 * Validate issued by batch.
	 *
	 * @param loanDTOs the loan DT os
	 * @return the list
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#validateIssuedByBatch(java.util.List)
	 */
	@Override
	public List<LoanDTO> validateIssuedByBatch(List<LoanDTO> loanDTOs)
			throws UploadDocumentNotFoundException, DisbursementException,
			ConditionalApproveException, WorkFlowSettingException {

		List<LoanDTO> returnedList = new ArrayList<>();
		for (LoanDTO loanDTO : loanDTOs) {
			// load existing data of loan by id extern
			List<Loan> existLoans = loanRepository
					.findByIdLoanExternAndEnabled(loanDTO.getIdLoanExtern(), Boolean.TRUE);
			if (!ACMValidationUtils.isNullOrEmpty(existLoans)) {
				LoanDTO loan = mapper.map(existLoans.get(0), LoanDTO.class);
				loan.setIssueDate(loanDTO.getIssueDate());
				try {
					loan.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
					loan.setWorkFlowAction(
							ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_ISSUED);
					loan.setStatutLibelle(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_ISSUED)
							.getValue());
					// complete workflow for the founded loans
					returnedList.add(
							loanWorkflowUserActionService.actionInformCustomerAfterDocsSigne(loan));
				}
				catch (ResourcesNotFoundException | InformCustomerNotFoundException e) {
					logger.error("Error will validating loans : {}", e.getMessage());
				}
			}
		}
		logger.info("validate [{}] Issued Loans :: DONE", returnedList.size());
		return returnedList;
	}

	/**
	 * Find.
	 *
	 * @param reportingDTO the reporting DTO
	 * @return the reporting list DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#find(com.acm.utils.dtos.ReportingDTO)
	 */
	@Override
	public ReportingListDTO find(ReportingDTO reportingDTO) {

		Preconditions.checkNotNull(reportingDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));

		// find only INDIV / ORG
		predicate.and(qLoan.customerType.ne(CustomerType.GRP.name()));

		// find all user responsable && collaborator
		List<String> wheresOwners = new ArrayList<>();
		List<UserDTO> userDTOs = userClient.findUsers();
		userDTOs.forEach(user -> {
			if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
				wheresOwners.add(user.getLogin());
			}
		});
		// setting predicate to find by Id
		predicate.and(qLoan.owner.in(new ArrayList<>(new HashSet<>(wheresOwners))));

		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(",")).stream()
					.map(String::trim).mapToInt(Integer::parseInt).toArray();
			List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
			for (int i : arrayBranchIds) {
				listBranchIds.add(Integer.valueOf(i));
			}
			// setting predicate to find by given branch Id
			BooleanBuilder subPredicate = new BooleanBuilder();
			subPredicate.and(qLoan.branchID.in(listBranchIds));
			predicate.or(subPredicate);
		}

		// setting subPredicate to filter list participant by Id
		BooleanBuilder subPredicate = new BooleanBuilder();
		QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;
		List<LoanParticipantsDTO> loanParticipantsDTOs =
				loanParticipantsService.find(new LoanParticipantsDTO(null, userDTO.getLogin()));
		if (loanParticipantsDTOs.size() <= 1000) {
			List<Long> wheresIds = new ArrayList<>();
			loanParticipantsDTOs
					.forEach(loanParticipantsDTO -> wheresIds.add(loanParticipantsDTO.getIdLoan()));
			subPredicate.and(qLoan.idLoan.in(new ArrayList<>(new HashSet<>(wheresIds))));
		}
		else {
			subPredicate.and(qLoan.idLoan.in(
					JPAExpressions.selectFrom(qLoanParticipants).select(qLoanParticipants.idLoan)
							.where(qLoanParticipants.username.eq(userDTO.getLogin()))));
		}
		predicate.or(subPredicate);

		// find loan in list product
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getProductDTOs())) {
			List<Integer> produsctIds = new ArrayList<>();
			reportingDTO.getProductDTOs()
					.forEach(productDTO -> produsctIds.add(productDTO.getId().intValue()));
			predicate.and(qLoan.productId.in(produsctIds));
		}

		// find loan in list branch
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getBrancheDTOs())) {
			List<Integer> brancheIds = new ArrayList<>();
			reportingDTO.getBrancheDTOs()
					.forEach(brancheDTO -> brancheIds.add(brancheDTO.getBranchID().intValue()));
			predicate.and(qLoan.branchID.in(brancheIds));
		}

		// find loan in list loan officer
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getUserDTOs())) {
			List<Long> portfolioIds = new ArrayList<>();
			reportingDTO.getUserDTOs()
					.forEach(user -> portfolioIds.add(user.getAccountPortfolioId()));
			predicate.and(qLoan.portfolioId.in(portfolioIds));
		}

		// find loan in list status
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanStatus())) {
			List<Integer> statusIds = new ArrayList<>();
			reportingDTO.getLoanStatus().forEach(status -> statusIds.add(status.getKey()));
			predicate.and(qLoan.statutWorkflow.in(statusIds));
		}

		// find by loan amount
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanAmountMin())
				&& !ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanAmountMax())) {
			predicate.and(qLoan.applyAmountTotal.goe(reportingDTO.getLoanAmountMin()));
			predicate.and(qLoan.applyAmountTotal.loe(reportingDTO.getLoanAmountMax()));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanAmountMin())) {
			predicate.and(qLoan.applyAmountTotal.goe(reportingDTO.getLoanAmountMin()));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanAmountMax())) {
			predicate.and(qLoan.applyAmountTotal.loe(reportingDTO.getLoanAmountMax()));
		}

		// find by creation date
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanCreateDateMin())
				&& !ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanCreateDateMax())) {

			java.sql.Date sqlDateMin = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanCreateDateMin()));
			java.sql.Date sqlDateMax = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanCreateDateMax()));
			predicate.and(qLoan.dateInsertion.goe(sqlDateMin));
			predicate.and(qLoan.dateInsertion.loe(sqlDateMax));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanCreateDateMin())) {
			java.sql.Date sqlDateMin = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanCreateDateMin()));
			predicate.and(qLoan.dateInsertion.goe(sqlDateMin));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanCreateDateMax())) {
			java.sql.Date sqlDateMax = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanCreateDateMax()));
			predicate.and(qLoan.dateInsertion.loe(sqlDateMax));
		}

		// find by issue date
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanIssueDateMin())
				&& !ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanIssueDateMax())) {
			java.sql.Date sqlDateMin = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanIssueDateMin()));
			java.sql.Date sqlDateMax = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanIssueDateMax()));
			predicate.and(qLoan.issueDate.goe(sqlDateMin));
			predicate.and(qLoan.issueDate.loe(sqlDateMax));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanIssueDateMin())) {
			java.sql.Date sqlDateMin = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanIssueDateMin()));
			predicate.and(qLoan.issueDate.goe(sqlDateMin));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getLoanIssueDateMax())) {
			java.sql.Date sqlDateMax = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(reportingDTO.getLoanIssueDateMax()));
			predicate.and(qLoan.issueDate.loe(sqlDateMax));
		}

		/*
		 * find by customer number => find like customerID ABACUS stored in LOAN table (vue que le
		 * format customerNumber=0***+customerID)
		 */
		if (!ACMValidationUtils.isNullOrEmpty(reportingDTO.getCustomerNumber())) {
			predicate.and(qLoan.customerId.like('%' + reportingDTO.getCustomerNumber() + '%'));
		}

		// default grouped by product
		Sort sort = new Sort(Sort.Direction.ASC, "productId");
		if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
			sort = new Sort(Sort.Direction.ASC, "branchName");
		}
		else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
			sort = new Sort(Sort.Direction.ASC, "portfolioCode");
		}

		// find data
		Iterable<Loan> iterable = loanRepository.findAll(predicate, sort);
		List<Loan> loans = new ArrayList<>();
		iterable.forEach(loans::add);

		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> {
			// mapping data
			LoanDTO loanDTO = mapper.map(loan, LoanDTO.class);

			// Find {@link List} of {@link LoanApprovalHistoriqueDTO} by given Loan ID.
			List<LoanApprovalHistoriqueDTO> loanApprovalHistoriqueDTOs =
					loanApprovalHistoriqueService.find(new LoanApprovalHistoriqueDTO(loanDTO));
			// setting last approval date
			if (!ACMValidationUtils.isNullOrEmpty(loanApprovalHistoriqueDTOs)) {
				loanDTO.setLastApprovalDate(loanApprovalHistoriqueDTOs.get(0).getApprovalDate());
			}

			// setting customer group number && role of customer
			if (loan.getParentId() != 0) {
				try {
					LoanDTO loanGroup = find(loan.getParentId());
					loanDTO.setCustomerGroupeNumber(loanGroup.getCustomerDTO().getCustomerNumber());

					List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs =
							customerLinksRelationshipService.find(new CustomerLinksRelationshipDTO(
									loanGroup.getCustomerDTO().getId(), loanDTO.getCustomerDTO(),
									LinkRelationshipsCategory.MEMBERS.name()));
					// setting role customer
					if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTOs)) {
						loanDTO.setCustomerRole(
								customerLinksRelationshipDTOs.get(0).getLinkRelationshipType());
					}
				}
				catch (ResourcesNotFoundException e) {
					logger.error("{}", e.getMessage());
				}
			}

			// init list
			loanDTOs.add(loanDTO);
		});
		logger.info("METHOD : find for reporting : {} : Loan was founded", loanDTOs.size());

		ReportingListDTO reportingListDTO = new ReportingListDTO();
		if (Boolean.TRUE.equals(reportingDTO.getProduct())) {
			buildReportDataGroupByProduct(loanDTOs, reportingListDTO);
		}
		else if (Boolean.TRUE.equals(reportingDTO.getLoanOfficer())) {
			buildReportDataGroupByLoanOfficer(loanDTOs, reportingListDTO);
		}
		else if (Boolean.TRUE.equals(reportingDTO.getBranch())) {
			buildReportDataGroupByBranch(loanDTOs, reportingListDTO);
		}
		else {
			// default by product
			buildReportDataGroupByProduct(loanDTOs, reportingListDTO);
		}
		return reportingListDTO;
	}

	/**
	 * Builds the report data group by product.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @param reportingListDTO the reporting list DTO
	 * @return the reporting list DTO
	 */
	private ReportingListDTO buildReportDataGroupByProduct(List<LoanDTO> loanDTOs,
			ReportingListDTO reportingListDTO) {

		List<LoanDTO> loanDTOListFiltered =
				loanDTOs.stream().filter(CommonFunctions.distinctByKey(LoanDTO::getProductId))
						.collect(Collectors.toList());
		logger.info("List PRODUCT : {} ", loanDTOListFiltered.size());
		List<ReportingListGroupByDTO> reportingListGroupByDTOs = new ArrayList<>();
		// init response list && calculate TOTAL
		for (LoanDTO loanProduct : loanDTOListFiltered) {
			Integer selectedProductId = loanProduct.getProductId();
			ReportingListGroupByDTO reportingListGroupByDTO = new ReportingListGroupByDTO();
			List<LoanDTO> loanDTOListFilteredByProduct =
					loanDTOs.stream().filter(l -> l.getProductId() == selectedProductId)
							.collect(Collectors.toList());
			logger.info("loanDTOListFilteredByProduct by {} = {} ", selectedProductId,
					loanDTOListFilteredByProduct.size());
			reportingListGroupByDTO.setLoanDTOs(loanDTOListFilteredByProduct);
			// setting TotalRecords
			reportingListGroupByDTO.setTotalRecords(loanDTOListFilteredByProduct.size());
			// Calculate && setting totalAmount / totalIssueAmount
			Long totalAmount = 0L;
			Long totalIssueAmount = 0L;
			for (LoanDTO l : loanDTOListFilteredByProduct) {
				totalAmount += l.getApplyAmountTotal().longValue();
				totalIssueAmount += l.getApprovelAmount().longValue();
			}
			reportingListGroupByDTO.setTotalAmount(totalAmount);
			reportingListGroupByDTO.setTotalIssueAmount(totalIssueAmount);
			reportingListGroupByDTOs.add(reportingListGroupByDTO);
		}
		// init response object
		reportingListDTO.setReportingListGroupByDTOs(reportingListGroupByDTOs);
		// setting TotalRecords
		reportingListDTO.setTotalRecords(loanDTOs.size());
		// Calculate && setting totalAmount / totalIssueAmount
		Long totalAmount = 0L;
		Long totalIssueAmount = 0L;
		for (LoanDTO l : loanDTOs) {
			totalAmount += l.getApplyAmountTotal().longValue();
			totalIssueAmount += l.getApprovelAmount().longValue();
		}
		reportingListDTO.setTotalAmount(totalAmount);
		reportingListDTO.setTotalIssueAmount(totalIssueAmount);

		return reportingListDTO;
	}

	/**
	 * Builds the report data group by branch.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @param reportingListDTO the reporting list DTO
	 * @return the reporting list DTO
	 */
	private ReportingListDTO buildReportDataGroupByBranch(List<LoanDTO> loanDTOs,
			ReportingListDTO reportingListDTO) {

		List<LoanDTO> loanDTOListFiltered =
				loanDTOs.stream().filter(CommonFunctions.distinctByKey(LoanDTO::getBranchID))
						.collect(Collectors.toList());
		logger.info("List  Branch : {} ", loanDTOListFiltered.size());
		List<ReportingListGroupByDTO> reportingListGroupByDTOs = new ArrayList<>();
		// init response list && calculate TOTAL
		for (LoanDTO loan : loanDTOListFiltered) {
			Integer selectedBranchId = loan.getBranchID();
			ReportingListGroupByDTO reportingListGroupByDTO = new ReportingListGroupByDTO();
			List<LoanDTO> loanDTOListFilteredByBranch = loanDTOs.stream()
					.filter(l -> l.getBranchID() == selectedBranchId).collect(Collectors.toList());
			logger.info("loanDTOListFilteredByBranch by {} = {} ", selectedBranchId,
					loanDTOListFilteredByBranch.size());
			reportingListGroupByDTO.setLoanDTOs(loanDTOListFilteredByBranch);
			// setting TotalRecords
			reportingListGroupByDTO.setTotalRecords(loanDTOListFilteredByBranch.size());
			// Calculate && setting totalAmount / totalIssueAmount
			Long totalAmount = 0L;
			Long totalIssueAmount = 0L;
			for (LoanDTO l : loanDTOListFilteredByBranch) {
				totalAmount += l.getApplyAmountTotal().longValue();
				totalIssueAmount += l.getApprovelAmount().longValue();
			}
			reportingListGroupByDTO.setTotalAmount(totalAmount);
			reportingListGroupByDTO.setTotalIssueAmount(totalIssueAmount);
			reportingListGroupByDTOs.add(reportingListGroupByDTO);
		}
		// init response object
		reportingListDTO.setReportingListGroupByDTOs(reportingListGroupByDTOs);
		// setting TotalRecords
		reportingListDTO.setTotalRecords(loanDTOs.size());
		// Calculate && setting totalAmount / totalIssueAmount
		Long totalAmount = 0L;
		Long totalIssueAmount = 0L;
		for (LoanDTO l : loanDTOs) {
			totalAmount += l.getApplyAmountTotal().longValue();
			totalIssueAmount += l.getApprovelAmount().longValue();
		}
		reportingListDTO.setTotalAmount(totalAmount);
		reportingListDTO.setTotalIssueAmount(totalIssueAmount);
		return reportingListDTO;
	}

	/**
	 * Builds the report data group by loan officer.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTOs the loan DT os
	 * @param reportingListDTO the reporting list DTO
	 * @return the reporting list DTO
	 */
	private ReportingListDTO buildReportDataGroupByLoanOfficer(List<LoanDTO> loanDTOs,
			ReportingListDTO reportingListDTO) {

		List<LoanDTO> loanDTOListFiltered =
				loanDTOs.stream().filter(CommonFunctions.distinctByKey(LoanDTO::getPortfolioCode))
						.collect(Collectors.toList());
		logger.info("List LoanOfficer : {} ", loanDTOListFiltered.size());
		List<ReportingListGroupByDTO> reportingListGroupByDTOs = new ArrayList<>();
		// init response list && calculate TOTAL
		for (LoanDTO loan : loanDTOListFiltered) {
			String selectedPortfolioCode = loan.getPortfolioCode();
			ReportingListGroupByDTO reportingListGroupByDTO = new ReportingListGroupByDTO();
			List<LoanDTO> loanDTOListFilteredByPortfolioCode = loanDTOs.stream()
					.filter(l -> l.getPortfolioCode().equals(selectedPortfolioCode))
					.collect(Collectors.toList());
			logger.info("loanDTOListFilteredByLoan officer by {} = {} ", selectedPortfolioCode,
					loanDTOListFilteredByPortfolioCode.size());
			reportingListGroupByDTO.setLoanDTOs(loanDTOListFilteredByPortfolioCode);
			// setting TotalRecords
			reportingListGroupByDTO.setTotalRecords(loanDTOListFilteredByPortfolioCode.size());
			// Calculate && setting totalAmount / totalIssueAmount
			Long totalAmount = 0L;
			Long totalIssueAmount = 0L;
			for (LoanDTO l : loanDTOListFilteredByPortfolioCode) {
				totalAmount += l.getApplyAmountTotal().longValue();
				totalIssueAmount += l.getApprovelAmount().longValue();
			}
			reportingListGroupByDTO.setTotalAmount(totalAmount);
			reportingListGroupByDTO.setTotalIssueAmount(totalIssueAmount);
			reportingListGroupByDTOs.add(reportingListGroupByDTO);
		}
		// init response object
		reportingListDTO.setReportingListGroupByDTOs(reportingListGroupByDTOs);
		// setting TotalRecords
		reportingListDTO.setTotalRecords(loanDTOs.size());
		// Calculate && setting totalAmount / totalIssueAmount
		Long totalAmount = 0L;

		Long totalIssueAmount = 0L;
		for (LoanDTO l : loanDTOs) {
			totalAmount += l.getApplyAmountTotal().longValue();
			totalIssueAmount += l.getApprovelAmount().longValue();
		}
		reportingListDTO.setTotalAmount(totalAmount);
		reportingListDTO.setTotalIssueAmount(totalIssueAmount);
		return reportingListDTO;
	}

	/**
	 * Update group application.
	 *
	 * @param loanDTOs the loan DT os
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateForApplicationForGroup(java.util.List)
	 */
	@Override
	public LoanDTO updateGroupApplication(List<LoanDTO> loanDTOs)
			throws ResourcesNotFoundException, ApiAbacusException, IOException {

		List<LoanDTO> updatedLoanDTOs = new ArrayList<>();
		LoanDTO groupLoanDTO = new LoanDTO();
		List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs = new ArrayList<>();
		// Update ACM
		for (LoanDTO loanDTO : loanDTOs) {
			if (Boolean.TRUE.equals(loanDTO.getChanged())) {
				loanDTO.setUpdateLoan(Boolean.TRUE);
				LoanDTO updateLoanDTO = save(loanDTO.getLoanId(), loanDTO);

				// Update UDF
				for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : loanDTO
						.getUserDefinedFieldsLinksDTOs()) {
					userDefinedFieldsLinksDTO.setLoanId(updateLoanDTO.getLoanId());
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
				}

				userDefinedFieldsLinksDTOs = loanDTO.getUserDefinedFieldsLinksDTOs();
				updateLoanDTO
						.setUserDefinedFieldsLinksDTOs(loanDTO.getUserDefinedFieldsLinksDTOs());
				updatedLoanDTOs.add(updateLoanDTO);
			}
			else if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				groupLoanDTO = loanDTO;
			}
			else {
				// find by loan id
				UserDefinedFieldsLinksDTO params = new UserDefinedFieldsLinksDTO();
				params.setLoanId(loanDTO.getLoanId());
				userDefinedFieldsLinksDTOs.addAll(userDefinedFieldsLinksService.find(params));
				updatedLoanDTOs.add(loanDTO);
			}
		}
		try {
			// setting all udf childs
			groupLoanDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksDTOs);
			// setting group parent
			updatedLoanDTOs.add(groupLoanDTO);
			// update to abacus via API
			transversClient.updateLoanGRP(updatedLoanDTOs);

			// ADD / UPDATE from ABACUS by ID account Extern
			for (LoanDTO loan : updatedLoanDTOs) {
				// saving or updating UDF data in DB if not exist
				saveOrUpdateOrDeleteUDFLoan(loan);

			}
			return groupLoanDTO;
		}
		catch (Exception e) {
			logger.error("Failed to update loan grp {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
	}

	/**
	 * Cancel loans.
	 *
	 * @param listIdLoanExtern the list id loan extern
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#cancelLoans(java.util.List)
	 */
	@Override
	public List<LoanDTO> cancelLoans(List<Long> listIdLoanExtern) {

		logger.info("START cancelLoans() method");
		List<LoanDTO> canceledLoans = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(listIdLoanExtern)) {
			// init list status
			List<Integer> listStatut = new ArrayList<>();
			listStatut.add(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
			listStatut.add(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
			listStatut.add(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey());
			listStatut
					.add(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey());

			// load list Loan by ByIdLoanExtern In given list And StatutWorkflow Not In
			// given status
			List<Loan> loans = loanRepository
					.findByIdLoanExternInAndStatutWorkflowNotIn(listIdLoanExtern, listStatut);

			if (!ACMValidationUtils.isNullOrEmpty(loans)) {

				loans.forEach(loan -> {
					// set statut tab dashboard to cancelled
					loan.setStatut(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED)
							.getKey());
					// set statut workflow loan to cancelled
					loan.setStatutWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
					loan.setStatutLibelle(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getValue());
					// setting date change status
					loan.setChangeDateStatusWorkflow(new Date());
					loan.setCategory(CommonConstants.CATEGORY_INSTANCE);

					Loan updatedLoan = loanRepository.save(loan);
					// mappin data
					LoanDTO canceledLoanDTO = mapper.map(updatedLoan, LoanDTO.class);
					canceledLoans.add(canceledLoanDTO);

					// update statut for loan child
					try {
						updateStatutChild(canceledLoanDTO, ACMConstantWorkflowStatuts.CANCELLED,
								ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED);
					}
					catch (ResourcesNotFoundException e) {
						logger.error("Failed to update child status");
					}

					logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
				});
			}
			logger.info("cancelLoans() method :: DONE");
		}

		return canceledLoans;
	}

	/**
	 * Validate ready for disbursement.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws DisbursementException the disbursement exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#validateReadyForDisbursement(com.acm.utils.dtos. LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.VALIDATE_READY_FOR_DISBURSEMENT)
	public LoanDTO validateReadyForDisbursement(LoanDTO loanDTO) throws ApiAbacusException,
			IOException, ResourcesNotFoundException, DisbursementException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getProductId(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// Query Abacus to check if loan was issued.
		List<Long> ids = new ArrayList<>();
		ids.add(loanDTO.getIdLoanExtern());
		List<LoanDTO> loanDTOs = transversClient.findIssuedLoansbyIdExterne(ids);
		if (!ACMValidationUtils.isNullOrEmpty(loanDTOs)) {
			throw new DisbursementException(CommonErrorCode.LOAN_ALREADY_ISSUED,
					"LOAN ALREDY ISSUED");
		}

		try {
			// find product object by ID
			loanDTO.setProductDTO(
					parametrageClient.findProductById(loanDTO.getProductId().longValue()));
			// find UDF for loan
			UserDefinedFieldsLinksDTO params = new UserDefinedFieldsLinksDTO();
			params.setLoanId(loanDTO.getLoanId());
			loanDTO.setUserDefinedFieldsLinksDTOs(userDefinedFieldsLinksService.find(params));
			// get initial payment date from the setting
			Integer maxIssueDate =
					Integer.parseInt(parametrageClient.find("ISSUE_DATE").getValue());
			Integer dateRepaymentDate =
					Integer.parseInt(parametrageClient.find("INITIAL_PAYMENT").getValue());
			if (maxIssueDate != 0 && dateRepaymentDate != 0) {
				loanDTO.setInitialPaymentDate(
						Date.from(calculateFirstRepaymentDate(maxIssueDate, dateRepaymentDate)
								.atStartOfDay(ZoneId.systemDefault()).toInstant()));
			}
			// update && call Approval API
			updateAndApprovalLoan(loanDTO, 2);
		}
		catch (Exception e) {
			logger.error("Failed {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
		// set the loan as ready for disbursement
		loanDTO.setStatutWorkflow(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey());
		// update loan data
		save(loanDTO.getLoanId(), loanDTO);
		logger.info("Method validateReadyForDisbursement():: DONE");
		return loanDTO;
	}

	/**
	 * Update and approval loan.
	 * 
	 * @author Ines Dridi
	 * @param loanDTO the loan DTO
	 * @param levelApproval the level approval
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	private void updateAndApprovalLoan(LoanDTO loanDTO, Integer levelApproval)
			throws IOException, ResourcesNotFoundException, ApiAbacusException {

		logger.info("LOAN APPROVAL Process IN ABACUS via API : LEVEL [{}]", levelApproval);
		try {
			// init first level process step
			loanDTO.setLoanApprovalLevel(levelApproval);
			if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				List<LoanDTO> loanDTOs = new ArrayList<>();
				loanDTOs.add(loanDTO);
				// load group childs
				List<LoanDTO> loansChilds = findByParentId(loanDTO.getLoanId());
				// setting approval level for all childs
				for (LoanDTO child : loansChilds) {
					child.setLoanApprovalLevel(levelApproval);
					child.setProductDTO(loanDTO.getProductDTO());
					// add child
					loanDTOs.add(child);
				}
				// find customer object
				CustomerDTO customerDTO = customerService.find(loanDTO.getCustomerDTO().getId());

				// update CUSTOMER data via API ABACUS
				transversClient.updateCustomer(customerDTO);

				// Send APPROVAL && update API
				transversClient.approvelGroup(loanDTOs);
			}
			else {
				// find customer object
				CustomerDTO customerDTO = customerService.find(loanDTO.getCustomerDTO().getId());
				// update CUSTOMER data via API ABACUS
				transversClient.updateCustomer(customerDTO);
				// Send approval && update API loan ORG or INDIV
				transversClient.approveLoan(loanDTO);
			}
		}
		catch (Exception e) {
			logger.error("Failed {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
			final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
					jsonNode.get("errorMessage").asText());
		}
	}

	/**
	 * Check loan status issued.
	 *
	 * @param idLoanExtern the id loan extern
	 * @return the boolean
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#checkLoanStatusIssued(java.lang.Long)
	 */
	@Override
	public Boolean checkLoanStatusIssued(Long idLoanExtern) {

		Preconditions.checkNotNull(idLoanExtern, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Boolean issued = Boolean.FALSE;
		// check loan status issued
		List<LoanDTO> issuedLoan = transversClient.findLoanStatusIssued(idLoanExtern);
		// return true if loan status is issued
		if (!ACMValidationUtils.isNullOrEmpty(issuedLoan)) {
			issued = Boolean.TRUE;
		}
		return issued;
	}

	/**
	 * Update status.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateStatus(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.REVERSED_LOAN)
	public LoanDTO updateStatus(LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update statut loan  with ID = {}", loanDTO.getLoanId());
		Loan oldLoan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}
		oldLoan.setEtapeWorkflow(loanDTO.getEtapeWorkflow());
		oldLoan.setStatutLibelle(loanDTO.getStatutLibelle());
		oldLoan.setStatutWorkflow(loanDTO.getStatutWorkflow());
		oldLoan.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED).getKey());
		CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
		Loan newLoan = loanRepository.save(oldLoan);

		// mapping data
		LoanDTO newLoanDTO = mapper.map(newLoan, LoanDTO.class);

		Integer key =
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LOAN_REVERSER).getKey();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())) {
			loanApprovalHistoriqueService.saveAndSetApprovalLabel(
					new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
							key, "Loan has been reverser", loanDTO.getStatutWorkflow()));
		}
		logger.info("update statut loan() method :: DONE");
		return newLoanDTO;
	}

	/**
	 * Check loan status.
	 *
	 * @param loanDTO the loan DTO
	 * @return the boolean
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#checkLoanStatus(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public Boolean checkLoanStatus(LoanDTO loanDTO) {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Boolean issued = Boolean.FALSE;
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getId())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getStatutWorkflow())) {
			List<Loan> issuedLoan = loanRepository.findByCustomerAndStatutWorkflowAndEnabled(
					new Customer(loanDTO.getCustomerDTO().getId()), loanDTO.getStatutWorkflow(),
					Boolean.TRUE);
			// return true if loan status is issued
			if (!ACMValidationUtils.isNullOrEmpty(issuedLoan)) {
				issued = Boolean.TRUE;
			}
		}
		// check loan status issued
		return issued;
	}

	/**
	 * Gets the page.
	 *
	 * @param inputList the input list
	 * @param pageNumber the page number
	 * @param pageSize the page size
	 * @return the page
	 */
	public static List<String> getPage(List<String> inputList, int pageNumber, int pageSize) {

		int startIndex = (pageNumber) * pageSize;
		int endIndex = Math.min(startIndex + pageSize, inputList.size());

		if (startIndex < inputList.size()) {
			return inputList.subList(startIndex, endIndex);
		}
		else {
			return null; // Return null or handle out-of-range page numbers as needed.
		}
	}

	/**
	 * Find by owners.
	 *
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByOwners()
	 */
	@Override
	public List<LoanDTO> findByOwners() {

		// find all user responsable && collaborator
		List<Loan> loans = new ArrayList<>();
		List<String> owners = new ArrayList<>();
		List<UserDTO> userDTOs = userClient.findUsers();
		userDTOs.forEach(user -> {
			if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
				owners.add(user.getLogin());
			}
		});
		// find
		if (owners.size() > 2000) {
			double result = (double) owners.size() / 2000;
			for (int i = 0; i < Math.ceil(result); i++) {
				loans.addAll(loanRepository.findByOwnerInAndEnabled(getPage(owners, i, 2000),
						Boolean.TRUE));

			}
		}
		else {
			loans = loanRepository.findByOwnerInAndEnabled(owners, Boolean.TRUE);
		}

		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loans)) {
			return new ArrayList<>();
		}
		// mapping founded data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("METHOD : findByOwners : {} : Loan was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Find un assignment.
	 *
	 * @param loanPaginationDTO the loan pagination DTO
	 * @return the loan pagination DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findUnAssignment(com.acm.utils.dtos.pagination.
	 * LoanPaginationDTO)
	 */
	@Override
	public LoanPaginationDTO findUnAssignment(LoanPaginationDTO loanPaginationDTO) {

		Preconditions.checkNotNull(loanPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getPageNumber())) {
			loanPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getPageSize())) {
			loanPaginationDTO.setPageSize(10);
		}
		// setting default data
		loanPaginationDTO.setResultsLoans(new ArrayList<>());
		// setting default totals pages
		loanPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		loanPaginationDTO.setTotalPages(0);
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQueryUnassignedLoan(loanPaginationDTO.getParams(), qLoan);

		// find loan by customerIdExtern
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getCustomerId())) {
			predicate.and(qLoan.customerId.eq(loanPaginationDTO.getParams().getCustomerId()));
		}
		// find loan by customerId
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getCustomerDTO())
				&& !ACMValidationUtils
						.isNullOrEmpty(loanPaginationDTO.getParams().getCustomerDTO().getId())) {
			predicate.and(qLoan.customer
					.eq(new Customer(loanPaginationDTO.getParams().getCustomerDTO().getId())));
		}
		// find loan by accountNumber
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getAccountNumber())) {
			predicate.and(qLoan.accountNumberExtern
					.like(loanPaginationDTO.getParams().getAccountNumber() + "%"));
		}

		// setting pagination filter
		// find loan by accountNumber
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getAccountNumber())) {
			predicate.and(qLoan.accountNumberExtern
					.like("%" + loanPaginationDTO.getParams().getAccountNumber() + "%"));
		}
		// loanParams.productDescription
		if (!ACMValidationUtils
				.isNullOrEmpty(loanPaginationDTO.getParams().getProductDescription())) {
			predicate.and(qLoan.productDescription
					.like("%" + loanPaginationDTO.getParams().getProductDescription() + "%"));
		}
		// loanParams.customerName
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getCustomerName())) {
			List<Long> wheresIdCustomer = new ArrayList<>();
			CustomerDTO params = new CustomerDTO();
			params.setCustomerNumber(loanPaginationDTO.getParams().getCustomerName());
			List<CustomerDTO> customerDTOs = customerService.find(params);
			customerDTOs.forEach(customer -> wheresIdCustomer.add(customer.getId()));
			// find like CUSTOMERNAME OR like CUSTOMERNUMBER
			if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
				predicate.and(qLoan.customer.id.in(wheresIdCustomer));
			}
			else {
				StringTemplate convertedCustomerName = Expressions
						.stringTemplate("function('replace', {0}, '|', ' ')", qLoan.customerName);
				predicate.and(convertedCustomerName
						.like("%" + loanPaginationDTO.getParams().getCustomerName() + "%"));
			}
		}
		// loanParams.applyAmountTotal
		if (!ACMValidationUtils
				.isNullOrEmpty(loanPaginationDTO.getParams().getApplyAmountTotal())) {
			predicate.and(
					qLoan.applyAmountTotal.eq(loanPaginationDTO.getParams().getApplyAmountTotal()));
		}
		// loanParams.applyDate
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getApplyDate())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(DateUtil
					.convertToLocalDateViaInstant(loanPaginationDTO.getParams().getApplyDate()));
			predicate.and(qLoan.applyDate.eq(sqlDate));
		}
		// loanParams.portfolioDescription
		if (!ACMValidationUtils
				.isNullOrEmpty(loanPaginationDTO.getParams().getPortfolioDescription())) {
			predicate.and(qLoan.portfolioDescription
					.like("%" + loanPaginationDTO.getParams().getPortfolioDescription() + "%"));
		}

		// loanParams.statutWorkflow
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getStatutWorkflow())) {
			predicate.and(
					qLoan.statutWorkflow.eq(loanPaginationDTO.getParams().getStatutWorkflow()));
		}

		// loanParams.statutLibelle
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getStatutLibelle())) {
			predicate.and(qLoan.statutLibelle.eq(loanPaginationDTO.getParams().getStatutLibelle()));
		}

		// loanParams.dateLastUpdate
		if (!ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getParams().getDateLastUpdate())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
					loanPaginationDTO.getParams().getDateLastUpdate()));
			predicate.and(qLoan.dateLastUpdate.eq(sqlDate));
		}
		// find only parent loan
		predicate.and(qLoan.parentId.eq(0L));
		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(loanPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getSortField())) {
			// NB : le test est fait sur le field "accountNumber" puisque le nom de field
			// n'est le
			// meme dans LoanDTO <=> Loan
			// customerNameNoPipe => customerName
			String sortedField = loanPaginationDTO.getSortField();
			if (loanPaginationDTO.getSortField().equals("accountNumber")) {
				sortedField = "accountNumberExtern";
			}
			if (loanPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		}
		else if ("-1".equals(loanPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(loanPaginationDTO.getSortField())) {

			String sortedField = loanPaginationDTO.getSortField();
			if (loanPaginationDTO.getSortField().equals("accountNumber")) {
				sortedField = "accountNumberExtern";
			}
			if (loanPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		}
		else {
			// default sort by applyDate : DESC
			pageable = PageRequest.of(loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateLastUpdate");
		}

		// load data
		Page<Loan> pagedResult = loanRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Loan> loans = pagedResult.getContent();
			logger.info(
					"find by pagination method : {} : Loan was founded (PageNumber = {} / PageSize = {} )",
					loans.size(), loanPaginationDTO.getPageNumber(),
					loanPaginationDTO.getPageSize());
			List<LoanDTO> loanDTOs = new ArrayList<>();
			loans.forEach(loan -> {
				LoanDTO loanResultat = mapper.map(loan, LoanDTO.class);
				// find product object by ID
				loanResultat.setProductDTO(
						parametrageClient.findProductById(loanResultat.getProductId().longValue()));
				loanDTOs.add(loanResultat);
			});
			// setting data
			loanPaginationDTO.setResultsLoans(loanDTOs);
			// setting totals pages
			loanPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			loanPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return loanPaginationDTO;
	}

	/**
	 * Load filter product for unassigned loans.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#loadFilterProductForUnassignedLoans(com.acm.utils
	 * .dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> loadFilterProductForUnassignedLoans(LoanDTO loanDTO) {

		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQueryUnassignedLoan(loanDTO, qLoan);

		// SELECT distinct productDescription
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<Loan> loans =
				queryFactory.select(Projections.bean(Loan.class, qLoan.productDescription))
						.distinct().from(qLoan).where(predicate).fetch();
		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("{} : Products to use in Filter dashboard was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Load filter status for unassigned loans.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#loadFilterProductForUnassignedLoans(com.acm.utils
	 * .dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> loadFilterStatusForUnassignedLoans(LoanDTO loanDTO) {

		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQueryUnassignedLoan(loanDTO, qLoan);

		// SELECT distinct productDescription
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<Loan> loans = queryFactory.select(Projections.bean(Loan.class, qLoan.statutLibelle))
				.distinct().from(qLoan).where(predicate).fetch();
		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("{} : Products to use in Filter dashboard was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Load filter branch.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#loadFilterBranch(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> loadFilterBranch(LoanDTO loanDTO) {

		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(loanDTO, qLoan);
		// SELECT distinct branches
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<Loan> loans = queryFactory.select(Projections.bean(Loan.class, qLoan.branchName))
				.distinct().from(qLoan).where(predicate).fetch();

		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("{} : branch to use in Filter dashboard was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Count unassigned loans.
	 *
	 * @return the long
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#countUnassignedLoans(java.lang.String)
	 */
	@Override
	public Long countUnassignedLoans() {

		Long count = 0L;

		// INIT QLoan
		QLoan qLoan = QLoan.loan;
		// INIT params
		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setParentId(0L);

		// execute query
		count = loanRepository.count(buildQueryUnassignedLoan(loanDTO, qLoan));
		logger.info("Returning COUNT = {} ", count);
		return count;
	}

	/**
	 * Builds the query unassigned loan.
	 *
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @param qLoan the q loan
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQueryUnassignedLoan(LoanDTO loanDTO, QLoan qLoan) {

		BooleanBuilder predicate = new BooleanBuilder();
		BooleanBuilder subOwnerPredicate = new BooleanBuilder();
		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		GroupeDTO groupeDTO = userDTO.getGroupes().iterator().next();

		List<Long> listLoanId = acmLoanInstanceGroupeAssociationRepository
				.findUnassaingedApprovalLoansByGroup(groupeDTO.getCode());

		// find loans with an empty owner

		// check if the group of user connected is not empty
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO)
				&& !ACMValidationUtils.isNullOrEmpty(groupeDTO.getCode())) {
			// find only loans without owner
			subOwnerPredicate.and(qLoan.owner.isNull());
			// find loans by group owner of connected user
			subOwnerPredicate.and(qLoan.groupOwner.eq(groupeDTO.getCode()));
			subOwnerPredicate.or(qLoan.idLoan.in(listLoanId));
			predicate.and(subOwnerPredicate);

			// find unassigned loans by access branch
			if (userDTO.getCategory() != null
					&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
					&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
				int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(","))
						.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
				List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
				for (int i : arrayBranchIds) {
					listBranchIds.add(Integer.valueOf(i));
				}
				listBranchIds.add(userDTO.getBranchID());
				// setting predicate to find by given branch Id
				predicate.and(qLoan.branchID.in(listBranchIds));
			}
		}
		// for the TAB "My Tasks" Excluded statutWorkflow :
		// REJECTED / CANCELLED or DECLINE
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()));
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey()));
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey()));

		// find loan by parentId
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getParentId())) {
			predicate.and(qLoan.parentId.eq(loanDTO.getParentId()));
		}
		return predicate;
	}

	/**
	 * Assign loan.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#assignLoan(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO assignLoan(LoanDTO loanDTO) throws ResourcesNotFoundException, CreditException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// load loan data by ID
		Loan loan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (loan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId());
		}

		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// throw error if the loan already assigned by another user
		if (!ACMValidationUtils.isNullOrEmpty(loan.getOwner())
				|| !ACMValidationUtils.isNullOrEmpty(loan.getOwnerName())) {

			// Find & Update the Association table
			GroupeDTO groupeDTO = userDTO.getGroupes().iterator().next();

			List<AcmLoanInstanceAcmGroupeApproval> listApprovalGroup =
					acmLoanInstanceGroupeAssociationRepository
							.findUnassaingedApprovalAssocByGroupAndEtapeWf(groupeDTO.getCode(),
									loanDTO.getLoanId());

			if (!ACMValidationUtils.isNullOrEmpty(listApprovalGroup)) {
				AcmLoanInstanceAcmGroupeApproval acmLoanInstanceGroupeAssociation =
						listApprovalGroup.get(0);
				acmLoanInstanceGroupeAssociation.setOwner(userDTO.getLogin());
				acmLoanInstanceGroupeAssociation.setOwnerName(userDTO.getSimpleName());

				// Update setting collection third party

				acmLoanInstanceGroupeAssociationRepository.save(acmLoanInstanceGroupeAssociation);

				Loan updatedLoan = loanRepository.getOne(loanDTO.getLoanId());
				LoanDTO loanDTOResult = mapper.map(updatedLoan, LoanDTO.class);
				loanDTOResult.setProductDTO(loanDTO.getProductDTO());
				logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
				return loanDTOResult;
			}
			else {

				throw new CreditException(
						new ExceptionResponseMessage(CommonErrorCode.LOAN_ALREADY_ASSIGNED,
								CommonExceptionsMessage.LOAN_ALREADY_ASSIGNED,
								new TechnicalException()),
						CommonExceptionsMessage.LOAN_ALREADY_ASSIGNED);
			}
		}
		else {

			// assign the loan to the connected user
			loan.setOwner(userDTO.getLogin());
			loan.setOwnerName(userDTO.getSimpleName());
			loan.setGroupOwner(null);
			loan.setGroupOwnerName(null);

			// update list participants
			loanParticipantsService
					.update(new LoanParticipantsDTO(loan.getIdLoan(), loan.getOwner()));

			// setting list participants
			LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
					.save(new LoanParticipantsDTO(loan.getIdLoan(), loan.getOwner()));
			logger.info("setting Loan Participants with ID = [{}] :: DONE",
					newLoanParticipantsDTO.getId());

			CommonFunctions.mapperToUpdate(loan, userClient, logger);
			Loan updatedLoan = loanRepository.save(loan);
			LoanDTO loanDTOResult = mapper.map(updatedLoan, LoanDTO.class);
			loanDTOResult.setProductDTO(loanDTO.getProductDTO());
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
			// In the case where the user assigns to himself the loan ==> we must close the
			// open tasks for other members of the group and we keep the task open only
			// for this user
			CalendarEventDTO calendarEventDTO = new CalendarEventDTO();
			calendarEventDTO.setUsername(loan.getOwner());
			calendarEventDTO.setIdLoanExtern(loan.getIdLoanExtern());
			crmClient.createLoanTaskAndCloseOldTask(calendarEventDTO, Boolean.FALSE, Boolean.TRUE,
					Boolean.FALSE);

			return loanDTOResult;
		}

	}

	/**
	 * Synchronize issued loans.
	 *
	 * @return the integer
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws Exception the exception
	 */
	/*
	 * (non-Javadoc).
	 * @return the integer
	 * @see com.acm.service.LoanService#synchronizeIssuedLoans()
	 */
	@Override
	public Integer synchronizeIssuedLoans() throws UploadDocumentNotFoundException, Exception {

		// find loan where status = DISBURSEMENT_CASE_CLOSURE (key = 13)
		List<LoanDTO> loansdDtos = findByStatutWorkflow(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey());
		// get list of loan id extern
		List<Long> listIdLoanExtern = new ArrayList<>();
		loansdDtos.forEach(loanDTO -> listIdLoanExtern.add(loanDTO.getIdLoanExtern()));
		try {
			// select CULoanID from culoan where status =4 from ABACUS
			List<LoanDTO> issuedLoans =
					transversClient.findIssuedLoansbyIdExterne(listIdLoanExtern);
			logger.debug("founded issued loans NB = {}", issuedLoans.size());
			// validate && complete workflow founded loan
			if (!ACMValidationUtils.isNullOrEmpty(issuedLoans)) {
				// validate loans
				List<LoanDTO> returnedLoans = validateIssuedByBatch(issuedLoans);
				String subject = "Validate & Issued [ " + returnedLoans.size()
						+ " ] LOAN from ABACUS-DB successfully";
				String content = "Account Number of issued Loans";
				// send notification mail
				sendMail(returnedLoans, subject, content);
				// Add Guarantor in Abacus For Topuped loans
				for (LoanDTO issuedLoan : returnedLoans) {
					if (issuedLoan.getLoanApplicationStatus().equals(CommonConstants.TOPUP)
							|| issuedLoan.getLoanApplicationStatus()
									.equals(CommonConstants.REFINANCE)) {
						CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
						params.setIdLoan(issuedLoan.getLoanId());
						params.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						List<CustomerLinksRelationshipDTO> existGuarantors =
								customerLinksRelationshipService.find(params);
						// check guarantor exist in ABACUS DB
						if (!ACMValidationUtils.isNullOrEmpty(existGuarantors)
								&& existGuarantors.get(0).getMember() != null) {
							// init id customer extern
							Long idCustomerIdExtern =
									existGuarantors.get(0).getMember().getCustomerIdExtern();
							// check customer exist in ABACUS
							if (idCustomerIdExtern == 0) {
								// add guarantor as customer in ABACUS DB
								CustomerDTO guarantor = transversClient
										.addCustomer(existGuarantors.get(0).getMember());
								idCustomerIdExtern = guarantor.getCustomerIdExtern();
							}
							// setting Customer Id Extern
							GuarantorDTO newGuarantor = transversClient
									.createGuarantor(new GuarantorDTO(issuedLoan.getIdLoanExtern(),
											issuedLoan.getIdAccountExtern(), idCustomerIdExtern, "",
											null, 1L, existGuarantors.get(0).getAmountGuarantor(),
											0L));
							logger.info("{}", newGuarantor);
						}
					}
				}
				return returnedLoans.size();
			}
			logger.debug("### LoanIssuedWriter :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			return null;
		}
		return 0;
	}

	/**
	 * Synchronize cancelled loans.
	 *
	 * @return the integer
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#synchronizeCancelledLoans()
	 */
	@Override
	public Integer synchronizeCancelledLoans() {

		try {
			// find loan where status = 64 (Canceled) from ACM ABACUS
			List<LoanDTO> loansdDtos = transversClient.findCanceledLoan();
			// check if loansdDtos is not empty
			if (!ACMValidationUtils.isNullOrEmpty(loansdDtos)) {
				// get list of loan id extern
				List<Long> listIdLoanExtern = new ArrayList<>();
				loansdDtos.forEach(loanDTO -> listIdLoanExtern.add(loanDTO.getIdLoanExtern()));
				// init list of loan id extern by pagination
				List<Long> listByPagination = new ArrayList<>();
				int page = 0;
				int pageSize = 2000;
				List<LoanDTO> canceledLoans = new ArrayList<>();
				do {
					page++;
					// get of loan id extern by pagination
					listByPagination = getListByPage(listIdLoanExtern, page, pageSize);
					// cancel given loans in ACM
					canceledLoans.addAll(cancelLoans(listByPagination));
				}
				while (listByPagination != null);
				String subject = "Cacellend [ " + canceledLoans.size()
						+ " ] LOAN from ABACUS-DB successfully";
				String content = "Account Number of cancelled Loans";
				// send notification mail
				sendMail(canceledLoans, subject, content);
				// return number of cancelled loan
				return canceledLoans.size();
			}
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());

			return null;
		}

		return 0;
	}

	/**
	 * Gets the list by page.
	 * 
	 * @author idridi
	 * @param sourceList the source list
	 * @param page the page
	 * @param pageSize the page size
	 * @return the list by page
	 */
	public static List<Long> getListByPage(List<Long> sourceList, int page, int pageSize) {

		// check if page || pageSize are invalid
		if (pageSize <= 0 || page <= 0) {
			throw new IllegalArgumentException("invalid page size: " + pageSize);
		}
		// set fromIndex
		int fromIndex = (page - 1) * pageSize;
		// check if the list is empty or end of list
		if (sourceList == null || sourceList.size() <= fromIndex) {
			return null;
		}
		// return list by page and pageSize
		return sourceList.subList(fromIndex, Math.min(fromIndex + pageSize, sourceList.size()));
	}

	/**
	 * Send mail.
	 *
	 * @param loansDtos the loans dtos
	 * @param subject the subject
	 * @param content the content
	 */
	private void sendMail(List<LoanDTO> loansDtos, String subject, String content) {

		if (!ACMValidationUtils.isNullOrEmpty(loansDtos)) {
			try {
				String accountsNumbers = loansDtos.get(0).getAccountNumber();
				for (int i = 1; i < loansDtos.size(); i++) {
					accountsNumbers = accountsNumbers + " / " + loansDtos.get(i).getAccountNumber();
				}
				mailSenderClient.sendMail(
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL, defaultACMReceiverMail,
								subject, content + ": [ " + accountsNumbers + " ]."));
			}
			catch (FeignException e) {
				logger.error("Failed to send Mail");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
			logger.info("Sending Email Notification :: DONE");
		}
	}

	/**
	 * Synchronize loan by account number.
	 *
	 * @param accountNumber the account number
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#synchronizeLoanByAccountNumber(java.lang.String)
	 */
	@Override
	public LoanDTO synchronizeLoanByAccountNumber(String accountNumber)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		// reading loans from abacus
		Loan loan = getLoansFromAbacus(accountNumber);
		if (!ACMValidationUtils.isNullOrEmpty(loan.getIdLoanExtern())) {
			// save loans in acm and send mails and return number of synchronized loans
			return saveLoans(loan);
		}
		else {
			return null;
		}

	}

	/**
	 * Gets the loans from abacus.
	 *
	 * @param accountNumber the account number
	 * @return the loans from abacus
	 */
	private Loan getLoansFromAbacus(String accountNumber) {

		// new list of loans
		List<LoanDTO> loansAbacus = new ArrayList<>();
		try {
			// get loan from abacus by AccountNumber
			loansAbacus = transversClient.findLoanByAccountNumber(accountNumber);
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			return null;
		}
		Loan loan = new Loan();
		// if list of loans from abacus not empty
		if (!ACMValidationUtils.isNullOrEmpty(loansAbacus)) {

			loan = new Loan(
					loansAbacus.get(0).getPortfolioId() != null
							? loansAbacus.get(0).getPortfolioId()
							: 0,
					loansAbacus.get(0).getIdLoanExtern(), loansAbacus.get(0).getIdAccountExtern(),
					loansAbacus.get(0).getAccountNumber(), loansAbacus.get(0).getApplyDate(),
					loansAbacus.get(0).getProductCode(), loansAbacus.get(0).getProductDescription(),
					loansAbacus.get(0).getCustomerName(), loansAbacus.get(0).getPortfolioCode(),
					loansAbacus.get(0).getPortfolioDescription(),
					loansAbacus.get(0).getCurrencySymbol(),
					loansAbacus.get(0).getCurrencyDecimalPlaces(),
					loansAbacus.get(0).getProductId(), loansAbacus.get(0).getCustomerId(),
					loansAbacus.get(0).getApplyAmountTotal(), loansAbacus.get(0).getGracePeriod(),
					loansAbacus.get(0).getIndustryCode(),
					loansAbacus.get(0).getIndustryCodeDescription(),
					loansAbacus.get(0).getIssueDate(), loansAbacus.get(0).getCreationDate(),
					loansAbacus.get(0).getTermPeriodNum(), loansAbacus.get(0).getPaymentFreq(),
					loansAbacus.get(0).getIssueFeeAmount(), loansAbacus.get(0).getProductRate(),
					loansAbacus.get(0).getLoanReasonCode(),
					loansAbacus.get(0).getLoanReasonDescription(),
					loansAbacus.get(0).getInitialPaymentDate(),
					loansAbacus.get(0).getNormalPayment(), loansAbacus.get(0).getIgnoreOddDays(),
					loansAbacus.get(0).getPeriodsDeferred(),
					loansAbacus.get(0).getCalculateInitialPaymentDate(),
					loansAbacus.get(0).getTermPeriodID(), loansAbacus.get(0).getBranchID(),
					loansAbacus.get(0).getBranchName(), loansAbacus.get(0).getBranchDescription(),
					loansAbacus.get(0).getCustomerType(), loansAbacus.get(0).getCommunityCULoanID(),
					loansAbacus.get(0).getGuarantorSourceId(),
					loansAbacus.get(0).getSourceOfFundsID(),
					loansAbacus.get(0).getRefinanceReasonId(),
					loansAbacus.get(0).getDistrictCodeId(), loansAbacus.get(0).getIntPayPeriodNum(),
					loansAbacus.get(0).getLoanCalculationMode(), loansAbacus.get(0).getApr(),
					loansAbacus.get(0).getEffectiveIntRate());

		}
		return loan;
	}

	/**
	 * Calculate first repayment date.
	 *
	 * @param maxissueDate the maxissue date
	 * @param daterepaymentdate the daterepaymentdate
	 * @return the local date
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#calculateFirstRepaymentDate(java.lang.Integer,
	 * java.lang.Integer)
	 */
	@Override
	public LocalDate calculateFirstRepaymentDate(Integer maxissueDate, Integer daterepaymentdate) {

		int dayOfMonth = LocalDate.now().getDayOfMonth();

		if (dayOfMonth < maxissueDate) {

			return LocalDate.now().withDayOfMonth(daterepaymentdate);
		}
		else {

			return LocalDate.now().plusMonths(1).withDayOfMonth(daterepaymentdate);
		}
	}

	/**
	 * Gets the loans from abacus.
	 *
	 * @return the loans from abacus
	 */
	private List<Loan> getLoansFromAbacus() {

		// new list of loans
		List<LoanDTO> loansAbacus = new ArrayList<>();
		try {
			// loading the index where the last job has stopped
			AcmEnvironnementDTO environnementDTO = parametrageClient.find("LIMITE_ID_LOAN_EXTERNE");
			// loading list loan from ABACUS DB
			loansAbacus = transversClient.find(
					Long.valueOf(environnementDTO != null ? environnementDTO.getValue() : "0"));
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			return null;
		}
		List<Loan> loans = new ArrayList<>();
		// if list of loans from abacus not empty
		if (!ACMValidationUtils.isNullOrEmpty(loansAbacus)) {
			for (int i = 0; i < loansAbacus.size(); i++) {
				loans.add(new Loan(
						loansAbacus.get(i).getPortfolioId() != null
								? loansAbacus.get(i).getPortfolioId()
								: 0,
						loansAbacus.get(i).getIdLoanExtern(),
						loansAbacus.get(i).getIdAccountExtern(),
						loansAbacus.get(i).getAccountNumber(), loansAbacus.get(i).getApplyDate(),
						loansAbacus.get(i).getProductCode(),
						loansAbacus.get(i).getProductDescription(),
						loansAbacus.get(i).getCustomerName(), loansAbacus.get(i).getPortfolioCode(),
						loansAbacus.get(i).getPortfolioDescription(),
						loansAbacus.get(i).getCurrencySymbol(),
						loansAbacus.get(i).getCurrencyDecimalPlaces(),
						loansAbacus.get(i).getProductId(), loansAbacus.get(i).getCustomerId(),
						loansAbacus.get(i).getApplyAmountTotal(),
						loansAbacus.get(i).getGracePeriod(), loansAbacus.get(i).getIndustryCode(),
						loansAbacus.get(i).getIndustryCodeDescription(),
						loansAbacus.get(i).getIssueDate(), loansAbacus.get(i).getCreationDate(),
						loansAbacus.get(i).getTermPeriodNum(), loansAbacus.get(i).getPaymentFreq(),
						loansAbacus.get(i).getIssueFeeAmount(), loansAbacus.get(i).getProductRate(),
						loansAbacus.get(i).getLoanReasonCode(),
						loansAbacus.get(i).getLoanReasonDescription(),
						loansAbacus.get(i).getInitialPaymentDate(),
						loansAbacus.get(i).getNormalPayment(),
						loansAbacus.get(i).getIgnoreOddDays(),
						loansAbacus.get(i).getPeriodsDeferred(),
						loansAbacus.get(i).getCalculateInitialPaymentDate(),
						loansAbacus.get(i).getTermPeriodID(), loansAbacus.get(i).getBranchID(),
						loansAbacus.get(i).getBranchName(),
						loansAbacus.get(i).getBranchDescription(),
						loansAbacus.get(i).getCustomerType(),
						loansAbacus.get(i).getCommunityCULoanID(),
						loansAbacus.get(i).getGuarantorSourceId(),
						loansAbacus.get(i).getSourceOfFundsID(),
						loansAbacus.get(i).getRefinanceReasonId(),
						loansAbacus.get(i).getDistrictCodeId(),
						loansAbacus.get(i).getIntPayPeriodNum(),
						loansAbacus.get(i).getLoanCalculationMode(), loansAbacus.get(i).getApr(),
						loansAbacus.get(i).getEffectiveIntRate()));
			}
		}
		return loans;
	}

	/**
	 * Save loans.
	 *
	 * @param loan the loan
	 * @return the loan DTO
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */

	private LoanDTO saveLoans(Loan loan)
			throws CreditException, CalculateAgeException, ResourcesNotFoundException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		LoanDTO loanDTO = new LoanDTO();
		if (loan != null && loan.getIdLoanExtern() != null) {
			try {
				// insert imported loan into ACM DB
				loanDTO = save(new LoanDTO(
						loan.getPortfolioId() != null ? loan.getPortfolioId() : 0,
						loan.getIdLoanExtern(), loan.getIdAccountExtern(),
						loan.getAccountNumberExtern(), loan.getApplyDate(), loan.getProductCode(),
						loan.getProductDescription(), loan.getCustomerName(),
						loan.getPortfolioCode(), loan.getPortfolioDescription(),
						loan.getCurrencySymbol(), loan.getCurrencyDecimalPlaces(),
						loan.getProductId(), loan.getCustomerId(), loan.getApplyAmountTotal(),
						loan.getGracePeriod(), loan.getIndustryCode(),
						loan.getIndustryCodeDescription(), loan.getIssueDate(),
						loan.getCreationDate(), loan.getTermPeriodNum(), loan.getPaymentFreq(),
						loan.getIssueFeeAmount(), loan.getProductRate(), loan.getLoanReasonCode(),
						loan.getLoanReasonDescription(), loan.getInitialPaymentDate(),
						loan.getNormalPayment(), loan.getIgnoreOddDays(), loan.getPeriodsDeferred(),
						loan.getCalculateInitialPaymentDate(), loan.getTermPeriodID(),
						loan.getBranchID(), loan.getBranchName(), loan.getBranchDescription(),
						loan.getCustomerType(), loan.getCommunityCULoanID(),
						loan.getGuarantorSourceId(), loan.getSourceOfFundsID(),
						loan.getRefinanceReasonId(), loan.getDistrictCodeId(),
						loan.getIntPayPeriodNum(), loan.getLoanCalculationMode(), loan.getApr(),
						loan.getEffectiveIntRate()));
				// prepare to send mail notification if not null
				if (loanDTO != null) {
					logger.debug(
							"Loan with id_loan = [{}] / id_loan_Extern = [{}] was successfully added in ACM DB",
							loanDTO.getLoanId(), loanDTO.getIdLoanExtern());

				}
			}
			catch (FeignException e) {
				logger.error("Failed to save the loan in DB");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
		}

		// notification par mail
		sendMail(loanDTO);
		logger.debug("saving new loans in ACM-DB :: DONE");
		return loanDTO;

	}

	/**
	 * Send mail.
	 *
	 * @param loansDto the loans dto
	 */
	private void sendMail(LoanDTO loansDto) {

		if (!ACMValidationUtils.isNullOrEmpty(loansDto)) {
			try {

				mailSenderClient.sendMail(new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						defaultACMReceiverMail, "Processing LOAN from ABACUS-DB successfully",
						"Account Number : [ " + loansDto.getAccountNumber() + " ]."));
			}
			catch (FeignException e) {
				logger.error("Failed to send Mail");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
			logger.debug("Sending Email Notification :: DONE");
		}
	}

	/**
	 * Synchronize loans.
	 *
	 * @return the integer
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#synchronizeLoans()
	 */
	@Override

	public Integer synchronizeLoans()
			throws ResourcesNotFoundException, CreditException, CalculateAgeException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		// reading loans from abacus
		List<Loan> loans = getLoansFromAbacus();
		// save loans in acm and send mails and return number of synchronized loans
		return saveLoans(loans);
	}

	/**
	 * Save loans.
	 *
	 * @param loans the loans
	 * @return the integer
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */

	private Integer saveLoans(List<? extends Loan> loans)
			throws CreditException, CalculateAgeException, ResourcesNotFoundException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		List<LoanDTO> addedLoans = new ArrayList<>();
		for (Loan loan : loans) {
			if (loan != null && loan.getIdLoanExtern() != null) {
				try {
					// insert imported loan into ACM DB
					LoanDTO loanDTO = save(new LoanDTO(
							loan.getPortfolioId() != null ? loan.getPortfolioId() : 0,
							loan.getIdLoanExtern(), loan.getIdAccountExtern(),
							loan.getAccountNumberExtern(), loan.getApplyDate(),
							loan.getProductCode(), loan.getProductDescription(),
							loan.getCustomerName(), loan.getPortfolioCode(),
							loan.getPortfolioDescription(), loan.getCurrencySymbol(),
							loan.getCurrencyDecimalPlaces(), loan.getProductId(),
							loan.getCustomerId(), loan.getApplyAmountTotal(), loan.getGracePeriod(),
							loan.getIndustryCode(), loan.getIndustryCodeDescription(),
							loan.getIssueDate(), loan.getCreationDate(), loan.getTermPeriodNum(),
							loan.getPaymentFreq(), loan.getIssueFeeAmount(), loan.getProductRate(),
							loan.getLoanReasonCode(), loan.getLoanReasonDescription(),
							loan.getInitialPaymentDate(), loan.getNormalPayment(),
							loan.getIgnoreOddDays(), loan.getPeriodsDeferred(),
							loan.getCalculateInitialPaymentDate(), loan.getTermPeriodID(),
							loan.getBranchID(), loan.getBranchName(), loan.getBranchDescription(),
							loan.getCustomerType(), loan.getCommunityCULoanID(),
							loan.getGuarantorSourceId(), loan.getSourceOfFundsID(),
							loan.getRefinanceReasonId(), loan.getDistrictCodeId(),
							loan.getIntPayPeriodNum(), loan.getLoanCalculationMode(), loan.getApr(),
							loan.getEffectiveIntRate()));
					// prepare to send mail notification if not null
					if (loanDTO != null) {
						logger.debug(
								"Loan with id_loan = [{}] / id_loan_Extern = [{}] was successfully added in ACM DB",
								loanDTO.getLoanId(), loanDTO.getIdLoanExtern());
						addedLoans.add(loanDTO);
					}
				}
				catch (FeignException e) {
					logger.error("Failed to save the loan in DB");
					logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
							e.getMessage());
				}
			}
		}
		// notification par mail
		sendMail(addedLoans);
		logger.debug("saving new loans in ACM-DB :: DONE");
		return addedLoans.size();

	}

	/**
	 * Send mail.
	 *
	 * @param loansDtos the loans dtos
	 */
	private void sendMail(List<LoanDTO> loansDtos) {

		if (!ACMValidationUtils.isNullOrEmpty(loansDtos)) {
			try {
				String accountsNumbers = loansDtos.get(0).getAccountNumber();
				for (int i = 1; i < loansDtos.size(); i++) {
					accountsNumbers = accountsNumbers + " / " + loansDtos.get(i).getAccountNumber();
				}
				mailSenderClient.sendMail(
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL, defaultACMReceiverMail,
								"Processing [ " + loansDtos.size()
										+ " ] LOAN from ABACUS-DB successfully",
								"Account Number : [ " + accountsNumbers + " ]."));
			}
			catch (FeignException e) {
				logger.error("Failed to send Mail");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
			logger.debug("Sending Email Notification :: DONE");
		}
	}

	/**
	 * Find by customer.
	 *
	 * @param customerDTO the customer DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByIdCustomer(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> findByCustomer(CustomerDTO customerDTO) {

		List<CustomerDTO> customersDTo = customerService.find(customerDTO);
		if (customersDTo.size() > 0) {
			Customer customer = mapper.map(customersDTo.get(0), Customer.class);
			List<Loan> loans = loanRepository.findByCustomerAndEnabled(customer, Boolean.TRUE);
			// check if object is null
			if (ACMValidationUtils.isNullOrEmpty(loans)) {
				return new ArrayList<>();
			}
			List<LoanDTO> loanDTOs = new ArrayList<>();
			loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
			logger.info("METHOD : findByIdCustomer : {} : Loan was founded", loanDTOs.size());
			return loanDTOs;

		}
		return null;

	}

	/**
	 * Find by account.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByAccount(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public List<LoanDTO> findByAccount(LoanDTO loanDTO) {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QLoan
		QLoan qLoan = QLoan.loan;
		// build Predicate using given params
		BooleanBuilder predicate = buildQueryByAccount(loanDTO, qLoan);

		// find data
		Iterable<Loan> iterable = loanRepository.findAll(predicate);
		List<Loan> loans = new ArrayList<>();
		iterable.forEach(loans::add);

		// mapping && returning data
		List<LoanDTO> loanDTOs = new ArrayList<>();
		loans.forEach(loan -> loanDTOs.add(mapper.map(loan, LoanDTO.class)));
		logger.info("METHOD : find : {} : Loan was founded", loanDTOs.size());
		return loanDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param qLoan the q loan
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQueryByAccount(LoanDTO loanDTO, QLoan qLoan) {

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// FIND BY : STATUT TAB
		// find all user responsable && collaborator
		List<String> wheresOwners = new ArrayList<>();
		List<UserDTO> userDTOs = userClient.findUsers();
		userDTOs.forEach(user -> {
			if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
				wheresOwners.add(user.getLogin());
			}
		});
		// setting predicate to find by Id
		predicate.and(qLoan.owner.in(new ArrayList<>(new HashSet<>(wheresOwners))));

		// find loan by Access Branches for connected user
		if (userDTO.getCategory() != null
				&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
			int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(",")).stream()
					.map(String::trim).mapToInt(Integer::parseInt).toArray();
			List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
			for (int i : arrayBranchIds) {
				listBranchIds.add(Integer.valueOf(i));
			}
			// setting predicate to find by given branch Id
			BooleanBuilder subPredicate = new BooleanBuilder();
			subPredicate.and(qLoan.branchID.in(listBranchIds));
			// find by given status
			predicate.or(subPredicate);
			// if the connected user belong to(BRANCH_OPERATION/ CENTRAL_REVISION/
			// RISK_MANAGER
			// /COMPLIANCE_GROUP) the unassigned loans should not appears on dashboard
			GroupeDTO groupeDTO = userDTO.getGroupes().iterator().next();
			if (!ACMValidationUtils.isNullOrEmpty(groupeDTO)
					&& !ACMValidationUtils.isNullOrEmpty(groupeDTO.getCode())) {
				BooleanBuilder secondSubPredicate = buildGroupeSubPredicate(groupeDTO, qLoan);
				predicate.and(secondSubPredicate);
			}
		}
		else {
			// load loan by branch ID for SUPERVISOR
			boolean isManagerBranch = userDTOs.stream().anyMatch(
					user -> user.getTypeUser().equals(UserHierarchicalType.COLLABORATORS.name()));
			if (isManagerBranch) {
				// setting predicate to find by given branch Id
				BooleanBuilder subPredicate = new BooleanBuilder();
				subPredicate.and(qLoan.branchID.eq(userDTO.getBranchID()));
				// find by given status
				predicate.or(subPredicate);
			}
			// setting subPredicate to filter list participant by Id & status
			BooleanBuilder subPredicate = new BooleanBuilder();
			QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;
			List<LoanParticipantsDTO> loanParticipantsDTOs =
					loanParticipantsService.find(new LoanParticipantsDTO(null, userDTO.getLogin()));
			if (!ACMValidationUtils.isNullOrEmpty(loanParticipantsDTOs)
					&& loanParticipantsDTOs.size() <= 1000) {
				List<Long> wheresIds = new ArrayList<>();
				loanParticipantsDTOs.forEach(
						loanParticipantsDTO -> wheresIds.add(loanParticipantsDTO.getIdLoan()));
				subPredicate.and(qLoan.idLoan.in(new ArrayList<>(new HashSet<>(wheresIds))));
				predicate.or(subPredicate);
			}
			else if (!ACMValidationUtils.isNullOrEmpty(loanParticipantsDTOs)
					&& loanParticipantsDTOs.size() > 1000) {
				subPredicate.and(qLoan.idLoan.in(JPAExpressions.selectFrom(qLoanParticipants)
						.select(qLoanParticipants.idLoan)
						.where(qLoanParticipants.username.eq(userDTO.getLogin()))));
				predicate.or(subPredicate);
			}
		}
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey()));
		predicate.and(qLoan.statutWorkflow.ne(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey()));
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()));
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey()));
		predicate.and(qLoan.statutWorkflow
				.ne(CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.DECLINE).getKey()));

		// find loan by IdLoanExtern
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getIdLoanExtern())) {
			predicate.and(qLoan.idLoanExtern.eq(loanDTO.getIdLoanExtern()));
		}
		// find loan by customerType
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerType())) {
			predicate.and(qLoan.customerType.eq(loanDTO.getCustomerType()));
		}
		// find loan by parentId
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getParentId())) {
			predicate.and(qLoan.parentId.eq(loanDTO.getParentId()));
		}
		// find loan by customerIdExtern
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerId())) {
			predicate.and(qLoan.customerId.eq(loanDTO.getCustomerId()));
		}
		// find loan by customerId
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO())
				&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getId())) {
			predicate.and(qLoan.customer.eq(new Customer(loanDTO.getCustomerDTO().getId())));
		}
		// find loan by accountNumber
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getAccountNumber())) {
			predicate.and(qLoan.accountNumberExtern.like(loanDTO.getAccountNumber() + "%"));
		}

		// find only enabled data
		predicate.and(qLoan.enabled.eq(Boolean.TRUE));
		logger.info("*** Predicate = {}", predicate);
		return predicate;
	}

	/**
	 * Gets the closing balanceby id loan extern.
	 *
	 * @param idLoanExtern the id loan extern
	 * @return the closing balanceby id loan extern
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#getClosingBalancebyIdLoanExtern(java.lang.Long)
	 */
	@Override
	public Long getClosingBalancebyIdLoanExtern(Long idLoanExtern) throws ApiAbacusException {

		Preconditions.checkNotNull(idLoanExtern, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Long closingBalance = 0L;
		logger.info("Getting closing balance from abacus, calling transvers service");
		try {
			closingBalance = transversClient.getClosingBalancebyIdLoanExtern(idLoanExtern);
		}
		catch (Exception e) {
			logger.error("Error will calling transvers-service : {}", e.getMessage());
			throw new ApiAbacusException(
					new ExceptionResponseMessage(CommonErrorCode.API_ABACUS,
							CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE,
							new TechnicalException()),
					CommonExceptionsMessage.EXCEPTIONS_SERVER_UNAVAILABLE);
		}
		return closingBalance;
	}

	/**
	 * Refinance loan.
	 *
	 * @param loanDTO the loan DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#RefinanceLoan(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void refinanceLoan(LoanDTO loanDTO) throws IOException, ApiAbacusException {

		Preconditions.checkNotNull(loanDTO.getIdLoanExtern(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			transversClient.createRefinanceLoan(loanDTO);
		}
		catch (Exception e) {
			logger.error("Failed to refinance loan {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
		try {
			// create 'the new loan created in ABACUS' in ACM
			LoanDTO acmLoanDTO = createInAcm(loanDTO);
			if (!ACMValidationUtils.isNullOrEmpty(acmLoanDTO)) {
				// create guaranteer relationship with the guaranteer of the parent loan
				// get the guaranteer relationship of the parent loan
				CustomerLinksRelationshipDTO customerLinksRelationshipDTOParam =
						new CustomerLinksRelationshipDTO();
				customerLinksRelationshipDTOParam.setIdLoan(loanDTO.getLoanId());
				customerLinksRelationshipDTOParam
						.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
				List<CustomerLinksRelationshipDTO> customerLinksRelationshipDTOs =
						customerLinksRelationshipService.find(customerLinksRelationshipDTOParam);
				// save guaranteers of parent loan as guaranteer of refinanced loan
				if (!ACMValidationUtils.isNullOrEmpty(customerLinksRelationshipDTOs)) {
					for (CustomerLinksRelationshipDTO guarantor : customerLinksRelationshipDTOs) {
						customerLinksRelationshipService.save(
								new CustomerLinksRelationshipDTO(null, guarantor.getCustomerId(),
										guarantor.getMember(), guarantor.getLinkRelationshipType(),
										CommonConstants.RELATION_GUARANTOR, new Date(), null,
										acmLoanDTO.getLoanId(), guarantor.getAmountGuarantor()));
					}
				}
			}
		}
		catch (Exception e) {
			logger.error(" ################# Can not save Loan in ACM ################# ");
			AcmDamagedDataDTO acmDamagedCustomerDTO = parametrageClient
					.saveAcmDamagedData(new AcmDamagedDataDTO("GET LOAN AFTER REFINANCE",
							e.getMessage(), loanDTO.getIdAccountExtern()));
			logger.debug(
					"############# Loan's CUAccountID saved in ACM_DAMAGED_DATA{} #####################",
					acmDamagedCustomerDTO);
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, "Failed to save in ACM");
		}

	}

	/**
	 * Creates the in acm.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */

	private LoanDTO createInAcm(LoanDTO loanDTO)
			throws CreditException, CalculateAgeException, ResourcesNotFoundException,
			ConditionalApproveException, ApiAbacusException, DisbursementException,
			CheckApprovelLevelException, EnableCriticalDataException, WorkFlowSettingException,
			IOException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		// init loanDTO result
		LoanDTO loanDTOResult = null;
		// check if accountNUmber is null
		Preconditions.checkNotNull(loanDTO.getAccountNumber(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// find loan from Abacus by accountNumber
		List<LoanDTO> loanDTOs = transversClient.findLoanByAccountNumber(loanDTO.getAccountNumber())
				.stream().filter(l -> l.getStatut() == 1).collect(Collectors.toList());
		// check if loanDTO. loanExternID is not null
		if (!ACMValidationUtils.isNullOrEmpty(loanDTOs)
				&& loanDTOs.get(0).getIdLoanExtern() != null) {
			// create Loan in ACM

			loanDTOResult = save(new LoanDTO(
					loanDTOs.get(0).getPortfolioId() != null ? loanDTOs.get(0).getPortfolioId() : 0,
					loanDTOs.get(0).getIdLoanExtern(), loanDTOs.get(0).getIdAccountExtern(),
					loanDTOs.get(0).getAccountNumber(), loanDTOs.get(0).getApplyDate(),
					loanDTOs.get(0).getProductCode(), loanDTOs.get(0).getProductDescription(),
					loanDTOs.get(0).getCustomerName(), loanDTOs.get(0).getPortfolioCode(),
					loanDTOs.get(0).getPortfolioDescription(), loanDTOs.get(0).getCurrencySymbol(),
					loanDTOs.get(0).getCurrencyDecimalPlaces(), loanDTOs.get(0).getProductId(),
					loanDTOs.get(0).getCustomerId(), loanDTOs.get(0).getApplyAmountTotal(),
					loanDTOs.get(0).getGracePeriod(), loanDTOs.get(0).getIndustryCode(),
					loanDTOs.get(0).getIndustryCodeDescription(), loanDTOs.get(0).getIssueDate(),
					loanDTOs.get(0).getCreationDate(), loanDTO.getTermPeriodNum(),
					loanDTO.getInterestFreq(), loanDTO.getPaymentFreq(),
					loanDTOs.get(0).getIssueFeeAmount(), loanDTOs.get(0).getProductRate(),
					loanDTOs.get(0).getLoanReasonCode(), loanDTOs.get(0).getLoanReasonDescription(),
					loanDTOs.get(0).getInitialPaymentDate(), loanDTOs.get(0).getNormalPayment(),
					loanDTOs.get(0).getIgnoreOddDays(), loanDTOs.get(0).getPeriodsDeferred(),
					loanDTOs.get(0).getCalculateInitialPaymentDate(),
					loanDTOs.get(0).getTermPeriodID(), loanDTOs.get(0).getBranchID(),
					loanDTOs.get(0).getBranchName(), loanDTOs.get(0).getBranchDescription(),
					loanDTOs.get(0).getCustomerType(), loanDTOs.get(0).getCommunityCULoanID(),
					loanDTOs.get(0).getGuarantorSourceId(), loanDTOs.get(0).getSourceOfFundsID(),
					loanDTOs.get(0).getRefinanceReasonId(), loanDTOs.get(0).getDistrictCodeId(),
					loanDTOs.get(0).getIntPayPeriodNum(), loanDTOs.get(0).getLoanCalculationMode(),
					loanDTOs.get(0).getApr(), loanDTOs.get(0).getEffectiveIntRate(),
					loanDTO.getLoanApplicationStatus(), loanDTO.getOpeningBalance(),
					loanDTO.getPeriodsDeferredType(), loanDTOs.get(0).getTermPeriodNum(),
					loanDTO.getUserDefinedFieldsLinksDTOs()));
		}
		return loanDTOResult;
	}

	/**
	 * Update all loan.
	 *
	 * @param loanDTOs the loan DT os
	 * @param action the action
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateAllLoan(java.util.List)
	 */
	@Override
	public void updateAllLoan(List<LoanDTO> loanDTOs, String action) {

		logger.info("START UPDATE FOR ALL GIVEN Loan");
		// init loans list
		List<Loan> loans = new ArrayList<>();
		Loan loan = new Loan();
		if (!ACMValidationUtils.isNullOrEmpty(loanDTOs)) {
			// loop on loanDTOs list
			for (LoanDTO loanDTO : loanDTOs) {
				// find loan from repository

				// Loan loan = loanRepository
				// .findByIdAccountExternAndEnabled(loanDTO.getIdAccountExtern(),
				// Boolean.TRUE).get(0);
				List<Loan> loanlist = loanRepository.findByIdAccountExternAndEnabled(
						loanDTO.getIdAccountExtern(), Boolean.TRUE);
				if (!ACMValidationUtils.isNullOrEmpty(loanlist)) {
					loan = loanlist.get(0);

					// check if loan exist in ACM database
					if (!ACMValidationUtils.isNullOrEmpty(loan)) {
						if (action.equals(CommonConstants.SYNCHRONIZE_PORTFOLIOS)) {

							loan.setPortfolioId(loanDTO.getPortfolioId());
							loan.setPortfolioCode(loanDTO.getPortfolioCode());
							loan.setPortfolioDescription(loanDTO.getPortfolioDescription());

						}
						else if (action.equals(CommonConstants.SYNCHRONIZE_BRANCHES)) {
							loan.setBranchID(loanDTO.getBranchID());
							loan.setBranchDescription(loanDTO.getBranchDescription());
							loan.setBranchName(loanDTO.getBranchName());
						}
						// add loan to loan list to update
						CommonFunctions.mapperToUpdate(loan, userClient, logger);
						loans.add(loan);
					}
				}
			}
			// update loan list
			loanRepository.saveAll(loans);
			logger.info("DONE UPDATE ALL Loan");
		}
	}

	/**
	 * Update loan branches.
	 *
	 * @param customerDTOs the customer DT os
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateLoanBranches(java.util.List)
	 */
	@Override
	public void updateLoanBranches(List<CustomerDTO> customerDTOs) {

		logger.info("START UPDATE BRANCHES FOR ALL GIVEN CUSTOMERS AND THEIR LOANS");
		if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
			// loop on customerDTOs list
			customerDTOs.forEach(customerDTO -> {
				// get loans of the customer
				List<LoanDTO> loanDTOs = findByCustomer(customerDTO);
				if (!ACMValidationUtils.isNullOrEmpty(loanDTOs)) {
					// update branch information for each loanDTO
					loanDTOs.forEach(loanDTO -> {
						loanDTO.setBranchID(customerDTO.getBranchId());
						loanDTO.setBranchDescription(customerDTO.getBranchesDescription());
						loanDTO.setBranchName(customerDTO.getBranchesName());
					});
					// update loans
					updateAllLoan(loanDTOs, CommonConstants.SYNCHRONIZE_BRANCHES);
				}
			});
		}
		logger.info("DONE UPDATE BRANCHES FOR ALL GIVEN CUSTOMERS AND THEIR LOANS");
	}

	/**
	 * Automatic step loan.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CustomerMaxActiveAccountException the customer max active account exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws DisbursementException the disbursement exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 */
	@Override
	public LoanDTO automaticStepLoan(LoanDTO loanDTO) throws ResourcesNotFoundException,
			CustomerMaxActiveAccountException, ApiAbacusException, IOException,
			CalculateAgeException, CreditException, ConditionalApproveException,
			DisbursementException, CheckApprovelLevelException, EnableCriticalDataException,
			WorkFlowSettingException, InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			UploadSignedDocNotFoundExepction, UploadDocumentNotFoundException {

		// Get workflow step's setting of the completed step
		WorkFlowStepDTO actualWorkFlowStepDTOParam = new WorkFlowStepDTO();
		actualWorkFlowStepDTOParam.setIdWorkFlowStep(loanDTO.getEtapeWorkflow().longValue());
		List<WorkFlowStepDTO> actualWorkFlowStepDTOs =
				parametrageClient.findWorkFlowSteps(actualWorkFlowStepDTOParam);

		if (!ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTOs)) {
			WorkFlowStepDTO actualWorkFlowStepDTO = actualWorkFlowStepDTOs.get(0);

			// Check if the step is automatic
			if (Boolean.TRUE.equals(actualWorkFlowStepDTO.getAutomaticStep())) {
				boolean reject = false;
				boolean accept = false;

				if (!ACMValidationUtils
						.isNullOrEmpty(actualWorkFlowStepDTO.getAcceptationCondition())
						|| !ACMValidationUtils
								.isNullOrEmpty(actualWorkFlowStepDTO.getRejectionCondition())) {

					if (!ACMValidationUtils
							.isNullOrEmpty(actualWorkFlowStepDTO.getAcceptationCondition())
							&& !ACMValidationUtils
									.isNullOrEmpty(actualWorkFlowStepDTO.getRejectionCondition())
							&& actualWorkFlowStepDTO.getRejectionCondition()
									.equals(actualWorkFlowStepDTO.getAcceptationCondition())) {
						accept = Boolean.TRUE
								.equals(callThirdPartyAPI(actualWorkFlowStepDTO, loanDTO, true));
						reject = !accept;
					}
					else {
						if (!ACMValidationUtils
								.isNullOrEmpty(actualWorkFlowStepDTO.getAcceptationCondition())) {
							accept = Boolean.TRUE.equals(
									callThirdPartyAPI(actualWorkFlowStepDTO, loanDTO, true));
						}
						if (!ACMValidationUtils
								.isNullOrEmpty(actualWorkFlowStepDTO.getRejectionCondition())) {
							reject = Boolean.FALSE.equals(
									callThirdPartyAPI(actualWorkFlowStepDTO, loanDTO, false));
						}

					}

				}
				loanDTO = rejectionAndAcceptationConditions(actualWorkFlowStepDTO, loanDTO, reject,
						accept);
			}
		}
		return loanDTO;
	}

	/**
	 * Rejection and acceptation conditions.
	 *
	 * @param workFlowStepDTO the work flow step DTO
	 * @param loanDTO the loan DTO
	 * @param reject the reject
	 * @param accept the accept
	 * @return the loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private LoanDTO rejectionAndAcceptationConditions(WorkFlowStepDTO workFlowStepDTO,
			LoanDTO loanDTO, boolean reject, boolean accept)
			throws ApiAbacusException, ResourcesNotFoundException, IOException {

		if (reject) {
			// call method reject loan
			loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
			loanDTO.setParentId(0L);
			loanDTO.setCodeExternMotifRejet(1);
			loanDTO.setNote("Automatically Rejected Due To Insufficient Score");
			return rejected(loanDTO);
		}

		if (accept) {
			// call validate
			try {
				loanDTO.setWorkFlowAction(
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE);
				loanDTO.setNote("Automatically Approved");
				return stepWorkFlow(loanDTO);
			}
			catch (Exception e) {
				logger.error("Problem on stepWorkFlow called by automatic process : {}", e);
			}

		}

		// redirect to the Dashboard
		loanDTO.setIhmRoot("");
		return loanDTO;

	}

	/**
	 * Call third party API.
	 *
	 * @param workFlowStepDTO the work flow step DTO
	 * @param loanDTO the loan DTO
	 * @param checkAccept the check accept
	 * @return the boolean
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private Boolean callThirdPartyAPI(WorkFlowStepDTO workFlowStepDTO, LoanDTO loanDTO,
			boolean checkAccept) throws IOException, ResourcesNotFoundException {

		String condition = checkAccept ? workFlowStepDTO.getAcceptationCondition()
				: workFlowStepDTO.getRejectionCondition();
		if (!ACMValidationUtils.isNullOrEmpty(condition)) {
			switch (condition) {

				case CommonConstants.YAKEEN_API:
					return true;
				// return checkAPIYakeen(loanDTO, workFlowStepDTO);

				case CommonConstants.DAKHLI_API:

					return checkDakhliApi(loanDTO);

				case CommonConstants.MOFEED_API:

					return checkMofeedApi(loanDTO);
				// TO DO
				// break;
				// to implement in IB
				case CommonConstants.ABSHER:
					return true;
				// TO DO
				// break;
				case CommonConstants.SCORE_RIES_API:
					return checkAPIScoreRies(loanDTO, workFlowStepDTO);

				case CommonConstants.MOURABHA_BUY_API:

					return checkAPIMurabhaPurchase(loanDTO);

				case CommonConstants.MOURABHA_SELL_API:

					return checkAPIMurabhaSell(loanDTO);

				case CommonConstants.MOURABHA_TRANSFER_NOTICE_API:

					return checkAPITransferNotice(loanDTO);

				case CommonConstants.E_SIGNATURE_SANAD_EL_AMER_API:
					return loadDataIbService.createSingleSanadElAmer(loanDTO.getIdIbLoan());

				case CommonConstants.E_SIGNATURE_AGREEMENT_API:
					return true;

				// TO DO
				// break;
				case CommonConstants.SIMAH_API:
					return checkEnquiryNewCustomer(workFlowStepDTO, loanDTO);
				// TO DO
				// break;
				case CommonConstants.AML_API:
					return checkAPIAMLRies(loanDTO);
			}
		}
		return false;
	}

	/**
	 * Check APIAML ries.
	 *
	 * @author kouali
	 * @param loanDTO the loan DTO
	 * @return the search person customer response
	 */
	private Boolean checkAPIAMLRies(LoanDTO loanDTO) {

		CustomerVneuron customerVneuron = new CustomerVneuron();
		customerVneuron.setNid(loanDTO.getCustomerDTO().getIdentity());
		customerVneuron.setFirstName(loanDTO.getCustomerDTO().getFirstName());
		customerVneuron.setLastName(loanDTO.getCustomerDTO().getLastName());
		String formatPattern = "yyyy-MM-dd";
		// Create a SimpleDateFormat object with the desired format
		SimpleDateFormat dateFormat = new SimpleDateFormat(formatPattern);
		// Format the date using the SimpleDateFormat
		String formattedDate = dateFormat.format(loanDTO.getCustomerDTO().getDateOfBirth());
		customerVneuron.setBirthDate(formattedDate);
		// get nationality Value from UDF
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		userDefinedFieldsLinksDTO.setCustomerId(loanDTO.getCustomerDTO().getId());

		// List<UDFLinksGroupeFieldsDTO> linksGroupeFieldsDTOs =
		// userDefinedFieldsLinkService.findUDFGroupBy(userDefinedFieldsLinkService
		// .find(userDefinedFieldsLinksDTO).stream().filter(item -> item
		// .getUserDefinedFieldsDTO().getName().equals("Nationality"))
		// .collect(Collectors.toList()).get(0));

		// List<UDFLinksGroupeFieldsModelDTO> udfLinksGroupeFieldsModelDTOs =
		// linksGroupeFieldsDTOs.get(0).getUdfGroupeFieldsModels().stream()
		// .filter(udf -> udf.getFieldName().equals("Nationality"))
		// .collect(Collectors.toList());
		// customerVneuron
		// .setNationality(udfLinksGroupeFieldsModelDTOs.get(0).getValue().toUpperCase());
		customerVneuron.setNationality("113");

		SearchPersonCustomerResponse searchPersonCustomerResponse = transversClient
				.postAML(customerVneuron, loanDTO.getLoanId(), loanDTO.getCustomerId());

		try {
			save(loanDTO.getLoanId(), loanDTO);
		}
		catch (ResourcesNotFoundException e) {
			logger.info("error checkAPIAMLRies {}", e.getMessage());

		}
		// check if searchResultItems is empty then ok (contunious process)
		if (ACMValidationUtils.isNullOrEmpty(searchPersonCustomerResponse.getSearchResultItems())) {
			return true;
		}

		return null;

	}

	/**
	 * Check API yakeen.
	 *
	 * @param loanDTO the loan DTO
	 * @param workFlowStepDTO the work flow step DTO
	 * @return the boolean
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private Boolean checkAPIYakeen(LoanDTO loanDTO, WorkFlowStepDTO workFlowStepDTO)
			throws IOException {

		String dateOfBirth =
				DateUtil.formatDate(loanDTO.getCustomerDTO().getDateOfBirth(), "yyyy-MM-dd");
		ResponseEntity<String> responseYakken = transversClient.saudiByPassportOrNin(
				loanDTO.getCustomerDTO().getIdentity(), dateOfBirth, null, null);

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMinScoreRejected())
				&& !ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMaxScoreRejected())
				&& responseYakken.getStatusCodeValue() != 200) {

			return false;
		}
		return true;
	}

	/**
	 * Send otp mobishastra.
	 *
	 * @param loanDTO the loan DTO
	 * @param workFlowStepDTO the work flow step DTO
	 * @return the boolean
	 */
	private Boolean sendOtpMobishastra(LoanDTO loanDTO, WorkFlowStepDTO workFlowStepDTO) {

		// when we know what we should send from Consumer mobile we must change the
		// second param MSG
		ResponseEntity<String> responseOtp =
				transversClient.sendSmsOtp(loanDTO.getCustomerDTO().getTelephone(), "MSG");

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMinScoreRejected())
				&& !ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMaxScoreRejected())) {
			try {
				String responseJson = responseOtp.getBody();
				ObjectMapper objectMapper = new ObjectMapper();
				JsonNode jsonNode = objectMapper.readTree(responseJson);

				if (jsonNode.isArray()) {
					for (JsonNode node : jsonNode) {
						String responseValue = node.get("response").asText();
						if (!responseValue.equals("send success")) {
							return false;
						}
					}
				}
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
		return true;
	}

	/**
	 * Check dakhli api.
	 *
	 * @param loanDTO the loan DTO
	 * @return the boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private Boolean checkDakhliApi(LoanDTO loanDTO) throws ResourcesNotFoundException {

		ResponseEntity<ResponseIncomeDakhliApiDTO> responseDakhliApi = null;
		try {

			responseDakhliApi = transversClient.getEmploymentStatus(
					loanDTO.getCustomerDTO().getIdentity(), loanDTO.getLoanId());

			// if status 200 : we use the api dakhli on uat
			if (!ACMValidationUtils.isNullOrEmpty(responseDakhliApi.getBody())

					&& !ACMValidationUtils
							.isNullOrEmpty(responseDakhliApi.getBody().getEmploymentStatusInfo())) {

				List<RespInfoDakhliApiDTO> listInfoDakhli =
						responseDakhliApi.getBody().getEmploymentStatusInfo();

				BigDecimal salaryFromDakhli = new BigDecimal(listInfoDakhli.get(0).getBasicWage());

				return calculSalaryAndUpdateAcmLoanInIB(loanDTO, salaryFromDakhli);

			}
		}
		catch (Exception e) {

			// if status not ok we continue using this code for the test in our local
			// environement

			if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getOtherInformations())) {
				int salaryMockDakhli = 0;
				String otherInfos = loanDTO.getOtherInformations();
				String[] parts = otherInfos.split(",");
				for (String part : parts) {
					if (part.contains("basicSalary")) {
						String[] keyValueMap = part.split(":");
						String tempVar = keyValueMap[2];
						salaryMockDakhli = Integer.parseInt(tempVar);
					}
				}
				BigDecimal salaryFromDakhli = new BigDecimal(salaryMockDakhli);
				return calculSalaryAndUpdateAcmLoanInIB(loanDTO, salaryFromDakhli);

			}

		}
		return false;

	}

	/**
	 * Check API score ries.
	 *
	 * @param loanDTO the loan DTO
	 * @param workFlowStepDTO the work flow step DTO
	 * @return the boolean
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private Boolean checkAPIScoreRies(LoanDTO loanDTO, WorkFlowStepDTO workFlowStepDTO)
			throws IOException {

		CustomerVneuron customerVneuron = new CustomerVneuron();
		customerVneuron.setNid(loanDTO.getCustomerDTO().getIdentity());
		customerVneuron.setFirstName(loanDTO.getCustomerDTO().getFirstName());
		customerVneuron.setLastName(loanDTO.getCustomerDTO().getLastName());
		String formatPattern = "yyyy-MM-dd";
		// Create a SimpleDateFormat object with the desired format
		SimpleDateFormat dateFormat = new SimpleDateFormat(formatPattern);
		// Format the date using the SimpleDateFormat
		String formattedDate = dateFormat.format(loanDTO.getCustomerDTO().getDateOfBirth());
		customerVneuron.setBirthDate(formattedDate);
		// get nationality Value from UDF
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		userDefinedFieldsLinksDTO.setCustomerId(loanDTO.getCustomerDTO().getId());

		// List<UDFLinksGroupeFieldsDTO> linksGroupeFieldsDTOs =
		// userDefinedFieldsLinkService.findUDFGroupBy(userDefinedFieldsLinkService
		// .find(userDefinedFieldsLinksDTO).stream().filter(item -> item
		// .getUserDefinedFieldsDTO().getName().equals("Nationality"))
		// .collect(Collectors.toList()).get(0));

		// userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getName()

		// List<UDFLinksGroupeFieldsModelDTO> udfLinksGroupeFieldsModelDTOs =
		// linksGroupeFieldsDTOs.get(0).getUdfGroupeFieldsModels().stream()
		// .filter(udf -> udf.getFieldName().equals("Nationality"))
		// .collect(Collectors.toList());
		// customerVneuron
		// .setNationality(udfLinksGroupeFieldsModelDTOs.get(0).getValue().toUpperCase());
		customerVneuron.setNationality("113");
		transversClient.getScore(customerVneuron, loanDTO.getLoanId(), loanDTO.getCustomerId());
		// if Min and Max Rejection conditions are not empty so we must take in
		// consideration the resulat of api
		// a verifier

		String custumerStatus =
				transversClient.getCustomerStatus(loanDTO.getCustomerId(), loanDTO.getLoanId());
		if (custumerStatus.equals(CommonConstants.CUSTOMER_CATEGORY)) {
			return true;
		}
		else if (custumerStatus.equals(CommonConstants.CUSTOMER_STATUS_REIS)) {
			loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
			loanDTO.setParentId(0L);
			loanDTO.setCodeExternMotifRejet(1);
			loanDTO.setNote("Automatically Rejected Due To Unauthorized Customer (REIS)");
			try {
				rejected(loanDTO);
			}
			catch (ApiAbacusException | ResourcesNotFoundException e) {
				logger.info("error rejected loan  {}", e.getMessage());
			}

		}

		return false;
	}

	/**
	 * stepWorkFlow.
	 *
	 * @author Yesser Somai
	 * @param loanDTO LoanDTO
	 * @return LoanDTO
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws DisbursementException the disbursement exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	private LoanDTO stepWorkFlow(LoanDTO loanDTO)
			throws ConditionalApproveException, ApiAbacusException, DisbursementException,
			IOException, ResourcesNotFoundException, CheckApprovelLevelException,
			InformCustomerNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			EnableCriticalDataException, CreditException, UploadSignedDocNotFoundExepction,
			UploadDocumentNotFoundException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		LoanInstanceDTO parmLoanInstanceDTO = new LoanInstanceDTO();
		parmLoanInstanceDTO.setIdLoan(loanDTO.getLoanId());
		// find loanInstances of loanDTO ordered by OrderEtapeProcess
		List<LoanInstanceDTO> loanInstanceDTOs = loanInstanceService.find(parmLoanInstanceDTO);
		// get actual loan instance step
		final LoanDTO loanDTOFinal = loanDTO;
		LoanInstanceDTO currentLoanInstanceStep = loanInstanceDTOs.stream()
				.filter(instance -> instance.getCode().equals(loanDTOFinal.getEtapeWorkflow()))
				.findFirst().orElse(null);
		Boolean moveToNextStep = Boolean.TRUE;
		LoanDTO newLoanDTO = new LoanDTO();
		newLoanDTO.setIhmRoot("");
		loanDTO.setIhmRoot("");

		if (!ACMValidationUtils.isNullOrEmpty(currentLoanInstanceStep)) {
			// get workflow step's setting of the completed step
			WorkFlowStepDTO actualWorkFlowStepDTOParam = new WorkFlowStepDTO();
			actualWorkFlowStepDTOParam
					.setIdWorkFlowStep(currentLoanInstanceStep.getCode().longValue());
			List<WorkFlowStepDTO> actualWorkFlowStepDTOs =
					parametrageClient.findWorkFlowSteps(actualWorkFlowStepDTOParam);
			WorkFlowStepDTO actualWorkFlowStepDTO = new WorkFlowStepDTO();
			if (ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTOs)) {
				throw new WorkFlowSettingException(
						new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
								CommonExceptionsMessage.WORKFLOW_SETTING_NOT_FOUND,
								new TechnicalException()),
						CommonExceptionsMessage.WORKFLOW_SETTING_NOT_FOUND);
			}
			else {
				actualWorkFlowStepDTO = actualWorkFlowStepDTOs.get(0);
			}
			if (!ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTO)
					&& Boolean.TRUE.equals(actualWorkFlowStepDTO.getApprovalConditions())
					&& conditionalApproveService
							.countByIdLoanAndConditionnalValidation(loanDTO.getLoanId()) > 0) {

				throw new ConditionalApproveException(CommonErrorCode.APPROVAL_EXCEPTION,
						"Please check the approval conditions");
			}
			// Charge Fee
			if (actualWorkFlowStepDTO.getListChargeFees().size() > 0) {
				ChargeFeesDTO chargeFeeDTOParam = new ChargeFeesDTO();
				chargeFeeDTOParam.setIdLoanInstance(currentLoanInstanceStep.getId());
				chargeFeeDTOParam.setCharged(Boolean.FALSE);
				List<ChargeFeesDTO> feesToCharge = chargeFeesService.find(chargeFeeDTOParam);
				feesToCharge = addAutomaticFees(loanDTO, actualWorkFlowStepDTO,
						currentLoanInstanceStep, feesToCharge);

				if (!ACMValidationUtils.isNullOrEmpty(feesToCharge)) {
					try {
						ResponseChargeFeeDTO responseChargeFeeDTO = new ResponseChargeFeeDTO();
						responseChargeFeeDTO =
								transversClient.initializeChargeFee(loanDTO.getIdAccountExtern());
						responseChargeFeeDTO = setChargeFee(feesToCharge, responseChargeFeeDTO);
						logger.info("FEES CHARGED");
						transversClient.postChargeFees(responseChargeFeeDTO);
						chargeFeesService.saveAll(feesToCharge);

					}
					catch (Exception ex) {
						throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
								"charge fee exception : " + ex.getMessage());
					}
					// add new note
					addLoanNote(loanDTO, feesToCharge);
				}
			}
			if ((!ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTO.getCheckMezaCard())
					&& actualWorkFlowStepDTO.getCheckMezaCard().equals(Boolean.TRUE))
					&& (loanDTO.getCustomerDTO().getMezaCardStatus().equals("NEW")
							|| loanDTO.getCustomerDTO().getMezaCardStatus().equals("SENT"))) {

				throw new CheckMezaCardException(CommonErrorCode.CHECK_MEZA_CARD_EXCEPTION,
						"Please check meza card");
			}

			if ((!ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTO.getCheckMezaCard())
					&& actualWorkFlowStepDTO.getCheckMezaCard().equals(Boolean.TRUE))
					&& loanDTO.getCustomerDTO().getMezaCardStatus().equals("UNTRUSTED")) {
				throw new CheckMezaCardUntrustException(
						CommonErrorCode.CHECK_MEZA_CARD_EXCEPTION_UNTRUST,
						"Please reject the loan");
			}

			// controle fees
			if (!ACMValidationUtils.isNullOrEmpty(loanDTOFinal.getProductDTO())) {
				if ((!ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTO.getCheckFees())
						&& actualWorkFlowStepDTO.getCheckFees().equals(Boolean.TRUE))) {

					List<Long> lst = new ArrayList<Long>();
					actualWorkFlowStepDTO.getLstFees().forEach(item -> lst.add(item.getCufeeID()));

					List<Long> lstUniq = lst.stream().distinct().collect(Collectors.toList());
					if (!ACMValidationUtils.isNullOrEmpty(lstUniq)) {
						Long checkFee =
								transversClient.checkFee(loanDTO.getIdAccountExtern(), lstUniq);

						if (!ACMValidationUtils.isNullOrEmpty(checkFee) && checkFee > 0) {
							throw new CheckFeesException(CommonErrorCode.CHECK_FEES_EXCEPTION,
									"Please check the fees");

						}
					}
				}
				//
				// applicationFeeDTO = actualWorkFlowStepDTO.getLstFees().stream()
				// .filter(p -> p.getCode().contains("Application")).findFirst().orElse(null);
				//
				// if (!ACMValidationUtils.isNullOrEmpty(applicationFeeDTO)
				// && transversClient.findApplicationFee(loanDTO.getIdAccountExtern()) == 0) {
				// throw new CheckFeesException(CommonErrorCode.CHECK_FEES_EXCEPTION,
				// "Please check the fees");
				//
				// }

			}
			// int isStepTreat = loanDTO.getLoanInstancesDtos().stream()
			// .filter(item -> (!ACMValidationUtils.isNullOrEmpty(item.getActionUser()) &&
			// item
			// .getIbIhmRoot().equals(CommonConstants.IB_IHM_ROOT_SIGN_CONTRACT)))
			// .collect(Collectors.toList()).size();

			if (!ACMValidationUtils.isNullOrEmpty(actualWorkFlowStepDTO.getIbScreen())) {
				if (actualWorkFlowStepDTO.getIbScreen()
						.equals(CommonConstants.IB_IHM_ROOT_SIGN_CONTRACT)
						&& ACMValidationUtils.isNullOrEmpty(loanDTO.getSignContarctValidation())
						&& Boolean.TRUE.equals(actualWorkFlowStepDTO.getActiveTimerLoan())) {
					loanDTO.setSignContarctValidation(LocalTime.now().toString());
				}
			}

			// backLoanToFistStepInSignContract();

			if (actualWorkFlowStepDTO.getJournalEntryTypes().size() > 0) {
				// send JE to abacus and save it in Acm
				String resCreateJentry = CreateJournalEntryInAbacus(loanDTO, actualWorkFlowStepDTO,
						currentLoanInstanceStep);
				currentLoanInstanceStep.setJournalEntry(resCreateJentry);
				loanInstanceService.save(currentLoanInstanceStep.getId(), currentLoanInstanceStep);

			}
			// if actual setting step has 'ready for disbursement' enabled
			if (Boolean.TRUE.equals(actualWorkFlowStepDTO.getReadyForDisb())) {
				validateReadyForDisbursement(loanDTO);

				if (Boolean.TRUE.equals(loanDTO.getProductDTO().getDisburse())) {

					// approve level 2 of cuLoanProcess in Abacus
					DisburseResponse disburseResponse = disburseLoan(loanDTO);
					// Check if disburse is executed else an error has been occured
					if (disburseResponse.getReceiptPrintParameters().getReceiptNo() != 0) {
						// Disburse OK change statut to Issued
						loanDTO.setStatutWorkflow(22);
						loanDTO.setStatut(8);
						loanDTO.setStatutLibelle("Issued");
					}
					else {
						throw new CheckFeesException(CommonErrorCode.CHECK_DISBURSE_EXCEPTION,
								"Loan Not Disbursed");
					}

				}
			}

			// execute the workflowAction (if exist) of the step
			if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkFlowAction())) {
				loanDTO.setWorkFlowAction(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
			}
			switch (loanDTO.getWorkFlowAction()) {
				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_RECOMMEND_AUDIT:
				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_RECOMMEND_RISK:
					newLoanDTO = loanWorkflowUserActionService.actionDynamicAuditRisk(loanDTO);
					break;
				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE:
					UserDefinedFieldsLinksDTO udfLink = new UserDefinedFieldsLinksDTO();
					udfLink.setElementId(loanDTO.getLoanId());
					udfLink.setCategory(CommonConstants.LOAN_CATEGORY);
					loanDTO.setUserDefinedFieldsLinksDTOs(
							userDefinedFieldsLinksService.find(new UserDefinedFieldsLinksDTO()));
					moveToNextStep = actionDynamicLoanApprove(loanDTO, currentLoanInstanceStep,
							actualWorkFlowStepDTO);
					newLoanDTO = loanWorkflowUserActionService.actionDynamicWorkflow(loanDTO);

					break;
				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_CUSTOMER_DECISION:
					if (loanDTO.getWorkflowNextAction()
							.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_DECLINED)) {
						moveToNextStep = Boolean.FALSE;
					}
					loanWorkflowUserActionService.actionInformCustomer(loanDTO);
					break;

				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_AGREED_UPLOAD_SIGNED_AGREEMENT:
					loanWorkflowUserActionService.actionUploadSignedAgreements(loanDTO);
					break;

				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_ISSUED:
					moveToNextStep = Boolean.FALSE;
					newLoanDTO = loanWorkflowUserActionService
							.actionInformCustomerAfterDocsSigne(loanDTO);
					break;
				default: // TO HANDLE HISTORY ON SIMPLE NEXT DYNAMIC ACTION
					newLoanDTO = loanWorkflowUserActionService.actionDynamicWorkflow(loanDTO);
					break;
			}

			// move the loan to the next step
			if (Boolean.TRUE.equals(moveToNextStep)) {
				String connectedUserLogin = userClient.find().getLogin();
				// save user action for loan instance
				currentLoanInstanceStep.setActionUser(connectedUserLogin);
				loanInstanceService.save(currentLoanInstanceStep);

				// find next loan instance selon reviewFrom column

				// check if reviewFrom is not null ==> we are in the case of review only the
				// selected step
				// if reviewFrom is null ==> we are in the simple case of review
				LoanInstanceDTO nextLoanInstanceStep;
				if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getReviewFrom())) {
					nextLoanInstanceStep = loanInstanceDTOs.stream()
							.filter(instance -> instance.getCode()
									.equals(loanDTOFinal.getReviewFrom().intValue()))
							.findFirst().orElse(null);
				}
				else {
					// find next loan instance
					nextLoanInstanceStep = loanInstanceDTOs.stream().filter(
							instance -> instance.getCode() > (loanDTOFinal.getEtapeWorkflow()))
							.findFirst().orElse(null);
				}
				if (ACMValidationUtils.isNullOrEmpty(nextLoanInstanceStep)) {
					// next loan instance not found => end of workflow
					loanDTO.setWorkflowCompleted(Boolean.TRUE);
					// save loan and return
					Loan loan = mapper.map(loanDTO, Loan.class);

					CommonFunctions.mapperToUpdate(loan, userClient, logger);
					Loan newLoan = loanRepository.save(loan);
					// generate task for the next step if generation task in setting WF "of last
					// step" is active
					actualWorkFlowStepDTO.setGenerationTask(Boolean.FALSE);

					LoanCalendarSyncService loanCalendarSyncService =
							new LoanCalendarSyncServiceImpl(acmLoanInstanceGroupeAssociationService,
									notificationsServices, userClient, parametrageClient,
									crmClient);

					loanCalendarSyncService.generationTaskForStepLoan(loanDTO, null,
							actualWorkFlowStepDTO);
					if (!ACMValidationUtils.isNullOrEmpty(newLoanDTO.getOtherInformations())) {
						loadDataIBService.updateAcmLoanAndCustomerInIB(newLoanDTO);
					}
					return mapper.map(newLoan, LoanDTO.class);
				}
				// find step setting of the next step
				WorkFlowStepDTO nextWorkFlowStepDTOParam = new WorkFlowStepDTO();
				nextWorkFlowStepDTOParam
						.setIdWorkFlowStep(nextLoanInstanceStep.getCode().longValue());
				List<WorkFlowStepDTO> nextWorkFlowStepDTOs =
						parametrageClient.findWorkFlowSteps(nextWorkFlowStepDTOParam);
				WorkFlowStepDTO nextWorkFlowStepDTO = new WorkFlowStepDTO();
				if (ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTOs)) {
					throw new WorkFlowSettingException(
							new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
									CommonExceptionsMessage.WORKFLOW_SETTING_NOT_FOUND,
									new TechnicalException()),
							CommonExceptionsMessage.WORKFLOW_SETTING_NOT_FOUND);
				}
				else {
					nextWorkFlowStepDTO = nextWorkFlowStepDTOs.get(0);
				}
				// assign loan to the new owner
				// case of reviewed loan : the loan will be reviewed by the user who initially
				// worked on it

				loanDTO = assginLoanToOwner(nextLoanInstanceStep, nextWorkFlowStepDTO, loanDTO,
						loanInstanceDTOs);

				// set status workflow like etapeWorkflow only if statutWorkflow !== review
				if (!loanDTO.getStatutWorkflow().equals(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey())
						&& !loanDTO.getStatutWorkflow()
								.equals(CommonFunctions.mappingStatus(
										ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE)
										.getKey())) {
					loanDTO.setStatutWorkflow(nextLoanInstanceStep.getCode());
					loanDTO.setStatut(nextLoanInstanceStep.getCodeStatutLoan().intValue());
				}
				loanDTO.setStatutLibelle(nextLoanInstanceStep.getLibelle());
				loanDTO.setEtapeWorkflow(nextLoanInstanceStep.getCode());
				// generate task for the next step if generation task in setting WF is active

				LoanCalendarSyncService loanCalendarSyncService =
						new LoanCalendarSyncServiceImpl(acmLoanInstanceGroupeAssociationService,
								notificationsServices, userClient, parametrageClient, crmClient);

				loanCalendarSyncService.generationTaskForStepLoan(loanDTO,
						nextLoanInstanceStep.getCode().longValue(), nextWorkFlowStepDTO);

				// generate task for approval participants
				loanCalendarSyncService.generationTaskForApprovalParticipants(loanDTO,
						nextLoanInstanceStep.getCode().longValue(), nextWorkFlowStepDTO,
						nextLoanInstanceStep.getId());

				loanDTO.setReviewFrom(null);
				Loan loan = mapper.map(loanDTO, Loan.class);

				CommonFunctions.mapperToUpdate(loan, userClient, logger);
				Loan newLoan = loanRepository.save(loan);
				newLoanDTO = mapper.map(newLoan, LoanDTO.class);
				// set loan with his productDTO
				newLoanDTO.setProductDTO(loanDTO.getProductDTO());
				// call method for send notification to participants and approval participants
				if (!ACMValidationUtils.isNullOrEmpty(newLoanDTO.getIdIbLoan())) {
					// update ib loan and ib customer
					newLoanDTO.setCustomerDTO(
							customerService.find(newLoanDTO.getCustomerDTO().getId()));
					newLoanDTO.setStepPath(nextLoanInstanceStep.getIbIhmRoot());
					loadDataIBService.updateAcmLoanAndCustomerInIB(newLoanDTO);
					// if the next step has Jasper documents then save them in IB
					if (!ACMValidationUtils.isNullOrEmpty(nextWorkFlowStepDTO.getDocuments())) {
						for (SettingDocumentProductDTO doc : nextWorkFlowStepDTO.getDocuments()) {
							if (!ACMValidationUtils.isNullOrEmpty(doc.getReportName())) {
								// fill the jasper report and save it in IB
								ReportDTO r = new ReportDTO();
								ArrayList<LoanDTO> liste = new ArrayList<>();
								liste.add(newLoanDTO);
								r.setEntryList(liste);
								r.setTypeReport("AGREEMENT");
								r.setInputFileName(doc.getReportName());

								AcmDocumentsDTO d = new AcmDocumentsDTO();
								d.setTitre(doc.getSettingDocumentTypeDTO().getLibelle());
								d.setDescription(doc.getSettingDocumentTypeDTO().getLibelle());
								d.setAuteur("Auto-generated by ACM");
								d.setLoanId(newLoanDTO.getIdIbLoan());
								d.setSettingDocumentTypeDTO(doc.getSettingDocumentTypeDTO());
								d.setIdCustomer(newLoanDTO.getCustomerDTO().getIbCustomerId());
								d.setCustomerName(newLoanDTO.getCustomerDTO().getCustomerName());
								d.setAccountNumberExtern(
										newLoanDTO.getIdAccountExtern().toString());
								d.setMandatory(doc.getMandatory());
								d.setDocumentIndex(0);
								d.setName(doc.getSettingDocumentTypeDTO().getLibelle() + ".pdf");
								List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
								acmDocumentsDTOs.add(d);
								loadDataIBService.saveToGed(reportingClient.generateReport(r),
										acmDocumentsDTOs);
							}
						}
					}
				}
				loanCalendarSyncService.sendNotificationsToParticipants(currentLoanInstanceStep,
						loanDTO);

				Boolean serviceSMS = parametrageClient.find("SERVICE_SMS").getEnabled();

				if (actualWorkFlowStepDTO.getCodeAcmTemplateSms() != null) {

					AcmTemplateSMSDTO acmtemplateSMSDTO = new AcmTemplateSMSDTO();
					acmtemplateSMSDTO
							.setCodeSMSEvent(actualWorkFlowStepDTO.getCodeAcmTemplateSms());
					AcmTemplateSMSDTO acmtemplateSMSDTO1 =
							parametrageClient.findbycode(acmtemplateSMSDTO);
					if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getTelephone1())
							&& serviceSMS) {
						try {
							MessageDetailsDTO messageDetailsDTO = new MessageDetailsDTO();
							messageDetailsDTO.setToSender(loanDTO.getCustomerDTO().getTelephone1());

							// Utilisation d'une expression régulière pour identifier les balises
							// "${...}"
							Pattern pattern = Pattern.compile("\\$\\{([^}]+)\\}");
							Matcher matcher = pattern.matcher(acmtemplateSMSDTO1.getMessageBody());

							StringBuffer resultat = new StringBuffer();

							while (matcher.find()) {
								String nomEntite = matcher.group(1); // Capturer le nom de l'entité
								String valeurRemplacement = evaluerExpression(nomEntite, loanDTO);
								if (!valeurRemplacement.isEmpty()) {
									matcher.appendReplacement(resultat, valeurRemplacement);
								}

							}

							matcher.appendTail(resultat);
							String texteModifie = resultat.toString();

							messageDetailsDTO.setMessageBody(texteModifie);
							messageDetailsDTO.setCategory(acmtemplateSMSDTO1.getCategory());
							parametrageClient.saveSMS(messageDetailsDTO);
							parametrageClient.sendSms(messageDetailsDTO);

						}
						catch (Exception e) {
							logger.error(e.getMessage());
						}
					}
				}

				try {
					newLoanDTO = automaticStepLoan(newLoanDTO);
				}
				catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		else {
			throw new WorkFlowSettingException(
					new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
							CommonExceptionsMessage.WORKFLOW_INSTANCE_NOT_FOUND,
							new TechnicalException()),
					CommonExceptionsMessage.WORKFLOW_INSTANCE_NOT_FOUND);
		}
		return newLoanDTO;
	}

	/**
	 * Evaluer expression.
	 *
	 * @param expression the expression
	 * @param loanDTO the loan DTO
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#evaluerExpression(java.lang.String,
	 * com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public String evaluerExpression(String expression, LoanDTO loanDTO) {

		// Divisez l'expression en entité et champ
		String[] elements = expression.split("\\.");
		if (elements.length == 2) {
			String nomEntite = elements[0];
			String nomChamp = elements[1];

			// Utilisez la réflexion pour appeler la méthode correspondante
			try {

				// Class dtoClass = loanDTO.getClass();
				if (nomEntite.equals("customer")) {

					Class dtoClass = loanDTO.getCustomerDTO().getClass();
					// Get the method with the specified name
					Method method = dtoClass.getMethod(
							"get" + nomChamp.substring(0, 1).toUpperCase() + nomChamp.substring(1));
					if (method.getReturnType() == Date.class) {
						Date dateValue = (Date) method.invoke(loanDTO.getCustomerDTO());

						// Formatez la date en "jj/mm/aaaa" (24/10/2023)
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
						String formattedDate = dateFormat.format(dateValue);

						// formattedDate contient la date formatée

						return formattedDate;
					}
					else {

						String valeurChampStr =
								String.valueOf(method.invoke(loanDTO.getCustomerDTO()));
						return valeurChampStr;
					}
				}
				else if (nomEntite.equals("collection")) {
					return "Valeur introuvable";
				}
				else {
					Class dtoClass = loanDTO.getClass();
					// Get the method with the specified name
					Method method = dtoClass.getMethod(
							"get" + nomChamp.substring(0, 1).toUpperCase() + nomChamp.substring(1));
					if (method.getReturnType() == Date.class) {
						Date dateValue = (Date) method.invoke(loanDTO);

						// Formatez la date en "jj/mm/aaaa" (24/10/2023)
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
						String formattedDate = dateFormat.format(dateValue);

						// formattedDate contient la date formatée

						return formattedDate;
					}
					else {
						String value = String.valueOf(method.invoke(loanDTO));
						return value;
					}
				}

			}

			catch (Exception e) {
				e.printStackTrace();
			}
		}

		return "Valeur introuvable"; // Gestion des erreurs

	}

	/**
	 * Disburse loan.
	 *
	 * @author kouali
	 * @param loanDTO the loan DTO
	 * @return the disburse response
	 */
	private DisburseResponse disburseLoan(LoanDTO loanDTO) {

		DisburseDTO disburseDto = new DisburseDTO();
		disburseDto.setCustomerID(loanDTO.getCustomerDTO().getCustomerIdExtern());
		AccountChildTrn accountChildTrn = new AccountChildTrn();
		List<AccountChildTrn> accountChildTrns = new ArrayList<>();
		// set accountChildTrn
		accountChildTrn.setCuAccountID(loanDTO.getIdAccountExtern());
		accountChildTrn.setExpectedAmountCR(1.0);
		accountChildTrn.setExpectedAmountDR(1.0);
		// set transaction
		Transaction transaction = new Transaction();
		List<Transaction> transactions = new ArrayList<>();
		transaction.setAmount(loanDTO.getApprovelAmount().toString());
		transaction.setTransactionType(2);
		transactions.add(transaction);
		transactions.add(transaction);
		// set transaction in accountChildTrn
		accountChildTrn.setTransactions(transactions);
		// fill accountChildTrns list
		accountChildTrns.add(accountChildTrn);
		AccountFeeTrn accountFeeTrn = new AccountFeeTrn();
		List<AccountFeeTrn> accountFeeTrns = new ArrayList<>();
		// set transaction accountFeeTrn
		accountFeeTrn.setCuAccountID(loanDTO.getIdAccountExtern());
		accountFeeTrn
				.setCuFeeID(transversClient.getCuFeeIdByIdAcount(loanDTO.getIdAccountExtern()));
		accountFeeTrn.setDescription("fees");
		accountFeeTrn.setAmount(loanDTO.getFeeAmt1());
		// fill accountFeeTrns list
		accountFeeTrns.add(accountFeeTrn);
		AccountLoanTrn accountLoanTrn = new AccountLoanTrn();
		List<AccountLoanTrn> accountLoanTrns = new ArrayList<>();
		// set transaction in accountLoanTrn
		accountLoanTrn.setAccountChildTrn(accountChildTrns);
		accountLoanTrn.setAccountFeeTrn(accountFeeTrns);
		Transaction transactionLoan = new Transaction();
		List<Transaction> transactionsLoan = new ArrayList<>();
		// set transaction accountFeeTrn
		transactionLoan.setAmount(loanDTO.getApprovelAmount().toString());
		transactionLoan.setTransactionType(8);
		transactionsLoan.add(transactionLoan);
		// set transactionsLoan accountLoanTrn
		accountLoanTrn.setTransactions(transactionsLoan);
		accountLoanTrns.add(accountLoanTrn);
		// set accountLoanTrns in disburseDto
		disburseDto.setAccountLoanTrn(accountLoanTrns);
		// call api to disburse loan in abacus
		DisburseResponse disburseResponse = transversClient.disburseLoan(disburseDto);
		return disburseResponse;
	}

	/**
	 * Assgin loan to owner.
	 *
	 * @param nextLoanInstanceStep the next loan instance step
	 * @param nextWorkFlowStepDTO the next work flow step DTO
	 * @param loanDTO the loan DTO
	 * @param loanInstanceDTOs the loan instance DT os
	 * @return the loan DTO
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	public LoanDTO assginLoanToOwner(LoanInstanceDTO nextLoanInstanceStep,
			WorkFlowStepDTO nextWorkFlowStepDTO, LoanDTO loanDTO,
			List<LoanInstanceDTO> loanInstanceDTOs) throws WorkFlowSettingException {

		UserDTO userDTOResponsable = null;
		String connectedUserLogin = userClient.find().getLogin();
		if (!ACMValidationUtils.isNullOrEmpty(nextLoanInstanceStep.getActionUser())) {
			loanDTO.setOwner(nextLoanInstanceStep.getActionUser());
			if (nextLoanInstanceStep.getActionUser().equals(connectedUserLogin)) {
				loanDTO.setIhmRoot(nextLoanInstanceStep.getIhmRoot());
			}
			List<UserDTO> userDTOs =
					userClient.find(new UserDTO(nextLoanInstanceStep.getActionUser()));

			if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
				loanDTO.setOwnerName(userDTOs.get(0).getFullName());
				loanDTO.setOwnerEmail(userDTOs.get(0).getEmail());
			}
		}
		else if (nextWorkFlowStepDTO.getStepType().equals("link")) {
			loanDTO.setGroupOwner(null);
			loanDTO.setGroupOwnerName(null);
			UserDTO userDTOPram = new UserDTO();
			switch (nextWorkFlowStepDTO.getPreviousStep()) {
				// Manager of loan owner
				case "-1":
					userDTOPram.setLogin(loanDTO.getOwner());
					userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
					loanDTO.setOwner(userDTOResponsable.getLogin());
					loanDTO.setOwnerName(userDTOResponsable.getFullName());
					loanDTO.setOwnerEmail(userDTOResponsable.getEmail());
					break;
				// Loan owner
				case "-2":
					loanDTO.setIhmRoot(nextLoanInstanceStep.getIhmRoot());
					break;
				// Manager of loan portfolio owner
				case "-3":
					userDTOPram.setAccountPortfolioId(loanDTO.getPortfolioId());
					userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
					loanDTO.setOwner(userDTOResponsable.getLogin());
					loanDTO.setOwnerName(userDTOResponsable.getFullName());
					loanDTO.setOwnerEmail(userDTOResponsable.getEmail());
					break;
				// Loan portfolio owner
				case "-4":
					userDTOPram.setAccountPortfolioId(loanDTO.getPortfolioId());
					UserDTO userDTO = userClient.find(userDTOPram).get(0);
					loanDTO.setOwner(userDTO.getLogin());
					loanDTO.setOwnerName(userDTO.getFullName());
					loanDTO.setOwnerEmail(userDTO.getEmail());
					break;
				default:
					// find step setting
					String previousStepCode = nextWorkFlowStepDTO.getPreviousStep();
					LoanInstanceDTO linkedLoanInstanceDTO =
							loanInstanceDTOs.stream()
									.filter(instance -> Long
											.toString(instance.getOrderEtapeProcess())
											.equals(previousStepCode))
									.findFirst().orElse(null);

					if (!ACMValidationUtils.isNullOrEmpty(linkedLoanInstanceDTO)) {
						userDTOPram.setLogin(linkedLoanInstanceDTO.getActionUser());
						userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
						loanDTO.setOwner(userDTOResponsable.getLogin());
						loanDTO.setOwnerName(userDTOResponsable.getFullName());
						loanDTO.setOwnerEmail(userDTOResponsable.getEmail());
					}
					else {
						throw new WorkFlowSettingException(
								new ExceptionResponseMessage(
										CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
										CommonExceptionsMessage.WORKFLOW_INSTANCE_NOT_FOUND,
										new TechnicalException()),
								CommonExceptionsMessage.WORKFLOW_INSTANCE_NOT_FOUND);
					}

					break;
			}

		}
		else if (nextWorkFlowStepDTO.getStepType().equals("group")) {
			loanDTO = assignLoanToGroupOfUsers(nextWorkFlowStepDTO.getGroupCode(), loanDTO);
		}
		else if (nextWorkFlowStepDTO.getStepType().equals("customer")) {
			loanDTO.setAssignCustomer(true);
		}
		else {
			// TODO Exception
		}
		return loanDTO;
	}

	/**
	 * Reverse journal entry.
	 *
	 * @param currentLoanInstanceStep the current loan instance step
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void reverseJournalEntry(LoanInstanceDTO currentLoanInstanceStep)
			throws IOException, ApiAbacusException {

		final String reverssal = "Reversal- ";
		// check if for this loan and for this step, we have already sent a JE
		if (!ACMValidationUtils.isNullOrEmpty(currentLoanInstanceStep.getJournalEntry())) {
			// split string to remove ; delimiters
			String journalEntry = currentLoanInstanceStep.getJournalEntry();
			String[] splitString = journalEntry.split(";");
			ObjectMapper objectMapper = new ObjectMapper();
			for (int i = 0; i < splitString.length; i++) {
				// convert to JournalEnteriesToAbacusDTO
				JournalEnteriesToAbacusDTO journalEnteriesToAbacusDto =
						objectMapper.readValue(splitString[i], JournalEnteriesToAbacusDTO.class);
				// loop list JournalEnteriesInformationDTOs of each
				// journalEnteriesToAbacusDto
				for (JournalEnteriesInformationDTO journalEntryInformationDto : journalEnteriesToAbacusDto
						.getJournalEntry()) {
					// Reverse JE : Debit account will be credit account and credit account
					// will be debit account
					journalEntryInformationDto.setCredit(!journalEntryInformationDto.isCredit());
					// adding at the beginning the word Reversal
					journalEntryInformationDto.setDescription(
							reverssal + journalEntryInformationDto.getDescription());

				}
				journalEnteriesToAbacusDto.setPageDescription(
						reverssal + journalEnteriesToAbacusDto.getPageDescription());
				// send api to abacus
				try {
					transversClient.createJournalEntry(journalEnteriesToAbacusDto);
				}
				catch (Exception ex) {
					throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
							"journal entry exception : " + ex.getMessage());
				}
			}
		}
	}

	/**
	 * Date formatter.
	 *
	 * @return the string
	 */
	private String dateFormatter() {

		String pattern = "dd/MM/yyyy";
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
		String date = simpleDateFormat.format(new Date());
		return date;
	}

	/**
	 * Sets the charge fee.
	 *
	 * @param ChargeFeesDTOs the charge fees DT os
	 * @param responseChargeFeeDTO the response charge fee DTO
	 * @return the response charge fee DTO
	 */
	ResponseChargeFeeDTO setChargeFee(List<ChargeFeesDTO> ChargeFeesDTOs,
			ResponseChargeFeeDTO responseChargeFeeDTO) {

		for (ChargeFeesDTO item : ChargeFeesDTOs) {
			ChargeFeeDTO chargeFeeDTO = new ChargeFeeDTO();
			chargeFeeDTO.setManuallyAdded(true);
			chargeFeeDTO.setCanEditAmount(true);
			chargeFeeDTO.setPaid(false);
			chargeFeeDTO.setCharged(true);
			chargeFeeDTO.setAllowAmountChange(true);
			chargeFeeDTO.setVat(false);
			chargeFeeDTO.setCuFeeID(item.getCufeeId());
			chargeFeeDTO.setAmount(item.getAmount());
			chargeFeeDTO.setOpenBalance(item.getAmount());
			(responseChargeFeeDTO.getFees()).add(chargeFeeDTO);
		}
		return responseChargeFeeDTO;
	}

	/**
	 * Fees to charge.
	 *
	 * @param loanDTO the loan DTO
	 * @param actualWorkFlowStepDTO the actual work flow step DTO
	 * @param currentLoanInstanceStep the current loan instance step
	 * @param feesToCharge the fees to charge
	 * @return the list
	 */
	List<ChargeFeesDTO> addAutomaticFees(LoanDTO loanDTO, WorkFlowStepDTO actualWorkFlowStepDTO,
			LoanInstanceDTO currentLoanInstanceStep, List<ChargeFeesDTO> feesToCharge) {

		for (SettingChargeFeeDTO item : actualWorkFlowStepDTO.getListChargeFees()) {
			if (!item.getValue().equals(CommonConstants.MANUAL_ENTRY)) {
				ChargeFeesDTO chargefee = new ChargeFeesDTO();
				chargefee.setCode(item.getCode());
				chargefee.setLabel(item.getLabel());
				chargefee.setValue(item.getValue());
				chargefee.setIdLoanInstance(currentLoanInstanceStep.getId());
				chargefee.setCufeeId(item.getCufeeId());
				chargefee.setSettingFee(item.getId());
				if (CommonConstants.FIXED_AMOUNT.equals(item.getValue())) {
					if (item.getAmount() != null && !item.getAmount().equals(BigDecimal.ZERO)) {
						chargefee.setAmount(item.getAmount());
					}
				}
				if (CommonConstants.PERSONNAL_CONTRIBUTION.equals(item.getValue())) {
					BigDecimal chargeAmount = new BigDecimal(loanDTO.getPersonalContribution())
							.multiply(BigDecimal.valueOf(item.getPercentage()))
							.divide(BigDecimal.valueOf(100));
					chargefee.setAmount(chargeAmount);
				}
				if (CommonConstants.LOAN_AMOUNT.equals(item.getValue())) {
					BigDecimal chargeAmount = loanDTO.getApprovelAmount()
							.multiply(BigDecimal.valueOf(item.getPercentage()))
							.divide(BigDecimal.valueOf(100));
					chargefee.setAmount(chargeAmount);
				}
				feesToCharge.add(chargefee);
			}
		}
		return feesToCharge;
	}

	/**
	 * Adds the loan note.
	 *
	 * @param loanDTO the loan DTO
	 * @param feesToCharge the fees to charge
	 */
	void addLoanNote(LoanDTO loanDTO, List<ChargeFeesDTO> feesToCharge) {

		for (ChargeFeesDTO item : feesToCharge) {
			CustomerDecisionDTO feeNoteDTO = new CustomerDecisionDTO();
			feeNoteDTO.setIdLoan(loanDTO.getLoanId());
			feeNoteDTO.setStatusId(4);
			feeNoteDTO.setContactDate(new Date());
			feeNoteDTO.setAmount(loanDTO.getApprovelAmount());
			feeNoteDTO.setComments(
					"CHARGING FEES: " + item.getCode() + "= " + item.getAmount().toString());
			if (!item.getValue().equals(CommonConstants.MANUAL_ENTRY)) {
				customerDecisionService.save(feeNoteDTO, "SYSTEM");
			}
			else {
				customerDecisionService.save(feeNoteDTO);
			}
		}
	}

	/**
	 * Creates the journal entry in abacus.
	 *
	 * @param loanDTO the loan DTO
	 * @param workFlowStepDTO the work flow step DTO
	 * @param currentLoanInstanceStep the current loan instance step
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	String CreateJournalEntryInAbacus(LoanDTO loanDTO, WorkFlowStepDTO workFlowStepDTO,
			LoanInstanceDTO currentLoanInstanceStep) throws IOException, ApiAbacusException {

		/** The journal enteries to abacus. */
		JournalEnteriesToAbacusDTO journalEnteriesToAbacus;
		AtomicReference<String> requestCreateJournalEntry = new AtomicReference<>("");
		// reverse journal entry if already exist
		reverseJournalEntry(currentLoanInstanceStep);

		String formatedDate = dateFormatter();
		for (SettingJournalEntryTypeDTO item : workFlowStepDTO.getJournalEntryTypes()) {

			journalEnteriesToAbacus = new JournalEnteriesToAbacusDTO();
			journalEnteriesToAbacus.setJournalID(item.getJournalId());
			journalEnteriesToAbacus.setPageDescription(item.getDescription() + "-"
					+ loanDTO.getAccountNumber() + "-" + new Date().toString());
			journalEnteriesToAbacus.setBranchID(loanDTO.getBranchID());
			journalEnteriesToAbacus.setCurrencyID(1);

			for (SettingJournalEnteriesDTO journalEnteriesItem : item.getSettingJournalEnteries()) {

				JournalEnteriesInformationDTO journaEnterieInformation =
						new JournalEnteriesInformationDTO();
				journaEnterieInformation.setDescription(journalEnteriesItem.getDescription());
				journaEnterieInformation.setReference(loanDTO.getAccountNumber());
				journaEnterieInformation.setAccountID(transversClient.findMainAccount(
						journalEnteriesItem.getIdCreditAccount(), loanDTO.getBranchID()));
				journaEnterieInformation.setCredit(true);
				journaEnterieInformation.setValueDate(formatedDate);

				switch (journalEnteriesItem.getAmount()) {
					case "Loan interest":

						journaEnterieInformation.setAmount((loanDTO.getTotalInterest()
								.multiply(new BigDecimal(journalEnteriesItem.getPercentage())))
										.divide(new BigDecimal(100)));
						journaEnterieInformation.setCurrencyAmount((loanDTO.getTotalInterest()
								.multiply(new BigDecimal(journalEnteriesItem.getPercentage())))
										.divide(new BigDecimal(100)));
						break;
					case "Total asset":
						journaEnterieInformation.setAmount((((loanDTO.getApprovelAmount()
								.add(new BigDecimal(loanDTO.getPersonalContribution()))).multiply(
										new BigDecimal(journalEnteriesItem.getPercentage())))
												.divide(new BigDecimal(100))));
						journaEnterieInformation.setCurrencyAmount((((loanDTO.getApprovelAmount()
								.add(new BigDecimal(loanDTO.getPersonalContribution()))).multiply(
										new BigDecimal(journalEnteriesItem.getPercentage())))
												.divide(new BigDecimal(100))));
						break;
					case "Personnel contribution":
						journaEnterieInformation.setAmount(
								(new BigDecimal(loanDTO.getPersonalContribution()).multiply(
										new BigDecimal(journalEnteriesItem.getPercentage())))
												.divide(new BigDecimal(100)));
						journaEnterieInformation.setCurrencyAmount(
								(new BigDecimal(loanDTO.getPersonalContribution()).multiply(
										new BigDecimal(journalEnteriesItem.getPercentage())))
												.divide(new BigDecimal(100)));
						break;
					default:
						journaEnterieInformation.setAmount((loanDTO.getApprovelAmount()
								.multiply(new BigDecimal(journalEnteriesItem.getPercentage())))
										.divide(new BigDecimal(100)));
						journaEnterieInformation.setCurrencyAmount((loanDTO.getApprovelAmount()
								.multiply(new BigDecimal(journalEnteriesItem.getPercentage())))
										.divide(new BigDecimal(100)));
						break;
				}
				journaEnterieInformation.setCurrencyID(1);
				journaEnterieInformation.setExchangeRate(1);
				journaEnterieInformation.setBranchID(loanDTO.getBranchID());
				journaEnterieInformation.setJournalID(1);
				journaEnterieInformation.setIsIBTEntryRequired(false);
				journaEnterieInformation.setIsIBTEntry(false);
				if (!BigDecimal.ZERO.equals(journaEnterieInformation.getAmount())) {
					journalEnteriesToAbacus.addJournalEntry(journaEnterieInformation);

					JournalEnteriesInformationDTO journaEnterieInformationDebit =
							new JournalEnteriesInformationDTO();

					mapper.map(journaEnterieInformation, journaEnterieInformationDebit);
					journaEnterieInformationDebit.setCredit(false);
					journaEnterieInformationDebit.setAccountID(transversClient.findMainAccount(
							journalEnteriesItem.getIdDebitAcount(), loanDTO.getBranchID()));
					journalEnteriesToAbacus.addJournalEntry(journaEnterieInformationDebit);
				}
			}
			if (!ACMValidationUtils.isNullOrEmpty(journalEnteriesToAbacus)) {
				try {
					requestCreateJournalEntry.set(requestCreateJournalEntry
							+ transversClient.createJournalEntry(journalEnteriesToAbacus) + ";");
				}
				catch (Exception ex) {
					throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
							"journal entry exception : " + ex.getMessage());

				}
			}
		}

		return requestCreateJournalEntry.get();
	}

	/**
	 * The method used approve loan the given {@link LoanDTO}.
	 *
	 * @author ymezrani
	 * @param loanDTO the loan DTO
	 * @param currentLoanInstanceStep the current loan instance step
	 * @param actualWorkFlowStepDTO the actual work flow step DTO
	 * @return the loan DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private boolean actionDynamicLoanApprove(LoanDTO loanDTO,
			LoanInstanceDTO currentLoanInstanceStep, WorkFlowStepDTO actualWorkFlowStepDTO)
			throws ApiAbacusException, ResourcesNotFoundException, IOException {

		boolean moveToNext = true;

		// update loan in ABACUS
		try {
			if (Boolean.TRUE.equals(loanDTO.getUpdateLoanAbacus())) {
				// update to abacus via API
				transversClient.updateLoanINDIVORG(loanDTO);
			}
		}
		catch (Exception e) {
			logger.error("Failed to add loan {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
		// save historique approvel data
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getNote())) {
			if (!loanDTO.getNote().equals("Automatically Approved")) {
				loanDTO.setNote(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED)
						.getValue());
			}
		}
		LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO = new LoanApprovalHistoriqueDTO(loanDTO,
				loanDTO.getApprovelAmount().longValue(),
				CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED)
						.getKey(),
				loanDTO.getNote(), loanDTO.getStatutWorkflow());

		loanApprovalHistoriqueDTO.setApprovalLevelLabel(actualWorkFlowStepDTO.getStepName());
		loanApprovalHistoriqueDTO.setApprovalDesicionLabel(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED)
				.getValue());
		loanApprovalHistoriqueService.save(loanApprovalHistoriqueDTO);
		moveToNext = checkApprovalGroups(loanDTO, currentLoanInstanceStep);

		return moveToNext;
	}

	/**
	 * Check approval groups.
	 *
	 * @author ymezrani
	 * @param loanDTO the loan DTO
	 * @param currentLoanInstanceStep the current loan instance step
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private boolean checkApprovalGroups(LoanDTO loanDTO, LoanInstanceDTO currentLoanInstanceStep)
			throws ResourcesNotFoundException {

		boolean moveToNext = true;
		// check if all loan approvals are validated
		List<AcmLoanInstanceAcmGroupeApprovalDTO> notValidatedLoanApprovals;
		AcmLoanInstanceAcmGroupeApprovalDTO loanInstanceGroupeApprovalParam =
				new AcmLoanInstanceAcmGroupeApprovalDTO();

		loanInstanceGroupeApprovalParam.setLoanInstance(currentLoanInstanceStep);
		loanInstanceGroupeApprovalParam.setValidation(Boolean.FALSE);
		notValidatedLoanApprovals =
				acmLoanInstanceGroupeAssociationService.find(loanInstanceGroupeApprovalParam);

		// Loop the list of groups selected in setting workflow step
		if (!ACMValidationUtils.isNullOrEmpty(notValidatedLoanApprovals)) {
			for (int va = 0; va < notValidatedLoanApprovals.size(); va++) {
				// Get the connected user
				UserDTO userConnected = userClient.find();
				List<GroupeDTO> connectedUserGroupeDTOs = new ArrayList<>();
				// Convert set to list
				connectedUserGroupeDTOs.addAll(userConnected.getGroupes());

				// Loop the list group of the userConnected
				for (int kp = 0; kp < connectedUserGroupeDTOs.size(); kp++) {

					// Check if the idGroup of the user
					// connected is in list of groups
					if (notValidatedLoanApprovals.get(va).getGroupe().getId()
							.equals(connectedUserGroupeDTOs.get(kp).getId())) {

						AcmLoanInstanceAcmGroupeApprovalDTO loanInGpDTO =
								notValidatedLoanApprovals.get(va);
						loanInGpDTO.setValidation(true);
						loanInGpDTO.setOwnerName(userConnected.getSimpleName());
						loanInGpDTO.setOwner(userConnected.getLogin());

						acmLoanInstanceGroupeAssociationService.save(loanInGpDTO.getId(),
								loanInGpDTO);
						break;

					}
				}
			}

			if (notValidatedLoanApprovals.stream().noneMatch(
					loanApproval -> Boolean.FALSE.equals(loanApproval.getValidation()))) {
				loanDTO = save(loanDTO.getLoanId(), loanDTO);
			}
			else {
				loanDTO.setIhmRoot("");
				moveToNext = false;

			}
		}
		return moveToNext;
	}

	/**
	 * Loan review.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#LoanReview(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO LoanReview(LoanDTO loanDTO) throws ResourcesNotFoundException {

		List<AcmLoanInstanceAcmGroupeApprovalDTO> listValidators = new ArrayList<>();
		Preconditions.checkNotNull(loanDTO.getIdLoanExtern(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// get actual loanInstance code
		Integer actualLoanInstanceCode = loanDTO.getReviewFrom().intValue();
		// if "review only selected step" is checked then save 'reviewFrom' value in DB
		// ;else save
		// 'reviewFrom'
		// value as null
		if (!Boolean.TRUE.equals(loanDTO.getReviewOnlySelectedStep())) {
			loanDTO.setReviewFrom(null);
		}
		// call the same methode actionCorrectifs for review
		loanDTO = loanWorkflowUserActionService.actionCorrectifs(loanDTO);

		Loan loan = mapper.map(loanDTO, Loan.class);
		CommonFunctions.mapperToUpdate(loan, userClient, logger);
		Loan newLoan = loanRepository.save(loan);
		LoanDTO newLoanDTO = mapper.map(newLoan, LoanDTO.class);
		// reset all approvers of all the loanInstances to False
		newLoanDTO.getLoanInstancesDtos().forEach(loanInstanceDTO -> {
			AcmLoanInstanceAcmGroupeApprovalDTO loanInstanceGroupDTO =
					new AcmLoanInstanceAcmGroupeApprovalDTO();
			// find list approvers by loanInstanceId
			loanInstanceGroupDTO.setLoanInstance(loanInstanceDTO);
			listValidators
					.addAll(acmLoanInstanceGroupeAssociationService.find(loanInstanceGroupDTO));
			// reset all approvers'validations to False
			if (!ACMValidationUtils.isNullOrEmpty(listValidators)) {
				listValidators.forEach(validator -> validator.setValidation(Boolean.FALSE));
			}

			if (loanInstanceDTO.getCode().equals(actualLoanInstanceCode)) {
				// set actionUser of actual loanInstance with connectedUser
				loanInstanceDTO.setActionUser(userClient.find().getLogin());
				try {
					loanInstanceService.save(loanInstanceDTO.getId(), loanInstanceDTO);
				}
				catch (ResourcesNotFoundException e) {
					logger.error(
							"Exception in method loanInstanceService.save(id,loanInstance) : {} ",
							e.getMessage());
					e.printStackTrace();
				}
			}

		});
		// update list approvers
		acmLoanInstanceGroupeAssociationService.updateAll(listValidators);
		return newLoanDTO;
	}

	/**
	 * Find setting WF steps.
	 *
	 * @param loanDTO the loan DTO
	 * @return the list
	 */
	private List<WorkFlowStepDTO> findSettingWFSteps(LoanDTO loanDTO) {

		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getLoanApplicationStatus())) {
			loanDTO.setLoanApplicationStatus(CommonConstants.NEW_APPLICATION);
		}

		// Find WF steps by Loan Product ID
		WorkFlowStepDTO workFlowStepDTO = new WorkFlowStepDTO();
		workFlowStepDTO.setProductId(loanDTO.getProductId());
		workFlowStepDTO.setEnabled(Boolean.TRUE);
		workFlowStepDTO.setMinAmount(loanDTO.getApplyAmountTotal());
		workFlowStepDTO.setProcess(loanDTO.getLoanApplicationStatus());

		List<WorkFlowStepDTO> workFlowStepDTOs =
				parametrageClient.findWorkFlowSteps(workFlowStepDTO);

		return workFlowStepDTOs.stream().sorted().collect(Collectors.toList());
	}

	/**
	 * Count topups by account.
	 *
	 * @param accountId the account id
	 * @return the integer
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#countTopupsByAccount(java.lang.Long)
	 */
	@Override
	public Integer countTopupsByAccount(Long accountId) {

		// check if accountId is null
		Preconditions.checkNotNull(accountId, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Init count
		Integer count = 0;
		// get number of topups group by the accountId
		count = loanRepository.countByLoanApplicationStatusAndIdAccountExternAndEnabled(
				CommonConstants.REFINANCE, CommonConstants.TOPUP, accountId, Boolean.TRUE);

		return count;
	}

	/**
	 * Find by supplier.
	 *
	 * @param supplierId the supplier id
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findBySupplier(java.lang.Long)
	 */
	@Override
	public List<LoanDTO> findBySupplier(Long supplierId) {

		// TODO change the query from native to dslQuery
		logger.info("Calculate Supplier Loan Informations " + supplierId);
		Query q = entityManager.createNativeQuery(
				"select sup.ID_ACM_SUPPLIER, sup.NAME , al.ID_ACM_LOAN, sum(al.QUANTITE_ARTICLE) qt, sum(coalesce(al.PRIX_UNITAIRE * (100-al.REMISE_ARTICLE) /100 * al.QUANTITE_ARTICLE, 0)) \r\n"
						+ "from ACM_ASSET_ACM_LOAN al join ACM_ASSET ass on ass.ID_ACM_ASSET = al.ID_ACM_ASSET join ACM_SUPPLIER sup on sup.ID_ACM_SUPPLIER = ass.SUPPLIER_ID\r\n"
						+ "where sup.ID_ACM_SUPPLIER =? group by sup.ID_ACM_SUPPLIER, sup.NAME, al.ID_ACM_LOAN");
		q.setParameter(1, supplierId);
		List<LoanDTO> listloans = new ArrayList<>();
		List<Object[]> infos = q.getResultList();
		if (!ACMValidationUtils.isNullOrEmpty(infos)) {
			infos.forEach((data) -> {
				Loan loanSup = loanRepository.getOne(new Long(data[2].toString()));
				LoanDTO loanDTO = mapper.map(loanSup, LoanDTO.class);
				loanDTO.setQuantitySupplier((int) data[3]);
				loanDTO.setAccountNumber(loanSup.getAccountNumberExtern());
				loanDTO.setBalanceSupplier(new BigDecimal(data[4].toString()));
				CustomerDTO customerDTO = new CustomerDTO();
				try {
					customerDTO = customerService.find(loanSup.getCustomer().getId());
					loanDTO.setCustomerDTO(customerDTO);
				}
				catch (ResourcesNotFoundException e) {
					logger.error("cannot find customer ID {}", loanSup.getCustomerId());
					e.printStackTrace();
				}

				listloans.add(loanDTO);
			});
		}

		return listloans;
	}

	/**
	 * Assign loan to group of users.
	 *
	 * @author mlamloum
	 * @param groupeCode the groupe code
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	private LoanDTO assignLoanToGroupOfUsers(String groupeCode, LoanDTO loanDTO) {

		// get the list of users that have loanBranch in their accessBranch
		List<UserDTO> userDTOParam = userClient
				.findByGroupeCodeAndBranchIDAndAccessBranches(groupeCode, loanDTO.getBranchID());

		// if the group has only one user( with loan branch IN his list of access
		// branch) then
		// assign to this user
		if (userDTOParam.size() == 1) {
			loanDTO.setOwner(userDTOParam.get(0).getLogin());
			loanDTO.setOwnerName(userDTOParam.get(0).getSimpleName());
			loanDTO.setAssignedToOneUser(Boolean.TRUE);
			// setting list participants
			LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
					.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
			logger.info("(process Audit-Risk ) setting Loan Participants with ID = [{}] :: DONE",
					newLoanParticipantsDTO.getId());
		}
		else {
			// set owner and owner name null
			loanDTO.setOwner(null);
			loanDTO.setOwnerName(null);
			// set owner group and owner group name
			loanDTO.setGroupOwner(userDTOParam.get(0).getGroupes().iterator().next().getCode());
			loanDTO.setGroupOwnerName(
					userDTOParam.get(0).getGroupes().iterator().next().getLibelle());
			loanDTO.setAssignedToOneUser(Boolean.FALSE);
		}
		return loanDTO;
	}

	/**
	 * Find by search query id.
	 *
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findBySearchQueryId(com.acm.utils.dtos.LoanDTO)
	 */

	/**
	 * Find by customer id reis.
	 *
	 * @param loanDTO the loan DTO
	 * @return the listF
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#findByCustomerIdReis(com.acm.utils.dtos.LoanDTO)
	 */

	/**
	 * Job loans sanad.
	 */
	@Override
	public void jobLoansSanad() {

		// get list id of loans to execute on them the job
		List<BigInteger> listIdLoans = loanRepository.getLoansToTurnOnThemJob();
		if (!ACMValidationUtils.isNullOrEmpty(listIdLoans)) {
			for (BigInteger idLoan : listIdLoans) {
				LoanDTO loanDto = null;
				try {
					// find loan and check if his workflow not completed
					loanDto = find(idLoan.longValue());
					if (!ACMValidationUtils.isNullOrEmpty(loanDto)
							&& !Boolean.TRUE.equals(loanDto.getWorkflowCompleted())) {
						// call validate
						stepWorkFlow(loanDto);
					}
				}
				catch (Exception e) {
					logger.error("cannot execute job on this loan ID {}", loanDto.getLoanId());

				}
			}

		}

	}

	/**
	 * Update max installment.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanService#updateMaxInstallment(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO updateMaxInstallment(LoanDTO loanDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO.getLoanId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update loan  with ID = {}", loanDTO.getLoanId());
		Loan oldLoan = loanRepository.findById(loanDTO.getLoanId()).orElse(null);
		// check if object is null
		if (oldLoan == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Loan.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + loanDTO.getLoanId().toString());
		}
		// mapping new data with existing data (oldLoan)
		oldLoan.setMaxInstallment(loanDTO.getMaxInstallment());
		CommonFunctions.mapperToUpdate(oldLoan, userClient, logger);
		Loan newLoan = loanRepository.save(oldLoan);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Loan.class.getSimpleName());
		return mapper.map(newLoan, LoanDTO.class);
	}

	/**
	 * Back loan to fist step in sign contract.
	 */
	@Scheduled(cron = "#{@getPeriodValue}")
	public void backLoanToFistStepInSignContract() {

		Integer periodToBackLoan = Integer.parseInt(
				parametrageClient.find("PERIOD_BACK_LOAN_TO_FIRST_STEP_SIGN_CONTRACT").getValue());

		List<Loan> loans = loanRepository
				.findByLoanInstancesIbIhmRoot(CommonConstants.IB_IHM_ROOT_SIGN_CONTRACT);
		Boolean isCompleted;
		for (Loan loan : loans) {
			long hourDifference = ChronoUnit.HOURS
					.between(parseTimeString(loan.getSignContarctValidation()), LocalTime.now());
			isCompleted = isWorkFlowSignContractCompleted(loan.getLoanInstances());
			if (hourDifference >= periodToBackLoan && Boolean.TRUE.equals(!isCompleted)) {
				Set<LoanInstance> loanInstances = loan.getLoanInstances();
				List<LoanInstance> loanInstanceSignContarct = loanInstances.stream()
						.filter(item -> item.getIbIhmRoot()
								.equals(CommonConstants.IB_IHM_ROOT_SIGN_CONTRACT))
						.collect(Collectors.toList());
				loan.setEtapeWorkflow(loanInstanceSignContarct.get(0).getCode());
				loan.setSignContarctValidation(null);
				loan.setStatutWorkflow(loanInstanceSignContarct.get(0).getCode());
				WorkFlowStepDTO workFlowStepDTO = new WorkFlowStepDTO();
				workFlowStepDTO
						.setIdWorkFlowStep(new Long(loanInstanceSignContarct.get(0).getCode()));
				List<WorkFlowStepDTO> workFlowStepDTOs =
						parametrageClient.findWorkFlowSteps(workFlowStepDTO);
				loan.setStatutLibelle(workFlowStepDTOs.get(0).getStepName());
				loanRepository.save(loan);

			}

		}

	}

	/**
	 * Check API murabha purchase.
	 *
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	private boolean checkAPIMurabhaPurchase(LoanDTO loanDTO) {

		PurchaseMurabhaApiRequestDTO purchaseMurabhaApiRequestDTO =
				new PurchaseMurabhaApiRequestDTO();
		purchaseMurabhaApiRequestDTO.setClientCode("S101");
		purchaseMurabhaApiRequestDTO.setSeller("EIGER");
		purchaseMurabhaApiRequestDTO.setProductCode("ML1");
		purchaseMurabhaApiRequestDTO.setCommodity("PA");
		// Obtenir la date actuelle au format ISO 8601
		LocalDateTime now = LocalDateTime.now();
		// Formatter la date au format "yyyy-MM-dd'T'HH:mm:ss'Z'"
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'");
		String formattedDate = now.format(formatter);
		purchaseMurabhaApiRequestDTO.setSettlementDate(formattedDate);
		// customer can be anonyme
		purchaseMurabhaApiRequestDTO.setCustomer(loanDTO.getCustomerName());
		// currency USD
		purchaseMurabhaApiRequestDTO.setCurrencyCode("SAR");

		purchaseMurabhaApiRequestDTO.setPurchasePrice(loanDTO.getApplyAmountTotal());

		// call 1st api purchase
		ResponseEntity<PurchaseMurabhaApiResponseDTO> responsePurchaseApi =
				transversClient.purchase(purchaseMurabhaApiRequestDTO, loanDTO.getLoanId());

		if (!ACMValidationUtils.isNullOrEmpty(responsePurchaseApi.getBody())
				&& responsePurchaseApi.getBody().getEigerResultCode() == 200) {

			ConfirmPurchaseOrSaleRequestApiDTO confirmPurchaseRequestDTO =
					new ConfirmPurchaseOrSaleRequestApiDTO();

			confirmPurchaseRequestDTO
					.setReferenceId(responsePurchaseApi.getBody().getReferenceId());
			confirmPurchaseRequestDTO.setClientCode("S101");

			// call 2nd api confirm purchase and use the reference id of response purchase
			// api
			ResponseEntity<ConfirmPurchaseResponseApiDTO> responseConfirmPurchaseApi =
					transversClient.confirmPurchase(confirmPurchaseRequestDTO, loanDTO.getLoanId());

			if (!ACMValidationUtils.isNullOrEmpty(responseConfirmPurchaseApi.getBody())
					&& responseConfirmPurchaseApi.getBody().getEigerResultCode() == 200) {
				return true;

			}

		}

		return false;
	}

	/**
	 * Checks if is work flow sign contract completed.
	 *
	 * @param loanInstances the loan instances
	 * @return the boolean
	 */
	private Boolean isWorkFlowSignContractCompleted(Set<LoanInstance> loanInstances) {

		List<LoanInstance> loanInstanceSignContarct = loanInstances.stream().filter(
				item -> item.getIbIhmRoot().equals(CommonConstants.IB_IHM_ROOT_SIGN_CONTRACT))
				.collect(Collectors.toList());
		if (loanInstanceSignContarct.stream()
				.filter(item -> ACMValidationUtils.isNullOrEmpty(item.getActionUser()))
				.collect(Collectors.toList()).isEmpty()) {
			return true;
		}

		return false;

	}

	/**
	 * Check API murabha sell.
	 *
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	private boolean checkAPIMurabhaSell(LoanDTO loanDTO) {

		List<ThirdPartyHistorique> thirdPartyHistorique = thirdPartyHistoriqueRepository
				.findByIdLoanAndCategoryOrderByIdDesc(loanDTO.getLoanId(), "MURABHA/PURCHASE");

		String responseValue = thirdPartyHistorique.get(0).getResponseValue();

		// Split the input string by commas and spaces
		String[] parts = responseValue.split("[,\\s]+");

		// Loop through the parts to find referenceId
		String referenceId = null;
		for (String part : parts) {
			if (part.startsWith("referenceId=")) {
				// Extract the value of referenceId
				referenceId = part.substring("referenceId=".length());
				break;
			}
		}

		SaleMurabhaApiRequestDTO salePurchaseRequestDTO = new SaleMurabhaApiRequestDTO();
		salePurchaseRequestDTO.setReferenceId(referenceId);
		salePurchaseRequestDTO.setClientCode("S101");
		salePurchaseRequestDTO.setThirdPartyBuyer("FALCON");

		ResponseEntity<ConfirmPurchaseResponseApiDTO> responseSale =
				transversClient.sale(salePurchaseRequestDTO, loanDTO.getLoanId());

		if (!ACMValidationUtils.isNullOrEmpty(responseSale.getBody())
				&& responseSale.getBody().getEigerResultCode() == 200) {

			ConfirmPurchaseOrSaleRequestApiDTO confirmSaleRequestDTO =
					new ConfirmPurchaseOrSaleRequestApiDTO();
			confirmSaleRequestDTO.setReferenceId(referenceId);
			confirmSaleRequestDTO.setClientCode("S101");
			ResponseEntity<ConfirmPurchaseResponseApiDTO> responseConfirmSale =
					transversClient.confirmSale(confirmSaleRequestDTO, loanDTO.getLoanId());

			if (!ACMValidationUtils.isNullOrEmpty(responseConfirmSale.getBody())
					&& responseConfirmSale.getBody().getEigerResultCode() == 200) {
				return true;

			}
		}

		return false;
	}

	/**
	 * Check API transfer notice.
	 *
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	private boolean checkAPITransferNotice(LoanDTO loanDTO) {

		List<ThirdPartyHistorique> thirdPartyHistorique = thirdPartyHistoriqueRepository
				.findByIdLoanAndCategoryOrderByIdDesc(loanDTO.getLoanId(), "MURABHA/PURCHASE");

		String responseValue = thirdPartyHistorique.get(0).getResponseValue();

		// Split the input string by commas and spaces
		String[] parts = responseValue.split("[,\\s]+");

		// Loop through the parts to find referenceId
		String referenceId = null;
		for (String part : parts) {
			if (part.startsWith("referenceId=")) {
				// Extract the value of referenceId
				referenceId = part.substring("referenceId=".length());
				break;
			}
		}

		ConfirmPurchaseOrSaleRequestApiDTO transferNotice =
				new ConfirmPurchaseOrSaleRequestApiDTO();
		transferNotice.setReferenceId(referenceId);
		transferNotice.setClientCode("S101");

		ResponseEntity<ConfirmPurchaseResponseApiDTO> responseTransferNotice =
				transversClient.transferNotice(transferNotice, loanDTO.getLoanId());

		if (!ACMValidationUtils.isNullOrEmpty(responseTransferNotice.getBody())
				&& responseTransferNotice.getBody().getEigerResultCode() == 200) {

			return true;
		}

		return false;

	}

	/**
	 * Parses the time string.
	 *
	 * @param inputString the input string
	 * @return the local time
	 */
	public static LocalTime parseTimeString(String inputString) {

		try {
			// Define the format of the input string
			inputString = inputString.substring(0, 5);
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");

			// Parse the input string into a LocalTime object
			return LocalTime.parse(inputString, formatter);
		}
		catch (DateTimeParseException e) {
			// Handle parsing error
			return null;
		}

	}

	/**
	 * Check API simah score.
	 *
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	private boolean checkAPISimahScore(LoanDTO loanDTO) {

		RequestGetScoreDTO request = new RequestGetScoreDTO();

		IdentityInfoDTO identityInforDTO = new IdentityInfoDTO();
		identityInforDTO.setIdNumber(loanDTO.getCustomerDTO().getIdentity());
		// Id type of the customer - Get Values from ‘getidtypes’ lookup ; i put 1 just for testing
		identityInforDTO.setIdType(1);

		request.setIdentityInfo(identityInforDTO);

		ApplicationDetailsDTO applicationDetailsDTO = new ApplicationDetailsDTO();
		applicationDetailsDTO.setAmount(loanDTO.getApprovelAmount().intValue());
		// Product type for which enquiry is performed - Get Values from ‘getallproducts’ lookup; i
		// put 1 juste for testing
		applicationDetailsDTO.setProductType(1);

		request.setApplicationDetails(applicationDetailsDTO);

		DemographicInfoDTO demoGraphicInfoDTO = new DemographicInfoDTO();
		// Get Values from ‘nationalities’ lookup Nationality depends on ID Type.
		// If ID Type=National ID, then Nationailty must be Saudi Arabia If ID Type = Iqama
		// ID,Passport,Diplomat and Visitor
		// then Nationailty must NOT be Saudi Arabia If ID Type=GCC Customers,
		// then Nationailty must be Bahrain, Kuwait, Oman, Qatar or UAE.
		demoGraphicInfoDTO.setNationality(1);
		// Martial Status of the customer - Get Values from ‘maritalstatus’ lookup
		demoGraphicInfoDTO.setMaritalStatus(1);
		demoGraphicInfoDTO.setHijriIDExpiryDate(false);
		// demoGraphicInfoDTO.setDateOfBirth(loanDTO.getCustomerDTO().getDateOfBirth());
		// First name of the customer (Arabic/English) - The language depends on Request Header
		// param language=en/ar.
		// If ‘en’ then provide name in English, else if ‘ar’ then provide name in Arabic
		demoGraphicInfoDTO.setFirstName(loanDTO.getCustomerDTO().getFirstName());
		demoGraphicInfoDTO.setGender("M".equals(loanDTO.getCustomerDTO().getGender()) ? 1 : 0);
		demoGraphicInfoDTO.setFamilyName(loanDTO.getCustomerDTO().getLastName());

		request.setDemographicInfo(demoGraphicInfoDTO);

		try {
			ResponseEntity<String> responseApi = transversClient.getScoreSimah(request);

			if (responseApi.getStatusCodeValue() == 200) {
				// TODO

			}
		}
		catch (Exception ex) {
			logger.info("Exception SCORE SIMAH API : {}", ex.getMessage());
			return false;
		}
		return false;
	}

	/**
	 * Check mofeed api.
	 *
	 * @param loanDTO the loan DTO
	 * @return the boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private Boolean checkMofeedApi(LoanDTO loanDTO) throws ResourcesNotFoundException {

		ResponseEntity<EmploymentStatusInfoMasdrAPIDTO> responseMofeedApi = null;
		try {

			responseMofeedApi = transversClient.mofeedApi(loanDTO.getCustomerDTO().getIdentity(),
					loanDTO.getLoanId());

			// if status 200 : we use the api mofeed on uat
			if (!ACMValidationUtils.isNullOrEmpty(responseMofeedApi.getBody())

					&& !ACMValidationUtils
							.isNullOrEmpty(responseMofeedApi.getBody().getEmploymentStatusInfo())) {

				// Calculate the sum of basicWage using foreach
				double sumOfBasicWage = 0;
				for (EmploymentStatusEntryMasdrAPIDTO info : responseMofeedApi.getBody()
						.getEmploymentStatusInfo()) {
					sumOfBasicWage += info.getBasicWage();
				}

				BigDecimal salaryFromMofeed = new BigDecimal(sumOfBasicWage);

				return calculSalaryAndUpdateAcmLoanInIB(loanDTO, salaryFromMofeed);

			}
		}
		catch (Exception e) {

			// if status not ok we continue using this code for the test in our local
			// environement

			if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getOtherInformations())) {
				int salaryMockMofeed = 0;
				String otherInfos = loanDTO.getOtherInformations();
				String[] parts = otherInfos.split(",");
				for (String part : parts) {
					if (part.contains("basicSalary")) {
						String[] keyValueMap = part.split(":");
						String tempVar = keyValueMap[2];
						salaryMockMofeed = Integer.parseInt(tempVar);
					}
				}
				BigDecimal salaryFromMofeed = new BigDecimal(salaryMockMofeed);

				return calculSalaryAndUpdateAcmLoanInIB(loanDTO, salaryFromMofeed);
			}

		}
		return false;

	}

	/**
	 * Calcul salary and update acm loan in IB.
	 *
	 * @param loanDTO the loan DTO
	 * @param salary the salary
	 * @return true, if successful
	 */
	private boolean calculSalaryAndUpdateAcmLoanInIB(LoanDTO loanDTO, BigDecimal salary) {

		BigDecimal maxInstallment = simahClassDetailsService.calculateMaxInstallment(salary);
		loanDTO.setMaxInstallment(maxInstallment);
		try {
			updateMaxInstallment(loanDTO);
		}
		catch (ResourcesNotFoundException e) {
			logger.info("exception in update maxInstallemnt  {}", e.getMessage());
		}
		/** in case maxInstallment or salaryFromDakhli/Mofeed is equal to zero */
		if (loanDTO.getMaxInstallment().equals(BigDecimal.ZERO) || salary.equals(BigDecimal.ZERO)) {
			try {
				cancelled(loanDTO);
				return false;
			}
			catch (ApiAbacusException | CancelIssuedLoanException | ResourcesNotFoundException
					| IOException e1) {
				logger.info("Failed canceling loan  while mofeed/dakhli processing with id {}",
						loanDTO.getIdAccountExtern());
			}
		}
		else {
			loadDataIbService.updateAcmLoanInIB(loanDTO);
			return true;
		}
		return false;
	}

	/**
	 * Fill request enquiry new customer.
	 *
	 * @param loanDTO the loan DTO
	 * @return the request enquiry new customer simah api DTO
	 */
	RequestEnquiryNewCustomerSimahApiDTO fillRequestEnquiryNewCustomer(LoanDTO loanDTO) {

		RequestEnquiryNewCustomerSimahApiDTO request = new RequestEnquiryNewCustomerSimahApiDTO();

		request.setProductType(23);
		request.setAccept(true);
		request.setAmount(loanDTO.getApprovelAmount());
		request.setResponseType(2);

		List<RequestApplicantSimahApiDTO> applicants = new ArrayList<>();
		RequestApplicantSimahApiDTO applicant = new RequestApplicantSimahApiDTO();
		// fill data informations of customer
		IdentityInfoDTO identityInfoDTO = new IdentityInfoDTO();
		identityInfoDTO.setIdNumber(loanDTO.getCustomerDTO().getIdentity());
		identityInfoDTO.setIdType(1);
		applicant.setIdentityInfo(identityInfoDTO);

		DemographicInfoRequestSimahApiDTO demographicInfoRequestSimahApiDTO =
				new DemographicInfoRequestSimahApiDTO();
		demographicInfoRequestSimahApiDTO.setApplicantType("1");
		demographicInfoRequestSimahApiDTO.setFirstName(loanDTO.getCustomerDTO().getFirstName());
		demographicInfoRequestSimahApiDTO.setSecondName(loanDTO.getCustomerDTO().getSecondName());
		demographicInfoRequestSimahApiDTO.setThirdName(loanDTO.getCustomerDTO().getMiddleName());
		demographicInfoRequestSimahApiDTO.setFamilyName(loanDTO.getCustomerDTO().getLastName());
		demographicInfoRequestSimahApiDTO
				.setGender("M".equals(loanDTO.getCustomerDTO().getGender()) ? 1 : 0);
		demographicInfoRequestSimahApiDTO.setMaritalStatus(2);
		demographicInfoRequestSimahApiDTO.setHijriIDExpiryDate(false);
		Date dateOfBirth = loanDTO.getCustomerDTO().getDateOfBirth();
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
		String formattedDateOfBirth = dateFormat.format(dateOfBirth);
		demographicInfoRequestSimahApiDTO.setDateOfBirth(formattedDateOfBirth);
		demographicInfoRequestSimahApiDTO.setNationality(196);
		applicant.setDemographicInfo(demographicInfoRequestSimahApiDTO);

		// fill data address customer
		RequestAddressSimahApiDTO addressDTO = new RequestAddressSimahApiDTO();
		addressDTO.setAdditionalNumber("7927");
		addressDTO.setAddressType(6);
		addressDTO.setBuildingNumber(3963);
		addressDTO.setCity(396);
		addressDTO.setDistrict("Ash");
		addressDTO.setStreet("Jabal");
		addressDTO.setZipCode(23542);
		addressDTO.setUnitNumber("1");
		applicant.setAddress(addressDTO);

		ContactRequestSimahApiDTO contactDTO = new ContactRequestSimahApiDTO();
		contactDTO.setCountryCode(966);
		contactDTO.setContactType("4");
		contactDTO.setAreaCode("011");
		contactDTO.setExtension("");
		contactDTO.setPhoneNumber(loanDTO.getCustomerDTO().getTelephone1());
		applicant.setContact(contactDTO);

		// fill data occupation
		OccupationRequestSimahApiDTO occupationDTO = new OccupationRequestSimahApiDTO();
		occupationDTO.setOccupation("CD");
		occupationDTO.setSelfEmployment(false);
		occupationDTO.setCertificateRegNo("");
		occupationDTO.setBusinessType(2);
		occupationDTO.setEmployerType(1);
		occupationDTO.setEmployerName("Government");
		// to change
		occupationDTO.setBasicIncome(BigDecimal.valueOf(28500));
		occupationDTO.setTotalMonthlyIncome(BigDecimal.valueOf(28500));

		// fill data address of occupation
		RequestAddressSimahApiDTO addressOccupationDTO = new RequestAddressSimahApiDTO();
		addressOccupationDTO.setAdditionalNumber("123");
		addressOccupationDTO.setAddressType(6);
		addressOccupationDTO.setBuildingNumber(8478);
		addressOccupationDTO.setCity(333);
		addressOccupationDTO.setDistrict("Riyadh");
		addressOccupationDTO.setStreet("Employer Street");
		addressOccupationDTO.setZipCode(23542);
		addressOccupationDTO.setUnitNumber("21");
		occupationDTO.setAddress(addressOccupationDTO);
		applicant.setOccupation(occupationDTO);
		applicants.add(applicant);
		request.setApplicants(applicants);
		return request;
	}

	/**
	 * Gets the score from response simah api.
	 *
	 * @param responseApi the response api
	 * @return the score from response simah api
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	int getScoreFromResponseSimahApi(ResponseEntity<String> responseApi) throws IOException {

		// Parse JSON using Jackson
		ObjectMapper objectMapper = new ObjectMapper();
		JsonNode rootNode = objectMapper.readTree(responseApi.getBody());

		// Navigate to the "data" array
		JsonNode dataNode = rootNode.path("data");
		if (dataNode.isArray()) {
			for (JsonNode dataItem : dataNode) {
				// Navigate to the "score" array for each data item
				JsonNode scoreNode = dataItem.path("score");
				if (scoreNode.isArray() && scoreNode.size() > 0) {
					// Get the first item in the "score" array
					JsonNode firstScoreItem = scoreNode.get(0);

					// Extract the value of the "score" attribute
					int scoreValue = firstScoreItem.path("score").asInt();
					logger.info("Score from Enquiry new customer For SIMAH API : {}", scoreValue);

					// Break out of the loop if you only want the first occurrence
					return scoreValue;
				}
			}

		}
		return 0;
	}

	/**
	 * Check enquiry new customer.
	 *
	 * @param workFlowStepDTO the work flow step DTO
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	private boolean checkEnquiryNewCustomer(WorkFlowStepDTO workFlowStepDTO, LoanDTO loanDTO) {

		RequestEnquiryNewCustomerSimahApiDTO request = fillRequestEnquiryNewCustomer(loanDTO);

		int scoreValue = 0;

		try {
			ResponseEntity<String> responseApi =
					transversClient.enquiryNewCustomer(request, loanDTO.getLoanId());

			if (responseApi.getStatusCodeValue() == 200
					&& !ACMValidationUtils.isNullOrEmpty(responseApi.getBody())) {

				scoreValue = getScoreFromResponseSimahApi(responseApi);

				if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMinScoreRejected())
						&& !ACMValidationUtils
								.isNullOrEmpty(workFlowStepDTO.getMaxScoreRejected())) {

					if (scoreValue >= workFlowStepDTO.getMinScoreRejected()
							&& scoreValue <= workFlowStepDTO.getMaxScoreRejected()) {
						return false;
					}

				}

				else if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMinScoreAccepted())
						&& !ACMValidationUtils
								.isNullOrEmpty(workFlowStepDTO.getMaxScoreAccepted())) {

					if (scoreValue >= workFlowStepDTO.getMinScoreAccepted()
							&& scoreValue <= workFlowStepDTO.getMaxScoreAccepted()) {

						return true;
					}
				}

			}

		}
		catch (Exception ex) {
			logger.info("Exception Enquiry new customer For SIMAH API : {}", ex.getMessage());
			// return false;
		}
		return false;
	}
}
