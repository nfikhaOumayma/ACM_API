/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.acm.client.GedClient;
import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmDocumentsRepository;
import com.acm.service.AcmDocumentsService;
import com.acm.service.CustomerService;
import com.acm.service.LoanDetailsService;
import com.acm.service.LoanService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoansDocumentsDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.pagination.AcmDocumentsPaginationDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.models.AcmDocuments;
import com.acm.utils.models.QAcmDocuments;
import com.acm.utils.models.SettingDocumentType;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmDocumentsServiceImpls} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Service
public class AcmDocumentsServiceImpl implements AcmDocumentsService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmDocumentsServiceImpl.class);

	/** The acmDocuments repository. */
	@Autowired
	private AcmDocumentsRepository acmDocumentsRepository;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The loan details service. */
	@Autowired
	private LoanDetailsService loanDetailsService;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;
	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#find(java.lang.Integer)
	 */
	@Override
	public AcmDocumentsDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find AcmDocuments by ID : {}", id);
		AcmDocuments acmDocuments = acmDocumentsRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmDocuments)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmDocuments.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ AcmDocuments.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(acmDocuments, AcmDocumentsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#find(com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public List<AcmDocumentsDTO> find(AcmDocumentsDTO acmDocumentsDTO) {

		List<AcmDocuments> acmDocumentss = new ArrayList<>();
		if ((!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getTelephone1()))
				&& (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerNumber()))
				&& (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerName()))
				&& (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer()))) {
			acmDocumentss = acmDocumentsRepository
					.findDocumentsCustomerByTelephone(acmDocumentsDTO.getTelephone1());
		}
		else {
			// init Predicate
			BooleanBuilder predicate = buildQuery(acmDocumentsDTO, false);

			// QueryDSL using springDATA
			Iterable<AcmDocuments> iterable = acmDocumentsRepository.findAll(predicate);

			iterable.forEach(acmDocumentss::add);
			logger.info("{} : Documents was founded", acmDocumentss.size());
		}

		// mapping returned list
		List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
		acmDocumentss.forEach(acmDocuments -> acmDocumentsDTOs
				.add(mapper.map(acmDocuments, AcmDocumentsDTO.class)));

		// IF Collection step documents : get the report name
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCollectionInstanceId())) {
			acmDocumentsDTOs.forEach(acmDocDTO -> acmDocDTO.setReportName(
					getReportNameFromSetting(acmDocumentsDTO.getCollectionInstanceId(),
							acmDocDTO.getSettingDocumentTypeDTO().getId())));
		}

		logger.info("Returning founded data ...");
		return acmDocumentsDTOs;
	}

	/**
	 * Gets the report name from setting.
	 *
	 * @param collectionStepId the collection step id
	 * @param documentTypeId the document type id
	 * @return the report name from setting
	 */
	private String getReportNameFromSetting(Long collectionStepId, Long documentTypeId) {

		List<String> listReportName = acmDocumentsRepository
				.getReportNameForCollectionStepDoc(collectionStepId, documentTypeId);
		if (!ACMValidationUtils.isNullOrEmpty(listReportName)) {
			return listReportName.get(0);
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#find(com.acm.utils.dtos.pagination.
	 * AcmDocumentsPaginationDTO)
	 */
	@Override
	public AcmDocumentsPaginationDTO find(AcmDocumentsPaginationDTO acmDocumentsPaginationDTO) {

		Preconditions.checkNotNull(acmDocumentsPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsPaginationDTO.getPageNumber())) {
			acmDocumentsPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsPaginationDTO.getPageSize())) {
			acmDocumentsPaginationDTO.setPageSize(10);
		}
		// setting default data
		acmDocumentsPaginationDTO.setResultsAcmDocuments(new ArrayList<>());
		// setting default totals pages
		acmDocumentsPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		acmDocumentsPaginationDTO.setTotalPages(0);
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(acmDocumentsPaginationDTO.getParams(), true);

		// init pageable params (oage number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("+".equals(acmDocumentsPaginationDTO.getSortDirection())) {
			pageable = PageRequest.of(acmDocumentsPaginationDTO.getPageNumber(),
					acmDocumentsPaginationDTO.getPageSize(), Sort.Direction.ASC, "idDocument");
		}
		else if ("-".equals(acmDocumentsPaginationDTO.getSortDirection())) {
			pageable = PageRequest.of(acmDocumentsPaginationDTO.getPageNumber(),
					acmDocumentsPaginationDTO.getPageSize(), Sort.Direction.DESC, "idDocument");
		}
		else {
			// default sort by applyDate : DESC
			pageable = PageRequest.of(acmDocumentsPaginationDTO.getPageNumber(),
					acmDocumentsPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateCreation");
		}

		// load data
		Page<AcmDocuments> pagedResult = acmDocumentsRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<AcmDocuments> acmDocuments = pagedResult.getContent();
			logger.info("{} : AcmDocuments was founded", acmDocuments.size());
			List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
			acmDocuments.forEach(acmDocument -> {
				AcmDocumentsDTO acmDocumentsDTO = mapper.map(acmDocument, AcmDocumentsDTO.class);
				// find customer by id
				try {
					CustomerDTO customerDTO =
							customerService.findCustomer(acmDocumentsDTO.getIdCustomer());
					acmDocumentsDTO.setCustomerIdentity(customerDTO.getIdentity());
					acmDocumentsDTO.setCustomerNumber(customerDTO.getCustomerNumber());
				}
				catch (ResourcesNotFoundException e) {
					logger.error("Failed caused by {}", e.getMessage());
				}
				acmDocumentsDTOs.add(acmDocumentsDTO);
			});

			// setting data
			acmDocumentsPaginationDTO.setResultsAcmDocuments(acmDocumentsDTOs);
			// setting totals pages
			acmDocumentsPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			acmDocumentsPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return acmDocumentsPaginationDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acm documents DTO
	 * @param fetchOwnedLoans the fetch owend loans
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AcmDocumentsDTO acmDocumentsDTO, boolean fetchOwnedLoans) {

		// init QAcmDocuments
		QAcmDocuments qAcmDocuments = QAcmDocuments.acmDocuments;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by IdCustomer
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer())) {
			predicate.and(qAcmDocuments.idCustomer.eq(acmDocumentsDTO.getIdCustomer()));
		}

		// find by CustomerName
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerName())) {
			predicate.and(qAcmDocuments.customerName.eq(acmDocumentsDTO.getCustomerName()));
		}
		// find by CustomerNumber
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerNumber())) {
			CustomerDTO params = new CustomerDTO();
			params.setCustomerNumber(acmDocumentsDTO.getCustomerNumber());
			params.setSearchFormDocument(Boolean.TRUE);
			List<CustomerDTO> customerDTOs = customerService.find(params);
			if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
				predicate.and(qAcmDocuments.idCustomer.eq(customerDTOs.get(0).getId()));
			}
			else {
				predicate.and(qAcmDocuments.idCustomer.eq(0L));
			}
		}

		// find by CustomerIdentity
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerIdentity())) {
			CustomerDTO params = new CustomerDTO();
			params.setIdentity(acmDocumentsDTO.getCustomerIdentity());
			params.setSearchFormDocument(Boolean.TRUE);
			List<CustomerDTO> customerDTOs = customerService.find(params);
			if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
				predicate.and(qAcmDocuments.idCustomer.eq(customerDTOs.get(0).getId()));
			}
			else {
				predicate.and(qAcmDocuments.idCustomer.eq(0L));
			}
		}

		// find by Loan ID
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getLoanId())) {
			predicate.and(qAcmDocuments.loanId.eq(acmDocumentsDTO.getLoanId()));
		}
		else if (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getLoanId())
				&& Boolean.TRUE.equals(acmDocumentsDTO.getProcessLoanDocuments())) {
			predicate.and(qAcmDocuments.loanId.isNull());
		}
		// Check on all loans owned by connected user if find pagination and loan is is not provided
		// else if (fetchOwnedLoans) {
		// Boolean authorizedAdminGroup = parametrageClient
		// .checkAthorisationConnectedUser(CommonConstants.AUTHORIZED_GROUPS);
		// if (!Boolean.TRUE.equals(authorizedAdminGroup)) {
		// // find connected User Loans
		// List<LoanDTO> connectedUserLoanDTOs = loanService.findByOwners();
		// List<Long> connectedUserLoans = new ArrayList<>();
		// if (ACMValidationUtils.isNullOrEmpty(connectedUserLoanDTOs) && (!ACMValidationUtils
		// .isNullOrEmpty(acmDocumentsDTO.getIdCustomer())
		// || !ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerName())
		// || !ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCustomerNumber())
		// || !ACMValidationUtils
		// .isNullOrEmpty(acmDocumentsDTO.getCustomerIdentity()))) {
		// // connected user dosn't have any LOAN => find where IDLoan=NULL
		// predicate.and(qAcmDocuments.loanId.isNull());
		// }
		// else if (!ACMValidationUtils.isNullOrEmpty(connectedUserLoanDTOs)
		// && (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer())
		// || !ACMValidationUtils
		// .isNullOrEmpty(acmDocumentsDTO.getCustomerName())
		// || !ACMValidationUtils
		// .isNullOrEmpty(acmDocumentsDTO.getCustomerNumber())
		// || !ACMValidationUtils
		// .isNullOrEmpty(acmDocumentsDTO.getCustomerIdentity()))) {
		// // find in autorized loans and IDLoan=NULL
		// connectedUserLoanDTOs
		// .forEach(loanDTO -> connectedUserLoans.add(loanDTO.getLoanId()));
		// predicate.and(qAcmDocuments.loanId.in(connectedUserLoans)
		// .or(qAcmDocuments.loanId.isNull()));
		// }
		// else {
		// // find in authorized loans
		// connectedUserLoanDTOs
		// .forEach(loanDTO -> connectedUserLoans.add(loanDTO.getLoanId()));
		// predicate.and(qAcmDocuments.loanId.in(connectedUserLoans));
		// }
		// }
		// }

		// find by TITRE and old date creation (USED in saveToGed() method)
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getTitre())) {
			predicate.and(qAcmDocuments.titre.eq(acmDocumentsDTO.getTitre()));
		}

		// find by setting type ID
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getSettingDocumentTypeDTO())
				&& !ACMValidationUtils
						.isNullOrEmpty(acmDocumentsDTO.getSettingDocumentTypeDTO().getId())) {
			predicate.and(qAcmDocuments.settingDocumentType.eq(
					new SettingDocumentType(acmDocumentsDTO.getSettingDocumentTypeDTO().getId())));
		}

		// find by setting type CATEGORIE
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getSettingDocumentTypeDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						acmDocumentsDTO.getSettingDocumentTypeDTO().getCategorie())) {
			List<SettingDocumentTypeDTO> settingDocumentTypeDTOs =
					parametrageClient.find(acmDocumentsDTO.getSettingDocumentTypeDTO());
			List<SettingDocumentType> settingDocumentTypes = new ArrayList<>();
			settingDocumentTypeDTOs.forEach(settingDocumentTypeDTO -> settingDocumentTypes
					.add(new SettingDocumentType(settingDocumentTypeDTO.getId())));
			predicate.and(qAcmDocuments.settingDocumentType.in(settingDocumentTypes));
		}

		// find by creation date
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getDateCreation())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(acmDocumentsDTO.getDateCreation()));
			predicate.and(qAcmDocuments.dateInsertion.eq(sqlDate));
		}

		// find by Account Number
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getAccountNumberExtern())) {
			predicate.and(
					qAcmDocuments.accountNumberExtern.eq(acmDocumentsDTO.getAccountNumberExtern()));
		}

		// find by document index
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getDocumentIndex())) {
			predicate.and(qAcmDocuments.documentIndex.eq(acmDocumentsDTO.getDocumentIndex()));
		}

		// find by ID Expenses
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getExpensesId())) {
			predicate.and(qAcmDocuments.expensesId.eq(acmDocumentsDTO.getExpensesId()));
		}

		// find by ID Collection Instance
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCollectionInstanceId())) {
			predicate.and(qAcmDocuments.collectionInstanceId
					.eq(acmDocumentsDTO.getCollectionInstanceId()));
		}

		// find by ID Item Instance
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getItemInstanceId())) {
			predicate.and(qAcmDocuments.itemInstanceId.eq(acmDocumentsDTO.getItemInstanceId()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getItemInstanceIds())) {
			predicate.and(qAcmDocuments.itemInstanceId.in(acmDocumentsDTO.getItemInstanceIds()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCategory())) {
			predicate.and(qAcmDocuments.category.eq(acmDocumentsDTO.getCategory()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getElementId())) {
			predicate.and(qAcmDocuments.elementId.eq(acmDocumentsDTO.getElementId()));
		}

		// find only enabled data
		predicate.and(qAcmDocuments.enabled.eq(Boolean.TRUE));

		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#save(com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public AcmDocumentsDTO save(AcmDocumentsDTO acmDocumentsDTO) {

		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmDocuments acmDocuments = mapper.map(acmDocumentsDTO, AcmDocuments.class);
		CommonFunctions.mapperToSave(acmDocuments, userClient, logger);
		acmDocuments.setDateCreation(new Date());
		acmDocuments.setIdDocument(null);
		AcmDocuments newDocumentsLoan = acmDocumentsRepository.save(acmDocuments);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmDocuments.class.getSimpleName());
		return mapper.map(newDocumentsLoan, AcmDocumentsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#save(java.lang.Integer,
	 * com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public AcmDocumentsDTO save(Long id, AcmDocumentsDTO acmDocumentsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update acmDocuments  with ID = {}", id);
		AcmDocuments oldDocumentsLoan = acmDocumentsRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldDocumentsLoan)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmDocuments.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmDocuments.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldDocumentsLoan)
		mapper.map(acmDocumentsDTO, oldDocumentsLoan);
		CommonFunctions.mapperToUpdate(oldDocumentsLoan, userClient, logger);
		AcmDocuments newDocumentsLoan = acmDocumentsRepository.save(oldDocumentsLoan);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmDocuments.class.getSimpleName());
		return mapper.map(newDocumentsLoan, AcmDocumentsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#saveAll(java.util.List)
	 */
	@Override
	public AcmDocumentsDTO saveAll(List<AcmDocumentsDTO> acmDocumentsDTOs) {

		List<AcmDocuments> oldDocuments = new ArrayList<>();
		Preconditions.checkNotNull(acmDocumentsDTOs, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("saving all acmDocuments...");
		for (AcmDocumentsDTO acmDocumentDTO : acmDocumentsDTOs) {

			AcmDocuments oldDocumentsLoan =
					acmDocumentsRepository.findById(acmDocumentDTO.getLoanId()).orElse(null);
			if (!ACMValidationUtils.isNullOrEmpty(oldDocumentsLoan)) {
				// mapping new data with existing data (oldDocumentsLoan)
				mapper.map(acmDocumentDTO, oldDocumentsLoan);
				CommonFunctions.mapperToUpdate(oldDocumentsLoan, userClient, logger);
				oldDocuments.add(oldDocumentsLoan);
			}
		}

		List<AcmDocuments> newDocumentsLoan = acmDocumentsRepository.saveAll(oldDocuments);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmDocuments.class.getSimpleName());
		return mapper.map(newDocumentsLoan, AcmDocumentsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#delete(com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public void delete(AcmDocumentsDTO acmDocumentsDTO) {

		Preconditions.checkNotNull(acmDocumentsDTO.getIdDocument(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.warn("delete acmDocuments  with ID = {}", acmDocumentsDTO.getIdDocument());
		// delete object by id
		acmDocumentsRepository.deleteById(acmDocumentsDTO.getIdDocument());
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, AcmDocuments.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#saveList(java.util.List)
	 */
	@Override
	public AcmDocumentsDTO save(MultipartFile[] uploadedFiles, AcmDocumentsDTO acmDocumentsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(uploadedFiles, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// find list of duplicated document by titre if exist idLoan not null
		List<AcmDocumentsDTO> checkAcmDocumentsDTOs = findDocumentProcessLoan(acmDocumentsDTO);
		List<AcmDocumentsDTO> disbledAcmDocumentsDTOs = new ArrayList<>();
		for (AcmDocumentsDTO checkAcmDocumentsDTO : checkAcmDocumentsDTOs) {
			disbledAcmDocumentsDTOs.add(checkAcmDocumentsDTO);
		}

		// save data document loan in db
		AcmDocumentsDTO newAcmDocumentsDTO = save(acmDocumentsDTO);

		// save in GED
		AcmDocumentsDTO gedAcmDocumentsDTO = sendToGed(uploadedFiles, newAcmDocumentsDTO);
		logger.info("send to GED :: DONE");

		// save data document customer in db
		if (gedAcmDocumentsDTO != null && acmDocumentsDTO.getLoanId() != null
				&& acmDocumentsDTO.getSettingDocumentTypeDTO().getCategorie() == CommonFunctions
						.mappingStatus(CommonConstants.ACM_SETTING_DOCS_TYPE_CLIENT).getKey()) {
			acmDocumentsDTO.setLoanId(null);
			acmDocumentsDTO.setAccountNumberExtern(null);
			acmDocumentsDTO.setIdDocumentGED(gedAcmDocumentsDTO.getIdDocumentGED());
			// find list of duplicated document by titre if exist id loan null
			List<AcmDocumentsDTO> checkDocumentCustomers = findDocumentProcessLoan(acmDocumentsDTO);
			for (AcmDocumentsDTO checkAcmDocumentsDTO : checkDocumentCustomers) {
				disbledAcmDocumentsDTOs.add(checkAcmDocumentsDTO);
			}
			AcmDocumentsDTO newCustomerAcmDocumentsDTO = save(acmDocumentsDTO);
			logger.info("new customer document was inserted with id : {}",
					newCustomerAcmDocumentsDTO.getIdDocument());
		}

		// disable duplicate document for given loan
		for (AcmDocumentsDTO checkAcmDocumentsDTO : disbledAcmDocumentsDTOs) {
			checkAcmDocumentsDTO.setEnabled(Boolean.FALSE);
			save(checkAcmDocumentsDTO.getIdDocument(), checkAcmDocumentsDTO);
		}
		return newAcmDocumentsDTO;
	}

	/**
	 * function used to find document to disable when idLoan is null or not.
	 *
	 * @author AbdelkarimTurki
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the list
	 */
	private List<AcmDocumentsDTO> findDocumentProcessLoan(AcmDocumentsDTO acmDocumentsDTO) {

		// init QAcmDocuments
		QAcmDocuments qAcmDocuments = QAcmDocuments.acmDocuments;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmDocuments.enabled.eq(Boolean.TRUE));

		// find by IdCustomer
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer())) {
			predicate.and(qAcmDocuments.idCustomer.eq(acmDocumentsDTO.getIdCustomer()));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getCollectionInstanceId())) {
			predicate.and(qAcmDocuments.collectionInstanceId
					.eq(acmDocumentsDTO.getCollectionInstanceId()));
		}
		else {
			return new ArrayList<>();
		}

		// find ByItem instance
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getItemInstanceId())) {
			predicate.and(qAcmDocuments.itemInstanceId.eq(acmDocumentsDTO.getItemInstanceId()));
		}

		// find by Loan ID
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getLoanId())) {
			predicate.and(qAcmDocuments.loanId.eq(acmDocumentsDTO.getLoanId()));
		}
		else {
			predicate.and(qAcmDocuments.loanId.isNull());
		}

		// find by TITRE and old date creation (USED in saveToGed() method)
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getTitre())) {
			predicate.and(qAcmDocuments.titre.eq(acmDocumentsDTO.getTitre()));
		}

		// find by setting type ID
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getSettingDocumentTypeDTO())
				&& !ACMValidationUtils
						.isNullOrEmpty(acmDocumentsDTO.getSettingDocumentTypeDTO().getId())) {
			predicate.and(qAcmDocuments.settingDocumentType.eq(
					new SettingDocumentType(acmDocumentsDTO.getSettingDocumentTypeDTO().getId())));
		}

		// find by setting type CATEGORIE
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getSettingDocumentTypeDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						acmDocumentsDTO.getSettingDocumentTypeDTO().getCategorie())) {
			List<SettingDocumentTypeDTO> settingDocumentTypeDTOs =
					parametrageClient.find(acmDocumentsDTO.getSettingDocumentTypeDTO());
			List<SettingDocumentType> settingDocumentTypes = new ArrayList<>();
			settingDocumentTypeDTOs.forEach(settingDocumentTypeDTO -> settingDocumentTypes
					.add(new SettingDocumentType(settingDocumentTypeDTO.getId())));
			predicate.and(qAcmDocuments.settingDocumentType.in(settingDocumentTypes));
		}

		// find by creation date
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getDateCreation())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(acmDocumentsDTO.getDateCreation()));
			predicate.and(qAcmDocuments.dateInsertion.eq(sqlDate));
		}

		// find by Account Number
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getAccountNumberExtern())) {
			predicate.and(
					qAcmDocuments.accountNumberExtern.eq(acmDocumentsDTO.getAccountNumberExtern()));
		}

		// find by document index
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getDocumentIndex())) {
			predicate.and(qAcmDocuments.documentIndex.eq(acmDocumentsDTO.getDocumentIndex()));
		}

		// QueryDSL using springDATA
		Iterable<AcmDocuments> iterable = acmDocumentsRepository.findAll(predicate);
		List<AcmDocuments> acmDocumentss = new ArrayList<>();
		iterable.forEach(acmDocumentss::add);
		logger.info("{} : Existing Documents was founded", acmDocumentss.size());

		// mapping returned list
		List<AcmDocumentsDTO> checkAcmDocumentsDTOs = new ArrayList<>();
		acmDocumentss.forEach(acmDocuments -> checkAcmDocumentsDTOs
				.add(mapper.map(acmDocuments, AcmDocumentsDTO.class)));
		return checkAcmDocumentsDTOs;
	}

	/**
	 * Send to ged.
	 *
	 * @author HaythemBenizid
	 * @param uploadedFiles the uploaded files
	 * @param acmDocumentsDTO the loan documents DTO
	 * @return the acm documents DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private AcmDocumentsDTO sendToGed(MultipartFile[] uploadedFiles,
			AcmDocumentsDTO acmDocumentsDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(acmDocumentsDTO.getIdDocument(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		if (uploadedFiles != null && uploadedFiles.length > 0) {
			// init data to be send to GED
			GedParameterDTO gedParameterDTO = new GedParameterDTO();
			gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
			gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
			// setting tags
			List<String> tags = new ArrayList<>();
			tags.add(acmDocumentsDTO.getTitre());
			if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getLoanId())) {
				// find data of loan
				LoanDTO loanDTO = loanService.find(acmDocumentsDTO.getLoanId());
				tags.add(loanDTO.getAccountNumber());
				tags.add(loanDTO.getProductCode());
				// set loan number
				gedParameterDTO.setLoanNumber(loanDTO.getAccountNumber());
				// set customer number
				gedParameterDTO.setCustomerNumber(loanDTO.getCustomerDTO().getCustomerNumber());
				// setting data for backup if there is an exception
				gedParameterDTO.setIdCustomer(loanDTO.getCustomerDTO().getId());
				gedParameterDTO.setIdDocument(acmDocumentsDTO.getIdDocument());
				gedParameterDTO.setLoanId(loanDTO.getLoanId());
			}
			else if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer())) {
				// find customer data
				CustomerDTO customerDTO =
						customerService.findCustomer(acmDocumentsDTO.getIdCustomer());
				tags.add(customerDTO.getCustomerNumber());
				// set customer number
				gedParameterDTO.setCustomerNumber(customerDTO.getCustomerNumber());
				// setting data for backup if there is an exception
				gedParameterDTO.setIdCustomer(acmDocumentsDTO.getIdCustomer());
				gedParameterDTO.setIdDocument(acmDocumentsDTO.getIdDocument());
			}
			gedParameterDTO.setTags(tags);
			// convert MultipartFile to File
			List<File> filesToSend = new ArrayList<>();
			for (MultipartFile multipartFile : uploadedFiles) {
				File file = CommonFunctions.fileConverter(multipartFile,
						environment.getProperty("spring.servlet.multipart.location"));
				filesToSend.add(file);
			}
			gedParameterDTO.setFilesToUpload(filesToSend);
			try {
				// send to GED
				List<String> ids = gedClient.uploadToGed(gedParameterDTO);
				String idDocumentGED =
						ACMValidationUtils.isNullOrEmpty(ids) ? null : ids.get(0).split(";")[0];
				logger.info("idDocumentGed inserted : {} ", idDocumentGED);
				// update data : id DocumentGED if is not NULL
				if (!ACMValidationUtils.isNullOrEmpty(idDocumentGED)) {
					acmDocumentsDTO.setIdDocumentGED(idDocumentGED);
					return save(acmDocumentsDTO.getIdDocument(), acmDocumentsDTO);
				}
			}
			catch (Exception e) {
				logger.error("failed to save in GED");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
				delete(acmDocumentsDTO);
				return null;
			}
		}
		return acmDocumentsDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#saveImageToGed(org.springframework.web.multipart.
	 * MultipartFile[], com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public AcmDocumentsDTO save(MultipartFile[] uploadedFiles, LoanDTO loanDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanDTO.getCustomerDTO(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanDTO.getCustomerDTO().getCustomerNumber(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		if (uploadedFiles != null && uploadedFiles.length > 0) {
			// init data to be send to GED
			GedParameterDTO gedParameterDTO = new GedParameterDTO();
			gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
			gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
			// setting tags
			List<String> tags = new ArrayList<>();
			String photoTag = CommonConstantGED.SETTING_DOC_TYPE_CODE_PHOTO
					+ CommonConstantGED.TIR_6 + CommonConstantGED.CUSTOMER + CommonConstantGED.TIR_6
					+ loanDTO.getCustomerDTO().getCustomerNumber();
			tags.add(photoTag);
			gedParameterDTO.setTags(tags);
			// convert MultipartFile to File
			List<File> filesToSend = new ArrayList<>();
			for (MultipartFile multipartFile : uploadedFiles) {
				File file = CommonFunctions.fileConverter(multipartFile,
						environment.getProperty("spring.servlet.multipart.location"));
				filesToSend.add(file);
			}
			gedParameterDTO.setFilesToUpload(filesToSend);
			// if exist in ged
			List<GedDocumentDTO> existingDocumentInGed = gedClient.findDemandeDocuments(photoTag);
			if (!ACMValidationUtils.isNullOrEmpty(existingDocumentInGed)) {
				logger.error(CommonLoggerMessage.FILE_EXIST_ALREADY, File.class.getSimpleName());
				for (GedDocumentDTO gedDocumentDTO : existingDocumentInGed) {
					gedClient.delete(gedDocumentDTO.getId());
				}
			}
			// send to GED
			try {
				List<String> ids = gedClient.uploadToGed(gedParameterDTO);
				String idDocumentGED =
						ACMValidationUtils.isNullOrEmpty(ids) ? null : ids.get(0).split(";")[0];
				logger.info("Photo customer inserted in GED with ID : {} ", idDocumentGED);
			}
			catch (Exception e) {
				logger.error("failed to save Photo in GED");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#checkRequiredDocument(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public Boolean checkRequiredDocument(LoanDTO loanDTO) {

		// no need to check Doc status for GRP loan
		if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
			return Boolean.TRUE;
		}

		// load list document from setting table by loan
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOs =
				loanDetailsService.findRequiredDocument(loanDTO);

		// filter list of required documents
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOsRequired = settingDocumentTypeDTOs
				.stream()
				.filter(settingDocumentTypeDTO -> Boolean.TRUE.equals(settingDocumentTypeDTO
						.getMandatory()
						&& settingDocumentTypeDTO.getCategorie() != CommonFunctions
								.mappingStatus(
										CommonConstants.ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE)
								.getKey()
						&& settingDocumentTypeDTO.getCategorie() != CommonFunctions
								.mappingStatus(CommonConstants.ACM_SETTING_DOCS_COLLECTION)
								.getKey()))
				.collect(Collectors.toList());

		// load list of document by loan
		AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
		acmDocumentsDTO.setLoanId(loanDTO.getLoanId());
		List<AcmDocumentsDTO> acmDocumentsDTOs = find(acmDocumentsDTO);

		// filter inserted required document for given loan
		List<AcmDocumentsDTO> acmDocumentsDTOsInserted = acmDocumentsDTOs.stream()
				.filter(acmDocuments -> settingDocumentTypeDTOsRequired.stream()
						.anyMatch(settingDocumentTypeDTORequired -> acmDocuments
								.getSettingDocumentTypeDTO().getCode()
								.equals(settingDocumentTypeDTORequired.getCode())))
				.collect(Collectors.toList());

		// check list documents inserted in ACM --> if exist in GED by ID_DOCUMENT_GED
		Boolean documentsInsertedExistInGED = Boolean.TRUE;
		for (AcmDocumentsDTO documents : acmDocumentsDTOsInserted) {
			if (Boolean.FALSE
					.equals(gedClient.isExist(new AcmDocumentsDTO(documents.getIdDocument(),
							documents.getIdDocumentGED())))) {
				documentsInsertedExistInGED = Boolean.FALSE;
			}
		}
		List<String> settingDocumentTypeInsertedCode = acmDocumentsDTOsInserted.stream()
				.map(acmDocumentDTO -> acmDocumentDTO.getSettingDocumentTypeDTO().getCode())
				.collect(Collectors.toList());

		Boolean compareDocumentRequired = Boolean.TRUE;
		for (SettingDocumentTypeDTO settingDocumentTypeDTORequired : settingDocumentTypeDTOsRequired) {
			if (Boolean.FALSE.equals(settingDocumentTypeInsertedCode
					.contains(settingDocumentTypeDTORequired.getCode()))) {
				compareDocumentRequired = Boolean.FALSE;
			}
		}

		return (Boolean.TRUE.equals(compareDocumentRequired)
				&& Boolean.TRUE.equals(documentsInsertedExistInGED));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#disableDocument(com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public void disableDocument(AcmDocumentsDTO acmDocumentsDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmDocumentsDTO.getIdDocument(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.warn("disable acmDocuments  with ID = {}", acmDocumentsDTO.getIdDocument());
		// delete object by id
		AcmDocumentsDTO newAcmDocumentsDTO = find(acmDocumentsDTO.getIdDocument());
		newAcmDocumentsDTO.setEnabled(Boolean.FALSE);
		save(newAcmDocumentsDTO.getIdDocument(), newAcmDocumentsDTO);

		logger.info("disable Document  with ID = {} :: DONE", acmDocumentsDTO.getIdDocument());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#checkRequiredDocument(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public Boolean checkRequiredDocumentSigned(LoanDTO loanDTO) {

		// load list document from setting table by loan
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOs =
				loanDetailsService.findRequiredDocument(loanDTO);

		// filter list of required documents
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOsRequired = settingDocumentTypeDTOs
				.stream()
				.filter(settingDocumentTypeDTO -> Boolean.TRUE.equals(settingDocumentTypeDTO
						.getMandatory()
						&& settingDocumentTypeDTO.getCategorie() == CommonFunctions
								.mappingStatus(
										CommonConstants.ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE)
								.getKey()))
				.collect(Collectors.toList());

		// load list of document by loan
		AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
		acmDocumentsDTO.setLoanId(loanDTO.getLoanId());
		List<AcmDocumentsDTO> acmDocumentsDTOs = find(acmDocumentsDTO);

		// filter inserted required document for given loan
		List<AcmDocumentsDTO> acmDocumentsDTOsInserted = acmDocumentsDTOs.stream()
				.filter(acmDocuments -> settingDocumentTypeDTOsRequired.stream()
						.anyMatch(settingDocumentTypeDTORequired -> acmDocuments
								.getSettingDocumentTypeDTO().getCode()
								.equals(settingDocumentTypeDTORequired.getCode())))
				.collect(Collectors.toList());

		// check list documents inserted in ACM --> if exist in GED by ID_DOCUMENT_GED
		Boolean documentsInsertedExistInGED = Boolean.TRUE;
		for (AcmDocumentsDTO documents : acmDocumentsDTOsInserted) {
			if (Boolean.FALSE
					.equals(gedClient.isExist(new AcmDocumentsDTO(documents.getIdDocument(),
							documents.getIdDocumentGED())))) {
				documentsInsertedExistInGED = Boolean.FALSE;
			}
		}
		List<String> settingDocumentTypeInsertedCode = acmDocumentsDTOsInserted.stream()
				.map(acmDocumentDTO -> acmDocumentDTO.getSettingDocumentTypeDTO().getCode())
				.collect(Collectors.toList());

		Boolean compareDocumentRequired = Boolean.TRUE;
		for (SettingDocumentTypeDTO settingDocumentTypeDTORequired : settingDocumentTypeDTOsRequired) {
			if (Boolean.FALSE.equals(settingDocumentTypeInsertedCode
					.contains(settingDocumentTypeDTORequired.getCode()))) {
				compareDocumentRequired = Boolean.FALSE;
			}
		}

		return (Boolean.TRUE.equals(compareDocumentRequired)
				&& Boolean.TRUE.equals(documentsInsertedExistInGED));
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findLoansDocumentsByCustomer(com.acm.utils.dtos.
	 * AcmDocumentsDTO)
	 */
	@Override
	public List<LoansDocumentsDTO> findLoansDocumentsByCustomer(AcmDocumentsDTO acmDocumentsDTO) {

		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check customer id
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getIdCustomer())) {
			return new ArrayList<>();
		}
		List<LoansDocumentsDTO> loansDocumentsDTOs = new ArrayList<>();
		// List loans by Customer ID
		List<LoanDTO> loanDTOs = loanService.findByIdCustomer(acmDocumentsDTO.getIdCustomer());
		// init object
		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		SettingDocumentTypeDTO settingDocumentTypeDTO2 = new SettingDocumentTypeDTO();
		// find SettingDocumentTypeDTO by givin categorie
		settingDocumentTypeDTO.setCategorie(0);
		settingDocumentTypeDTO2.setCategorie(2);
		List<SettingDocumentTypeDTO> settingDocumentTypeDTOs =
				parametrageClient.find(settingDocumentTypeDTO);
		settingDocumentTypeDTOs.addAll(0, parametrageClient.find(settingDocumentTypeDTO2));

		// mapping returned list
		List<SettingDocumentType> settingDocumentTypes = new ArrayList<>();
		settingDocumentTypeDTOs.forEach(settingDocumentType -> settingDocumentTypes
				.add(mapper.map(settingDocumentType, SettingDocumentType.class)));
		// List documents by loans ID
		for (LoanDTO loanDTO : loanDTOs) {
			// find document by given ID loan
			List<AcmDocuments> acmDocumentss =
					acmDocumentsRepository.findByLoanIdAndEnabledAndSettingDocumentTypeIn(
							loanDTO.getLoanId(), Boolean.TRUE, settingDocumentTypes);
			logger.info("{} : Documents was founded by ID loan = {}", acmDocumentss.size(),
					loanDTO.getLoanId());

			// mapping returned list
			List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
			acmDocumentss.forEach(acmDocuments -> acmDocumentsDTOs
					.add(mapper.map(acmDocuments, AcmDocumentsDTO.class)));
			// init object
			loansDocumentsDTOs.add(new LoansDocumentsDTO(loanDTO.getLoanId(),
					loanDTO.getAccountNumber(), acmDocumentsDTOs));
		}

		// Returning data
		return loansDocumentsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmDocumentsService#saveToGed(org.springframework.web.multipart.MultipartFile
	 * [], java.lang.String)
	 */
	@Override
	public List<AcmDocumentsDTO> saveToGed(MultipartFile[] uploadedFiles, String documentsLoanDTOs)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(documentsLoanDTOs, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(uploadedFiles, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			// convert JSON String to list of object
			ObjectMapper objectMapper = new ObjectMapper();
			List<AcmDocumentsDTO> acmDocumentsDTOs =
					objectMapper.readValue(documentsLoanDTOs, objectMapper.getTypeFactory()
							.constructCollectionType(List.class, AcmDocumentsDTO.class));
			// INIT list
			List<AcmDocumentsDTO> documentsWithmultipartFile = new ArrayList<>();
			List<Long> listIdSettingDocumentTypeDTO = new ArrayList<>();
			// attach multipartFile wit his document data
			for (MultipartFile multipartFile : uploadedFiles) {
				logger.info("filename: {}", multipartFile.getOriginalFilename());
				logger.info("size: {}", multipartFile.getSize());
				// find document/multipartFile with same size
				AcmDocumentsDTO doc = acmDocumentsDTOs.stream()
						.filter(s -> s.getDocumentSize() == multipartFile.getSize()
								&& !(listIdSettingDocumentTypeDTO
										.contains(s.getSettingDocumentTypeDTO().getId())))
						.findFirst().orElse(null);
				logger.info("{}", doc);
				if (doc != null) {
					// store ID SettingDocumentType
					listIdSettingDocumentTypeDTO.add(doc.getSettingDocumentTypeDTO().getId());
					doc.setMultipartFile(multipartFile);
					documentsWithmultipartFile.add(doc);
				}
			}
			// processing data into ACM
			List<AcmDocumentsDTO> newDocumentsWithmultipartFile = new ArrayList<>();
			List<AcmDocumentsDTO> disbledAcmDocumentsDTOs = new ArrayList<>();
			List<GedParameterDTO> gedParameterDTOs = new ArrayList<>();
			for (AcmDocumentsDTO acmDocumentsDTO : documentsWithmultipartFile) {
				// find list of duplicated document by titre if exist idLoan not null
				acmDocumentsDTO.setDateCreation(null);
				List<AcmDocumentsDTO> checkAcmDocumentsDTOs =
						findDocumentProcessLoan(acmDocumentsDTO);
				for (AcmDocumentsDTO checkAcmDocumentsDTO : checkAcmDocumentsDTOs) {
					disbledAcmDocumentsDTOs.add(checkAcmDocumentsDTO);
				}
				// save data document loan in db
				AcmDocumentsDTO newAcmDocumentsDTO = save(acmDocumentsDTO);
				// init data to be send to GED
				GedParameterDTO gedParameterDTO = new GedParameterDTO();
				gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
				gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
				gedParameterDTO.setAcmDocumentsDTO(newAcmDocumentsDTO);
				// setting tags
				List<String> tags = new ArrayList<>();
				tags.add(newAcmDocumentsDTO.getTitre());
				if (!ACMValidationUtils.isNullOrEmpty(newAcmDocumentsDTO.getLoanId())) {
					// find data of loan
					LoanDTO loanDTO = loanService.find(newAcmDocumentsDTO.getLoanId());
					tags.add(loanDTO.getAccountNumber());
					tags.add(loanDTO.getProductCode());
					// set loan number
					gedParameterDTO.setLoanNumber(loanDTO.getAccountNumber());
					// set customer number
					gedParameterDTO.setCustomerNumber(loanDTO.getCustomerDTO().getCustomerNumber());
					// setting data for backup if there is an exception
					gedParameterDTO.setIdCustomer(loanDTO.getCustomerDTO().getId());
					gedParameterDTO.setIdDocument(newAcmDocumentsDTO.getIdDocument());
					gedParameterDTO.setLoanId(loanDTO.getLoanId());
				}
				else if (!ACMValidationUtils.isNullOrEmpty(newAcmDocumentsDTO.getIdCustomer())) {
					// find customer data
					CustomerDTO customerDTO =
							customerService.findCustomer(newAcmDocumentsDTO.getIdCustomer());
					tags.add(customerDTO.getCustomerNumber());
					// set customer number
					gedParameterDTO.setCustomerNumber(customerDTO.getCustomerNumber());
					// setting data for backup if there is an exception
					gedParameterDTO.setIdCustomer(newAcmDocumentsDTO.getIdCustomer());
					gedParameterDTO.setIdDocument(newAcmDocumentsDTO.getIdDocument());
				}
				gedParameterDTO.setTags(tags);
				// convert MultipartFile to File
				File file = CommonFunctions.fileConverter(acmDocumentsDTO.getMultipartFile(),
						environment.getProperty("spring.servlet.multipart.location"));
				List<File> filesToSend = new ArrayList<>();
				filesToSend.add(file);
				gedParameterDTO.setFilesToUpload(filesToSend);

				gedParameterDTOs.add(gedParameterDTO);
			}
			// send All document to GED
			try {
				// send to GED
				List<GedParameterDTO> ids = gedClient.uploadListToGed(gedParameterDTOs);
				for (GedParameterDTO gedParameterDTO : ids) {
					String idDocumentGED =
							ACMValidationUtils.isNullOrEmpty(gedParameterDTO.getIdDocumentGed())
									? null
									: gedParameterDTO.getIdDocumentGed().split(";")[0];
					logger.info("idDocumentGed inserted : {} ", idDocumentGED);

					// update data : id DocumentGED if is not NULL
					AcmDocumentsDTO acmDocumentsDTO = gedParameterDTO.getAcmDocumentsDTO();
					if (!ACMValidationUtils.isNullOrEmpty(idDocumentGED)) {
						acmDocumentsDTO.setIdDocumentGED(idDocumentGED);
						newDocumentsWithmultipartFile
								.add(save(acmDocumentsDTO.getIdDocument(), acmDocumentsDTO));
					}
					else { // if GED id down
						newDocumentsWithmultipartFile.add(acmDocumentsDTO);
					}
					// save data document customer in DB
					if (acmDocumentsDTO.getLoanId() != null && acmDocumentsDTO
							.getSettingDocumentTypeDTO().getCategorie() == CommonFunctions
									.mappingStatus(CommonConstants.ACM_SETTING_DOCS_TYPE_CLIENT)
									.getKey()) {
						acmDocumentsDTO.setLoanId(null);
						acmDocumentsDTO.setAccountNumberExtern(null);
						acmDocumentsDTO.setIdDocumentGED(idDocumentGED);
						// find list of duplicated document by titre if exist id loan null
						acmDocumentsDTO.setDateCreation(null);
						List<AcmDocumentsDTO> checkDocumentCustomers =
								findDocumentProcessLoan(acmDocumentsDTO);
						for (AcmDocumentsDTO checkAcmDocumentsDTO : checkDocumentCustomers) {
							disbledAcmDocumentsDTOs.add(checkAcmDocumentsDTO);
						}
						AcmDocumentsDTO newCustomerAcmDocumentsDTO = save(acmDocumentsDTO);
						logger.info("new customer document was inserted with id : {}",
								newCustomerAcmDocumentsDTO.getIdDocument());
					}
				}
			}
			catch (Exception e) {
				logger.error("failed to save in GED");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
				// delete inserted Document in ACM
				for (GedParameterDTO gedParameterDTO : gedParameterDTOs) {
					delete(new AcmDocumentsDTO(gedParameterDTO.getIdDocument()));
				}
			}

			// disable duplicate document for given loan
			for (AcmDocumentsDTO checkAcmDocumentsDTO : disbledAcmDocumentsDTOs) {
				if (checkAcmDocumentsDTO.getCollectionInstanceId() != null && ACMValidationUtils
						.isNullOrEmpty(checkAcmDocumentsDTO.getIdDocumentGED())) {
					delete(checkAcmDocumentsDTO);
				}
				else {
					checkAcmDocumentsDTO.setEnabled(Boolean.FALSE);
					save(checkAcmDocumentsDTO.getIdDocument(), checkAcmDocumentsDTO);
				}
			}

			// retrun data
			return newDocumentsWithmultipartFile;
		}
		catch (IOException e) {
			logger.error("Error while pasing and saving file in ACM DB Or in GED");
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmDocumentsService#findExpensesDocument(com.acm.utils.dtos.AcmDocumentsDTO)
	 */
	@Override
	public List<AcmDocumentsDTO> findExpensesDocument(AcmDocumentsDTO acmDocumentsDTO) {

		Preconditions.checkNotNull(acmDocumentsDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getExpensesId())) {
			return new ArrayList<>();
		}
		// init QAcmDocuments
		QAcmDocuments qAcmDocuments = QAcmDocuments.acmDocuments;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmDocuments.enabled.eq(Boolean.TRUE));

		// find by ID Expenses
		if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTO.getExpensesId())) {
			predicate.and(qAcmDocuments.expensesId.eq(acmDocumentsDTO.getExpensesId()));
		}

		// QueryDSL using springDATA
		Iterable<AcmDocuments> iterable = acmDocumentsRepository.findAll(predicate);
		List<AcmDocuments> acmDocumentss = new ArrayList<>();
		iterable.forEach(acmDocumentss::add);
		logger.info("{} : Documents was founded", acmDocumentss.size());

		// mapping returned list
		List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
		acmDocumentss.forEach(acmDocuments -> acmDocumentsDTOs
				.add(mapper.map(acmDocuments, AcmDocumentsDTO.class)));

		logger.info("Returning founded data ...");
		return acmDocumentsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#updateListDocument(java.util.List)
	 */
	@Override
	public List<AcmDocumentsDTO> updateListDocument(List<AcmDocumentsDTO> acmDocumentsDTOs)
			throws ResourcesNotFoundException {

		logger.info("Start update list of document ");
		List<AcmDocumentsDTO> acmDocumentsDTOsResult = new ArrayList<>();
		if (ACMValidationUtils.isNullOrEmpty(acmDocumentsDTOs)) {
			return acmDocumentsDTOsResult;
		}
		for (AcmDocumentsDTO acmDocumentsDTO : acmDocumentsDTOs) {
			acmDocumentsDTOsResult.add(save(acmDocumentsDTO.getIdDocument(), acmDocumentsDTO));
		}
		logger.info("End update list of document ");

		return acmDocumentsDTOsResult;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findHistoryDocument(long, long)
	 */
	@Override
	public List<AcmDocumentsDTO> findHistoryDocument(long typeDocId, long loanId,
			Integer docIndex) {

		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setId(typeDocId);
		List<AcmDocumentsDTO> acmDocumentsListDTOs = acmDocumentsRepository
				.findBySettingDocumentTypeAndLoanIdAndDocumentIndex(settingDocumentType, loanId,
						docIndex)
				.stream().map(item -> mapper.map(item, AcmDocumentsDTO.class))
				.collect(Collectors.toList());

		return acmDocumentsListDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findHistoryDocumentByCustomerId(long, long)
	 */
	@Override
	public List<AcmDocumentsDTO> findHistoryDocumentByCustomerId(long typeDocId, long custumerId,
			Integer docIndex) {

		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setId(typeDocId);
		List<AcmDocumentsDTO> acmDocumentsListDTOs = acmDocumentsRepository
				.findBySettingDocumentTypeAndIdCustomerAndDocumentIndexAndLoanIdIsNull(
						settingDocumentType, custumerId, docIndex)
				.stream().map(item -> mapper.map(item, AcmDocumentsDTO.class))
				.collect(Collectors.toList());

		return acmDocumentsListDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findhistoryDocumentByCollectionStep(long, long)
	 */
	@Override
	public List<AcmDocumentsDTO> findhistoryDocumentByCollectionStep(long typeDocId,
			long collectionInstanceId, Integer docIndex) {

		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setId(typeDocId);
		return acmDocumentsRepository
				.findBySettingDocumentTypeAndCollectionInstanceIdAndDocumentIndex(
						settingDocumentType, collectionInstanceId, docIndex)
				.stream().map(item -> mapper.map(item, AcmDocumentsDTO.class))
				.collect(Collectors.toList());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findInIdIbDocuments(java.util.List)
	 */
	@Override
	public List<AcmDocumentsDTO> findInIdIbDocuments(List<Long> idIbDocuments) {

		if (ACMValidationUtils.isNullOrEmpty(idIbDocuments)) {
			return new ArrayList<>();
		}
		// init acmDocumentsResult
		List<AcmDocuments> acmDocumentsResult = new ArrayList<>();

		// init QAcmDocuments
		QAcmDocuments qAcmDocuments = QAcmDocuments.acmDocuments;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find in idIbDocument
		predicate.and(qAcmDocuments.idIbDocument.in(idIbDocuments));

		Iterable<AcmDocuments> iterable = acmDocumentsRepository.findAll(predicate);

		iterable.forEach(acmDocumentsResult::add);
		// mapping returned list
		List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
		acmDocumentsResult.forEach(acmDocuments -> acmDocumentsDTOs
				.add(mapper.map(acmDocuments, AcmDocumentsDTO.class)));
		return acmDocumentsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findInSettingDocumentTypeIds(com.acm.utils.dtos.
	 * AcmDocumentsDTO, java.util.List)
	 */
	@Override
	public List<AcmDocumentsDTO> findInSettingDocumentTypeIds(AcmDocumentsDTO documentsDTO,
			List<Long> settingDocumentTypeIds) {

		if (ACMValidationUtils.isNullOrEmpty(settingDocumentTypeIds)) {
			return new ArrayList<>();
		}
		// init acmDocumentsResult
		List<AcmDocuments> acmDocumentsResult = new ArrayList<>();

		// init QAcmDocuments
		QAcmDocuments qAcmDocuments = QAcmDocuments.acmDocuments;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find in idIbDocument
		predicate.and(qAcmDocuments.settingDocumentType.id.in(settingDocumentTypeIds));
		if (!ACMValidationUtils.isNullOrEmpty(documentsDTO.getLoanId())) {
			predicate.and(qAcmDocuments.loanId.eq(documentsDTO.getLoanId()));
		}
		Iterable<AcmDocuments> iterable = acmDocumentsRepository.findAll(predicate);

		iterable.forEach(acmDocumentsResult::add);
		// mapping returned list
		List<AcmDocumentsDTO> acmDocumentsDTOs = new ArrayList<>();
		acmDocumentsResult.forEach(acmDocuments -> acmDocumentsDTOs
				.add(mapper.map(acmDocuments, AcmDocumentsDTO.class)));
		return acmDocumentsDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findhistoryDocumentByItemInstanceStep(long, long)
	 */
	@Override
	public List<AcmDocumentsDTO> findhistoryDocumentByItemInstanceStep(long typeDocId,
			long itemInstanceStepId) {

		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setId(typeDocId);
		return acmDocumentsRepository
				.findBySettingDocumentTypeAndItemInstanceId(settingDocumentType, itemInstanceStepId)
				.stream().map(item -> mapper.map(item, AcmDocumentsDTO.class))
				.collect(Collectors.toList());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDocumentsService#findhistoryDocumentByElementCategory(long, long,
	 * java.lang.String)
	 */
	@Override
	public List<AcmDocumentsDTO> findhistoryDocumentByElementCategory(long typeDocId,
			long elementId, String category) {

		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setId(typeDocId);
		return acmDocumentsRepository
				.findBySettingDocumentTypeAndElementIdAndCategory(settingDocumentType, elementId,
						category)
				.stream().map(item -> mapper.map(item, AcmDocumentsDTO.class))
				.collect(Collectors.toList());

	}

}
