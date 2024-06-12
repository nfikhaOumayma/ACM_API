/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.GedClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.NationalIdNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ThirdPartyHistoriqueRepository;
import com.acm.service.CustomerService;
import com.acm.service.LoanService;
import com.acm.service.ThirdPartyHistoriqueService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.ThirdPartyCategory;
import com.acm.utils.enums.ThirdPartyStatus;
import com.acm.utils.models.QThirdPartyHistorique;
import com.acm.utils.models.ThirdPartyHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.OrderSpecifier;

/**
 * {@link ThirdPartyHistoriqueServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@Service
public class ThirdPartyHistoriqueServiceImpl implements ThirdPartyHistoriqueService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(ThirdPartyHistoriqueServiceImpl.class);

	/** The third party historique repository. */
	@Autowired
	private ThirdPartyHistoriqueRepository thirdPartyHistoriqueRepository;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The third party transvers client. */
	@Autowired
	private TransversClient thirdPartyTransversClient;

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The Constant CUSTOMER. */
	private static final String CUSTOMER = "CUSTOMER";

	/** The Constant GUARANTOR. */
	private static final String GUARANTOR = "GUARANTOR";

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#find(java.lang.Long)
	 */
	@Override
	public ThirdPartyHistoriqueDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find 3-RD Party Historique by ID : {}", id);
		ThirdPartyHistorique thirdPartyHistorique =
				thirdPartyHistoriqueRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(thirdPartyHistorique)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ThirdPartyHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ ThirdPartyHistorique.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(thirdPartyHistorique, ThirdPartyHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#find(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public List<ThirdPartyHistoriqueDTO> find(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// check IdCustomer && idLoan
		if (ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomer())
				&& ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdLoan())
				&& ACMValidationUtils
						.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomerGuarantor())
				&& ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomer())
				&& ACMValidationUtils
						.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())) {
			return new ArrayList<>();
		}

		// init QThirdPartyHistorique
		QThirdPartyHistorique qThirdPartyHistorique = QThirdPartyHistorique.thirdPartyHistorique;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qThirdPartyHistorique.enabled.eq(Boolean.TRUE));
		// find by IdCustomer
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomer())) {
			predicate.and(
					qThirdPartyHistorique.idCustomer.eq(thirdPartyHistoriqueDTO.getIdCustomer()));
		}
		// find by IdLoan
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdLoan())) {
			predicate.and(qThirdPartyHistorique.idLoan.eq(thirdPartyHistoriqueDTO.getIdLoan()));
		}

		// find by category
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getCategory())) {
			predicate.and(qThirdPartyHistorique.category.eq(thirdPartyHistoriqueDTO.getCategory()));
		}

		// find by identity customer or identity guarantor if identitycustomer is not null
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomer())) {
			predicate.and(qThirdPartyHistorique.identityCustomer
					.eq(thirdPartyHistoriqueDTO.getIdentityCustomer())
					.or(qThirdPartyHistorique.identityCustomerGuarantor
							.eq(thirdPartyHistoriqueDTO.getIdentityCustomer())));
		}
		// find by identity customer or identity guarantor if identityguarantor is not null

		else if (!ACMValidationUtils
				.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())) {
			predicate.and(qThirdPartyHistorique.identityCustomer
					.eq(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())
					.or(qThirdPartyHistorique.identityCustomerGuarantor
							.eq(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())));
		}
		// find by Status
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getStatus())) {
			predicate.and(qThirdPartyHistorique.status.eq(thirdPartyHistoriqueDTO.getStatus()));
		}

		// find IN StatusList
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getStatusList())) {
			predicate.and(qThirdPartyHistorique.status.in(thirdPartyHistoriqueDTO.getStatusList()));
		}
		// find by query_date
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getQueryDate())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(
					DateUtil.convertToLocalDateViaInstant(thirdPartyHistoriqueDTO.getQueryDate()));
			predicate.and(qThirdPartyHistorique.queryDate.between(sqlDate, new Date()));
		}

		// QueryDSL using springDATA
		Iterable<ThirdPartyHistorique> iterable = thirdPartyHistoriqueRepository.findAll(predicate,
				new Sort(Sort.Direction.DESC, "dateInsertion"));
		List<ThirdPartyHistorique> thirdPartyHistoriques = new ArrayList<>();
		iterable.forEach(thirdPartyHistoriques::add);
		logger.info("{} : 3-RD Party Historiques was founded", thirdPartyHistoriques.size());

		// mapping returned list
		List<ThirdPartyHistoriqueDTO> thirdPartyHistoriqueDTOs = new ArrayList<>();
		thirdPartyHistoriques.forEach(thirdPartyHistorique -> thirdPartyHistoriqueDTOs
				.add(mapper.map(thirdPartyHistorique, ThirdPartyHistoriqueDTO.class)));

		logger.info("Returning founded data ...");
		return thirdPartyHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#save(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public ThirdPartyHistoriqueDTO save(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ThirdPartyHistorique thirdPartyHistorique =
				mapper.map(thirdPartyHistoriqueDTO, ThirdPartyHistorique.class);
		CommonFunctions.mapperToSave(thirdPartyHistorique, userClient, logger);
		ThirdPartyHistorique newThirdPartyHistorique =
				thirdPartyHistoriqueRepository.save(thirdPartyHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				ThirdPartyHistorique.class.getSimpleName());
		return mapper.map(newThirdPartyHistorique, ThirdPartyHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#save(java.lang.Long,
	 * com.acm.utils.dtos.ThirdPartyHistoriqueDTO)
	 */
	@Override
	public ThirdPartyHistoriqueDTO save(Long id, ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update ThirdPartyHistorique with ID = {}", id);
		ThirdPartyHistorique oldThirdPartyHistorique =
				thirdPartyHistoriqueRepository.findById(id).orElse(null);

		// check if object is null
		if (oldThirdPartyHistorique == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ThirdPartyHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ThirdPartyHistorique.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldThirdPartyHistorique)
		mapper.map(thirdPartyHistoriqueDTO, oldThirdPartyHistorique);
		CommonFunctions.mapperToUpdate(oldThirdPartyHistorique, userClient, logger);

		// update & persist data in DB
		ThirdPartyHistorique newThirdPartyHistorique =
				thirdPartyHistoriqueRepository.save(oldThirdPartyHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				ThirdPartyHistorique.class.getSimpleName());
		return mapper.map(newThirdPartyHistorique, ThirdPartyHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#findForScreening(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public List<ThirdPartyHistoriqueDTO> findForScreening(
			ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) throws NationalIdNotFoundException {

		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// check identity customer && identity guarantor
		Boolean nationalIdNotFound = Boolean.FALSE;
		List<String> messagesError = new ArrayList<>();
		if (ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomer())
				&& ACMValidationUtils
						.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())) {
			if (thirdPartyHistoriqueDTO.getCategory().equals(CommonConstants.CUSTOMER_CATEGORY)) {
				messagesError.add(CommonConstants.CUSTOMER_CATEGORY);
				nationalIdNotFound = Boolean.TRUE;
			}
			if (thirdPartyHistoriqueDTO.getCategory().equals(CommonConstants.RELATION_GUARANTOR)) {
				messagesError.add(CommonConstants.RELATION_GUARANTOR);
				nationalIdNotFound = Boolean.TRUE;
			}

			if (nationalIdNotFound.equals(Boolean.TRUE)) {
				// throw exception with listParameter
				logger.error("National Id not found : {}", messagesError);
				ExceptionResponseMessage exceptionResponseMessage =
						new ExceptionResponseMessage(CommonErrorCode.NATIONAL_ID_NOT_FOUND, null,
								new TechnicalException("National Id not found"), messagesError);
				throw new NationalIdNotFoundException(exceptionResponseMessage);
			}
		}
		// init list category
		List<String> listCatogory = new ArrayList<>();
		listCatogory.add(ThirdPartyCategory.AML.name());
		listCatogory.add(ThirdPartyCategory.ISCORE.name());
		listCatogory.add(ThirdPartyCategory.KYC.name());

		List<ThirdPartyHistoriqueDTO> filtredThirdPartyHistoriqueDTOs = new ArrayList<>();
		// init QThirdPartyHistorique
		QThirdPartyHistorique qThirdPartyHistorique = QThirdPartyHistorique.thirdPartyHistorique;
		for (String category : listCatogory) {
			// init Predicate
			BooleanBuilder predicate = new BooleanBuilder();
			// find only enabled data
			predicate.and(qThirdPartyHistorique.enabled.eq(Boolean.TRUE));
			// find by IdCustomer
			if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomer())) {
				predicate.and(qThirdPartyHistorique.idCustomer
						.eq(thirdPartyHistoriqueDTO.getIdCustomer()));
			}
			// find by IdCustomerGuarantor
			if (!ACMValidationUtils
					.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdCustomerGuarantor())) {
				predicate.and(qThirdPartyHistorique.idCustomerGuarantor
						.eq(thirdPartyHistoriqueDTO.getIdCustomerGuarantor()));
			}
			// find by IdLoan
			if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdLoan())) {
				predicate.and(qThirdPartyHistorique.idLoan.eq(thirdPartyHistoriqueDTO.getIdLoan()));
			}
			// find by identity customer or identity guarantor if identitycustomer is not null
			if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomer())) {
				predicate.and(qThirdPartyHistorique.identityCustomer
						.eq(thirdPartyHistoriqueDTO.getIdentityCustomer())
						.or(qThirdPartyHistorique.identityCustomerGuarantor
								.eq(thirdPartyHistoriqueDTO.getIdentityCustomer())));
			}
			// find by identity customer or identity guarantor if identityguarantor is not null
			else if (!ACMValidationUtils
					.isNullOrEmpty(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())) {
				predicate.and(qThirdPartyHistorique.identityCustomer
						.eq(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())
						.or(qThirdPartyHistorique.identityCustomerGuarantor
								.eq(thirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())));
			}
			// QueryDSL using springDATA
			OrderSpecifier<Date> sortOrder1 = qThirdPartyHistorique.dateInsertion.desc();
			OrderSpecifier<String> sortOrder2 = qThirdPartyHistorique.category.asc();
			// find by category
			predicate.and(qThirdPartyHistorique.category.eq(category));
			Iterable<ThirdPartyHistorique> iterable =
					thirdPartyHistoriqueRepository.findAll(predicate, sortOrder1, sortOrder2);
			List<ThirdPartyHistorique> thirdPartyHistoriques = new ArrayList<>();
			iterable.forEach(thirdPartyHistoriques::add);
			if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriques)) {
				filtredThirdPartyHistoriqueDTOs.add(
						mapper.map(thirdPartyHistoriques.get(0), ThirdPartyHistoriqueDTO.class));
			}
		}

		logger.info("filtredThirdPartyHistoriqueDTOs {} : 3-RD Party Historiques was founded",
				filtredThirdPartyHistoriqueDTOs.size());
		return filtredThirdPartyHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#find(java.lang.String, java.lang.Long,
	 * java.lang.Long, java.lang.String)
	 */
	@Override
	public List<ThirdPartyHistoriqueDTO> find(String category, Long idLoan, Long idCustomer,
			String categoryCustomer) {

		logger.info(
				"Find thirdPartyHistoriques by CATEGORY : {} | ID_LOAN : {} | ID_CUSTOMER : {} | CATEGORY_CUSTOMER : {}",
				category, idLoan, idCustomer, categoryCustomer);

		// Check given params
		if (ACMValidationUtils.isNullOrEmpty(category) || ACMValidationUtils.isNullOrEmpty(idLoan)
				|| ACMValidationUtils.isNullOrEmpty(idCustomer)
				|| ACMValidationUtils.isNullOrEmpty(categoryCustomer)) {
			return new ArrayList<>();
		}

		// Init && Search
		List<ThirdPartyHistorique> thirdPartyHistoriques = new ArrayList<>();
		if (categoryCustomer.equals(CUSTOMER)) {
			thirdPartyHistoriques = thirdPartyHistoriqueRepository
					.findByCategoryAndIdLoanAndIdCustomer(category, idLoan, idCustomer);
		}
		else if (categoryCustomer.equals(GUARANTOR)) {
			thirdPartyHistoriques = thirdPartyHistoriqueRepository
					.findByCategoryAndIdLoanAndIdCustomerGuarantor(category, idLoan, idCustomer);
		}

		// Check returned list
		if (ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriques)) {
			return new ArrayList<>();
		}
		// mapping data
		List<ThirdPartyHistoriqueDTO> thirdPartyHistoriqueDTOs = new ArrayList<>();
		thirdPartyHistoriques.forEach(model -> thirdPartyHistoriqueDTOs
				.add(mapper.map(model, ThirdPartyHistoriqueDTO.class)));
		logger.info(" {} : ThirdPartyHistorique was founded", thirdPartyHistoriqueDTOs.size());
		return thirdPartyHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#generateIscoreReport(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public byte[] generateIscoreReport(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException, GEDException {

		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// load && parse response data
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getRequestValue())
				&& ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getReportTag())) {
			logger.debug("generate Iscore Report : request = {}",
					thirdPartyHistoriqueDTO.getRequestValue());
			// call API API && return byte[]
			ScreeningDTO params = new ScreeningDTO();
			params.setXmlRequest(thirdPartyHistoriqueDTO.getRequestValue());
			// generated byte from ISCORE API
			byte[] iscoreReportByte = thirdPartyTransversClient.generateIScoreReport(params);

			// *** SAVE GENERATED REPORT IN GED ***
			// init data to be send to GED
			GedParameterDTO gedParameterDTO = new GedParameterDTO();
			gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
			gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
			// setting tags
			List<String> tags = new ArrayList<>();
			Long idCustomer = thirdPartyHistoriqueDTO.getIdCustomer() != null
					? thirdPartyHistoriqueDTO.getIdCustomer()
					: thirdPartyHistoriqueDTO.getIdCustomerGuarantor();
			// find customer data
			CustomerDTO customerDTO = customerService.findCustomer(idCustomer);
			// INIT tag = ISCORE - CUSTOMER_NUMBER - ID_THIRD_PARTY_HISTORIQUE
			tags.add(ThirdPartyCategory.ISCORE.name() + "-" + customerDTO.getCustomerNumber() + "-"
					+ thirdPartyHistoriqueDTO.getId());
			// set customer number
			gedParameterDTO.setCustomerNumber(customerDTO.getCustomerNumber());
			gedParameterDTO.setTags(tags);
			// convert byte to File
			List<File> filesToSend = new ArrayList<>();
			filesToSend.add(CommonFunctions.fileConverter(iscoreReportByte,
					environment.getProperty("spring.servlet.multipart.location"),
					ThirdPartyCategory.ISCORE.name()));
			gedParameterDTO.setFilesToUpload(filesToSend);
			try {
				// send to GED
				List<String> ids = gedClient.uploadToGed(gedParameterDTO);
				String idDocumentGED =
						ACMValidationUtils.isNullOrEmpty(ids) ? null : ids.get(0).split(";")[0];
				logger.info("INSERTED ID RREPORT IN GED : {} ", idDocumentGED);
				// update THIRD PARTY HISTORIQUE
				thirdPartyHistoriqueDTO.setReportTag(idDocumentGED);
				save(thirdPartyHistoriqueDTO.getId(), thirdPartyHistoriqueDTO);
			}
			catch (Exception e) {
				logger.error("FAILED TO SAVE IN GED");
				logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
						e.getMessage());
			}
			// returned result
			return iscoreReportByte;
		}
		else if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTO.getReportTag())) {
			// find by id ged
			GedDocumentDTO existingReportInGed =
					gedClient.find(thirdPartyHistoriqueDTO.getReportTag());
			// check & return byte[] if exist
			if (!ACMValidationUtils.isNullOrEmpty(existingReportInGed)) {
				return existingReportInGed.getDocumentContentByte();
			}
			else {
				// Fire exception
				ExceptionResponseMessage exceptionResponseMessage = new ExceptionResponseMessage(
						CommonErrorCode.DOCUMENT_ERROR,
						environment.getProperty("ged.document.found.error"),
						new TechnicalException(),
						Collections.singletonList(environment.getProperty("ged.document.error")));
				throw new GEDException(exceptionResponseMessage);
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ThirdPartyHistoriqueService#generateIscoreStoredReport(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public byte[] generateIscoreStoredReport(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(thirdPartyHistoriqueDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find 3-RD Party Historique by ID : {}", thirdPartyHistoriqueDTO.getId());
		ThirdPartyHistorique thirdPartyHistorique = thirdPartyHistoriqueRepository
				.findById(thirdPartyHistoriqueDTO.getId()).orElse(null);
		// check if object is null
		if (thirdPartyHistorique != null
				&& !ACMValidationUtils.isNullOrEmpty(thirdPartyHistorique.getReportIscore())) {
			// returning stored report as byte[]
			return thirdPartyHistorique.getReportIscore();
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#validate(com.acm.utils.dtos.
	 * ThirdPartyHistoriqueDTO)
	 */
	@Override
	public ThirdPartyHistoriqueDTO validate(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(thirdPartyHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// find loan data
		LoanDTO loanDTO = loanService.find(thirdPartyHistoriqueDTO.getIdLoan());
		// assign to default ACCOUNTPORTFOLIO_CODE
		UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
		loanDTO.setOwner(foundedUserDTO.getLogin());
		loanDTO.setOwnerName(foundedUserDTO.getSimpleName());
		loanService.save(loanDTO.getLoanId(), loanDTO);

		thirdPartyHistoriqueDTO.setStatus(ThirdPartyStatus.ACCEPTED.name());
		// save Third Party Historique
		return save(thirdPartyHistoriqueDTO.getId(), thirdPartyHistoriqueDTO);
	}

	/**
	 * Gets the user by portfolio id otherwise returning default user.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the username by portfolio id
	 */
	private UserDTO getUsernameByPortfolioId(LoanDTO loanDTO) {

		// default ADMIN user
		String userName = CommonConstants.DEFAULT_USER;
		// find user by AccountPortfolioId
		if (loanDTO.getPortfolioId() != null && loanDTO.getPortfolioId() != 0) {
			UserDTO params = new UserDTO();
			params.setAccountPortfolioId(loanDTO.getPortfolioId());
			List<UserDTO> userDTOs = userClient.find(params);
			// check && setting
			if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
				// setting userName
				return userDTOs.get(0);
			}
		}
		logger.info(" founded USER_NAME = [{}]", userName);
		return userClient.findByLogin(userName);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyHistoriqueService#findByIdLoan(java.lang.Long)
	 */
	@Override
	public List<ThirdPartyHistoriqueDTO> findByIdLoan(Long idLoan) {

		List<ThirdPartyHistoriqueDTO> thirdPartyHistoriqueDTOs = new ArrayList<>();
		List<ThirdPartyHistorique> thirdPartyHistoriques =
				thirdPartyHistoriqueRepository.findByIdLoan(idLoan);
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriques)) {
			thirdPartyHistoriques.forEach(item -> {
				thirdPartyHistoriqueDTOs.add(mapper.map(item, ThirdPartyHistoriqueDTO.class));
			});

		}
		return thirdPartyHistoriqueDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ThirdPartyHistoriqueService#findBySearchQueryIdAndCategory(java.lang.Long,
	 * java.lang.String)
	 */
	@Override
	public LoanDTO findBySearchQueryIdAndCategory(Long searchQueryId, String category)
			throws ResourcesNotFoundException {

		ThirdPartyHistorique thirdPartyHistorique = thirdPartyHistoriqueRepository
				.findBySearchQueryIdAndCategory(searchQueryId, category);

		return loanService.find(thirdPartyHistorique.getIdLoan());
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ThirdPartyHistoriqueService#findByCustomerIdReisAndCategory(java.lang.Long,
	 * java.lang.String)
	 */
	@Override
	public LoanDTO findByCustomerIdReisAndCategory(Long customerIdReis, String category)
			throws ResourcesNotFoundException {

		ThirdPartyHistorique thirdPartyHistorique = thirdPartyHistoriqueRepository
				.findByCustomerReisIdAndCategory(customerIdReis, category);

		return loanService.find(thirdPartyHistorique.getIdLoan());
	}

}
