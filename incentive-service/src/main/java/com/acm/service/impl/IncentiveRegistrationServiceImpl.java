/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.IncentiveRegistrationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.IncentiveRegistrationRepository;
import com.acm.service.IncentiveRegistrationService;
import com.acm.utils.dtos.IncentiveRegistrationDTO;
import com.acm.utils.dtos.pagination.IncentiveRegistrationPaginationDTO;
import com.acm.utils.models.IncentiveRegistration;
import com.acm.utils.models.IncentiveSettingConstant;
import com.acm.utils.models.QIncentiveRegistration;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveRegistrationServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveRegistrationServiceImpl implements IncentiveRegistrationService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveRegistrationServiceImpl.class);

	/** The incentive registration repository. */
	@Autowired
	private IncentiveRegistrationRepository incentiveRegistrationRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationServiceImp#find(java.lang.Long)
	 */
	@Override
	public IncentiveRegistrationDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find incentiveRegistration by ID : {}", id);
		IncentiveRegistration incentiveRegistration =
				incentiveRegistrationRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveRegistration)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveRegistration.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ IncentiveRegistration.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(incentiveRegistration, IncentiveRegistrationDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationServiceImp#find(com.acm.utils.dtos.
	 * IncentiveRegistrationDTO)
	 */
	@Override
	public List<IncentiveRegistrationDTO> find(IncentiveRegistrationDTO incentiveRegistrationDto) {

		Preconditions.checkNotNull(incentiveRegistrationDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QIncentiveRegistration
		QIncentiveRegistration qIncentiveRegistration =
				QIncentiveRegistration.incentiveRegistration;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveRegistration.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRegistrationDto.getProductId())) {
			predicate.and(
					qIncentiveRegistration.productId.eq(incentiveRegistrationDto.getProductId()));
		}
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRegistrationDto.getFrequency())
				&& !ACMValidationUtils
						.isNullOrEmpty(incentiveRegistrationDto.getFrequency().getId())) {
			predicate.and(qIncentiveRegistration.frequency.id
					.eq(incentiveRegistrationDto.getFrequency().getId()));
		}
		Iterable<IncentiveRegistration> iterable =
				incentiveRegistrationRepository.findAll(predicate);
		List<IncentiveRegistration> incentiveRegistrations = new ArrayList<>();
		iterable.forEach(incentiveRegistrations::add);
		// mapping data
		List<IncentiveRegistrationDTO> incentiveRegistrationDTOs = new ArrayList<>();
		incentiveRegistrations.forEach(incentiveRegistration -> incentiveRegistrationDTOs
				.add(mapper.map(incentiveRegistration, IncentiveRegistrationDTO.class)));
		logger.info("{} : incentiveRegistration was founded", incentiveRegistrations.size());
		return incentiveRegistrationDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationServiceImp#save(com.acm.utils.dtos.
	 * IncentiveRegistrationDTO)
	 */
	@Override
	public IncentiveRegistrationDTO save(IncentiveRegistrationDTO incentiveRegistrationDto)
			throws IncentiveRegistrationException {

		Preconditions.checkNotNull(incentiveRegistrationDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if the role and customer type are unique
		checkUniquinessByRoleAndCustomerType(incentiveRegistrationDto);
		IncentiveRegistration incentiveRegistration =
				mapper.map(incentiveRegistrationDto, IncentiveRegistration.class);
		CommonFunctions.mapperToSave(incentiveRegistration, userClient, logger);

		IncentiveRegistration newIncentiveRegistration =
				incentiveRegistrationRepository.saveAndFlush(incentiveRegistration);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				IncentiveRegistration.class.getSimpleName());
		return mapper.map(newIncentiveRegistration, IncentiveRegistrationDTO.class);
	}

	/**
	 * Check uniquiness by role and customer type.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the boolean
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	private Boolean checkUniquinessByRoleAndCustomerType(
			IncentiveRegistrationDTO incentiveRegistrationDTO)
			throws IncentiveRegistrationException {

		Preconditions.checkNotNull(incentiveRegistrationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<IncentiveRegistration> incentiveRegistrations =
				incentiveRegistrationRepository.findByRoleAndCustomerType(
						incentiveRegistrationDTO.getRole(), new IncentiveSettingConstant(
								incentiveRegistrationDTO.getCustomerType().getId()));
		// throw exception if the setting is already exist in DB
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRegistrations)) {
			throw new IncentiveRegistrationException(
					new ExceptionResponseMessage(
							CommonErrorCode.INCENTIVE_REGISTRATION_SETTING_EXIST,
							CommonExceptionsMessage.INCENTIVE_SETTING_EXIST_ALREADY,
							new TechnicalException()),
					CommonExceptionsMessage.INCENTIVE_SETTING_EXIST_ALREADY);
		}
		return Boolean.TRUE;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.IncentiveRegistrationDTO)
	 */
	@Override
	public IncentiveRegistrationDTO save(Long id, IncentiveRegistrationDTO incentiveRegistrationDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(incentiveRegistrationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if the role and the customer type are unique
		checkUniquinessByRoleAndCustomerType(incentiveRegistrationDTO);
		logger.info("Update IncentiveRegistration  with ID = {}", id);
		IncentiveRegistration oldIncentiveRegistration =
				incentiveRegistrationRepository.findById(id).orElse(null);
		// check if object is null
		if (oldIncentiveRegistration == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveRegistration.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveRegistration.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// Init FK reference
		oldIncentiveRegistration.setCustomerType(null);
		oldIncentiveRegistration.setIncentiveType(null);
		oldIncentiveRegistration.setFrequency(null);
		oldIncentiveRegistration.setBasedOn(null);
		// mapping new data with existing data (oldIncentiveRegistration)
		mapper.map(incentiveRegistrationDTO, oldIncentiveRegistration);
		CommonFunctions.mapperToUpdate(oldIncentiveRegistration, userClient, logger);
		IncentiveRegistration newIncentiveRegistration =
				incentiveRegistrationRepository.saveAndFlush(oldIncentiveRegistration);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveRegistration.class.getSimpleName());
		return mapper.map(newIncentiveRegistration, IncentiveRegistrationDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Incentive Registration Settinge with ID = {}", id);
		// find Incentive Registration by id
		IncentiveRegistration deletedIncentiveRegistration =
				incentiveRegistrationRepository.findById(id).orElse(null);
		// update ordre after deleteing an IncentiveRepayment
		Long oldOrdre =
				deletedIncentiveRegistration != null ? deletedIncentiveRegistration.getOrdre() : 0L;
		// delete old Incentive Registration Type
		incentiveRegistrationRepository.deleteById(id);
		// find IncentiveRegistration by ordre greater than the ordre of deleted Incentive
		// Registration
		List<IncentiveRegistration> incentiveRegistrations =
				incentiveRegistrationRepository.findByOrdreGreaterThan(oldOrdre);
		// updated Ordre
		for (IncentiveRegistration incentiveRegistration : incentiveRegistrations) {
			incentiveRegistration.setOrdre(incentiveRegistration.getOrdre() - 1);
			incentiveRegistrationRepository.save(incentiveRegistration);
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				IncentiveRegistration.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationService#saveStatus(com.acm.utils.dtos.
	 * IncentiveRegistrationDTO)
	 */
	@Override
	public List<IncentiveRegistrationDTO> saveStatus(
			IncentiveRegistrationDTO incentiveRegistrationDTO) {

		// Check incentiveRegistrationDTO not null
		Preconditions.checkNotNull(incentiveRegistrationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check productId not null
		Preconditions.checkNotNull(incentiveRegistrationDTO.getProductId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// Get Old incentiveRegistrations by productId
		List<IncentiveRegistration> incentiveRegistrations = incentiveRegistrationRepository
				.findByProductId(incentiveRegistrationDTO.getProductId());
		// update enabled / disabled status
		for (IncentiveRegistration incentiveRegistration : incentiveRegistrations) {
			incentiveRegistration.setEnabled(incentiveRegistrationDTO.getEnabled());
			incentiveRegistrationRepository.save(incentiveRegistration);
		}
		// init list
		List<IncentiveRegistrationDTO> incentiveRegistrationDTOs = new ArrayList<>();
		incentiveRegistrations.forEach(incentiveRegistration -> incentiveRegistrationDTOs
				.add(mapper.map(incentiveRegistration, IncentiveRegistrationDTO.class)));
		return incentiveRegistrationDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRegistrationService#find(com.acm.utils.dtos.pagination.
	 * IncentiveRegistrationPaginationDTO)
	 */
	@Override
	public IncentiveRegistrationPaginationDTO find(
			IncentiveRegistrationPaginationDTO incentiveRegistrationPaginationDTO) {

		Preconditions.checkNotNull(incentiveRegistrationPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(incentiveRegistrationPaginationDTO.getPageNumber())) {
			incentiveRegistrationPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(incentiveRegistrationPaginationDTO.getPageSize())) {
			incentiveRegistrationPaginationDTO.setPageSize(10);
		}
		// setting default data
		incentiveRegistrationPaginationDTO.setResultsIncentiveRegistrations(new ArrayList<>());
		// setting default totals pages
		incentiveRegistrationPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		incentiveRegistrationPaginationDTO.setTotalPages(0);
		// init QIncentiveRegistration
		QIncentiveRegistration qIncentiveRegistration =
				QIncentiveRegistration.incentiveRegistration;
		// build Predicate using given params
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveRegistration.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRegistrationPaginationDTO.getParams().getProductId())) {
			predicate.and(qIncentiveRegistration.productId
					.eq(incentiveRegistrationPaginationDTO.getParams().getProductId()));
		}
		// find by frequency
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRegistrationPaginationDTO.getParams().getFrequency())
				&& !ACMValidationUtils.isNullOrEmpty(
						incentiveRegistrationPaginationDTO.getParams().getFrequency().getId())) {
			predicate.and(qIncentiveRegistration.frequency.id
					.eq(incentiveRegistrationPaginationDTO.getParams().getFrequency().getId()));
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(incentiveRegistrationPaginationDTO.getSortDirection()) && !ACMValidationUtils
				.isNullOrEmpty(incentiveRegistrationPaginationDTO.getSortField())) {
			pageable = PageRequest.of(incentiveRegistrationPaginationDTO.getPageNumber(),
					incentiveRegistrationPaginationDTO.getPageSize(), Sort.Direction.ASC,
					incentiveRegistrationPaginationDTO.getSortField());
		}
		else if ("-1".equals(incentiveRegistrationPaginationDTO.getSortDirection())
				&& !ACMValidationUtils
						.isNullOrEmpty(incentiveRegistrationPaginationDTO.getSortField())) {
			pageable = PageRequest.of(incentiveRegistrationPaginationDTO.getPageNumber(),
					incentiveRegistrationPaginationDTO.getPageSize(), Sort.Direction.DESC,
					incentiveRegistrationPaginationDTO.getSortField());
		}
		else {
			// default sort by code : ASC
			pageable = PageRequest.of(incentiveRegistrationPaginationDTO.getPageNumber(),
					incentiveRegistrationPaginationDTO.getPageSize(),
					Sort.by(Direction.ASC, "ordre"));
		}

		// load data
		Page<IncentiveRegistration> pagedResult =
				incentiveRegistrationRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<IncentiveRegistration> incentiveRegistrations = pagedResult.getContent();
			logger.info("{} : incentiveRegistration was founded (PageNumber = {} / PageSize = {} )",
					incentiveRegistrations.size(),
					incentiveRegistrationPaginationDTO.getPageNumber(),
					incentiveRegistrationPaginationDTO.getPageSize());
			List<IncentiveRegistrationDTO> incentiveRegistrationDTOs = new ArrayList<>();
			incentiveRegistrations.forEach(card -> incentiveRegistrationDTOs
					.add(mapper.map(card, IncentiveRegistrationDTO.class)));
			// setting data
			incentiveRegistrationPaginationDTO
					.setResultsIncentiveRegistrations(incentiveRegistrationDTOs);
			// setting totals pages
			incentiveRegistrationPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			incentiveRegistrationPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return incentiveRegistrationPaginationDTO;
	}
}
