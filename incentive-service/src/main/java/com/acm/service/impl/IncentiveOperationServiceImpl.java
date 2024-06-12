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
import com.acm.repository.IncentiveOperationRepository;
import com.acm.service.IncentiveOperationService;
import com.acm.utils.dtos.IncentiveOperationDTO;
import com.acm.utils.dtos.pagination.IncentiveOperationPaginationDTO;
import com.acm.utils.models.IncentiveOperation;
import com.acm.utils.models.QIncentiveOperation;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveOperationServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveOperationServiceImpl implements IncentiveOperationService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveOperationServiceImpl.class);

	/** The incentive operation repository. */
	@Autowired
	private IncentiveOperationRepository incentiveOperationRepository;

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
	 * @see com.acm.service.IncentiveOperationServiceImp#find(java.lang.Long)
	 */
	@Override
	public IncentiveOperationDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find incentiveOperation by ID : {}", id);
		IncentiveOperation incentiveOperation =
				incentiveOperationRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveOperation)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveOperation.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ IncentiveOperation.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(incentiveOperation, IncentiveOperationDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveOperationServiceImp#find(com.acm.utils.dtos.
	 * IncentiveOperationDTO)
	 */
	@Override
	public List<IncentiveOperationDTO> find(IncentiveOperationDTO incentiveOperationDto) {

		Preconditions.checkNotNull(incentiveOperationDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QIncentiveOperation
		QIncentiveOperation qIncentiveOperation = QIncentiveOperation.incentiveOperation;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveOperation.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils.isNullOrEmpty(incentiveOperationDto.getProductId())) {
			predicate.and(qIncentiveOperation.productId.eq(incentiveOperationDto.getProductId()));
		}
		// find by frequency
		if (!ACMValidationUtils.isNullOrEmpty(incentiveOperationDto.getFrequency())
				&& !ACMValidationUtils
						.isNullOrEmpty(incentiveOperationDto.getFrequency().getId())) {
			predicate.and(qIncentiveOperation.frequency.id
					.eq(incentiveOperationDto.getFrequency().getId()));
		}
		Iterable<IncentiveOperation> iterable = incentiveOperationRepository.findAll(predicate);
		List<IncentiveOperation> incentiveOperations = new ArrayList<>();
		iterable.forEach(incentiveOperations::add);
		// mapping data
		List<IncentiveOperationDTO> incentiveOperationDTOs = new ArrayList<>();
		incentiveOperations.forEach(incentiveOperation -> incentiveOperationDTOs
				.add(mapper.map(incentiveOperation, IncentiveOperationDTO.class)));
		logger.info("{} : incentiveOperation was founded", incentiveOperations.size());
		return incentiveOperationDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveOperationServiceImp#save(com.acm.utils.dtos.
	 * IncentiveOperationDTO)
	 */
	@Override
	public IncentiveOperationDTO save(IncentiveOperationDTO incentiveOperationDto)
			throws IncentiveRegistrationException {

		Preconditions.checkNotNull(incentiveOperationDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if the role is unique
		checkUniquinessByRole(incentiveOperationDto);
		IncentiveOperation incentiveOperation =
				mapper.map(incentiveOperationDto, IncentiveOperation.class);
		CommonFunctions.mapperToSave(incentiveOperation, userClient, logger);

		IncentiveOperation newIncentiveOperation =
				incentiveOperationRepository.save(incentiveOperation);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				IncentiveOperation.class.getSimpleName());
		return mapper.map(newIncentiveOperation, IncentiveOperationDTO.class);
	}

	/**
	 * Check uniquiness.
	 * 
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the boolean
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	private Boolean checkUniquinessByRole(IncentiveOperationDTO incentiveOperationDTO)
			throws IncentiveRegistrationException {

		Preconditions.checkNotNull(incentiveOperationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<IncentiveOperation> incentiveOperations =
				incentiveOperationRepository.findByRole(incentiveOperationDTO.getRole());
		// throw exception if the setting is already exist in DB
		if (!ACMValidationUtils.isNullOrEmpty(incentiveOperations)) {
			throw new IncentiveRegistrationException(
					new ExceptionResponseMessage(CommonErrorCode.INCENTIVE_OPERATION_SETTING_EXIST,
							CommonExceptionsMessage.INCENTIVE_SETTING_EXIST_ALREADY,
							new TechnicalException()),
					CommonExceptionsMessage.INCENTIVE_SETTING_EXIST_ALREADY);
		}
		return Boolean.TRUE;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveOperationServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.IncentiveOperationDTO)
	 */
	@Override
	public IncentiveOperationDTO save(Long id, IncentiveOperationDTO incentiveOperationDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(incentiveOperationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if the role is unique
		checkUniquinessByRole(incentiveOperationDTO);
		logger.info("Update IncentiveOperation  with ID = {}", id);
		IncentiveOperation oldIncentiveOperation =
				incentiveOperationRepository.findById(id).orElse(null);
		// check if object is null
		if (oldIncentiveOperation == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveOperation.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveOperation.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// Init FK reference
		oldIncentiveOperation.setIncentiveType(null);
		oldIncentiveOperation.setFrequency(null);
		oldIncentiveOperation.setBasedOn(null);
		// mapping new data with existing data (oldIncentiveOperation)
		mapper.map(incentiveOperationDTO, oldIncentiveOperation);
		CommonFunctions.mapperToUpdate(oldIncentiveOperation, userClient, logger);
		IncentiveOperation newIncentiveOperation =
				incentiveOperationRepository.save(oldIncentiveOperation);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveOperation.class.getSimpleName());
		return mapper.map(newIncentiveOperation, IncentiveOperationDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveOperationService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Incentive Operation Settinge with ID = {}", id);
		// fin Incentive Setting by id
		IncentiveOperation deletedIncentiveOperation =
				incentiveOperationRepository.findById(id).orElse(null);
		// update ordre after deleteing an IncentiveSetting
		Long oldOrdre =
				deletedIncentiveOperation != null ? deletedIncentiveOperation.getOrdre() : 0L;
		// delete old Incentive Operation Type
		incentiveOperationRepository.deleteById(id);
		// find IncentiveSetting by ordre greater than the ordre of deleted Incentive Setting
		List<IncentiveOperation> incentiveOperations =
				incentiveOperationRepository.findByOrdreGreaterThan(oldOrdre);
		// updated Ordre
		for (IncentiveOperation incentiveOperation : incentiveOperations) {
			incentiveOperation.setOrdre(incentiveOperation.getOrdre() - 1);
			incentiveOperationRepository.save(incentiveOperation);
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				IncentiveOperation.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveOperationService#saveStatus(com.acm.utils.dtos.
	 * IncentiveOperationDTO)
	 */
	@Override
	public List<IncentiveOperationDTO> saveStatus(IncentiveOperationDTO incentiveOperationDTO) {

		// Check incentiveOperationDTO not null
		Preconditions.checkNotNull(incentiveOperationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check productId not null
		Preconditions.checkNotNull(incentiveOperationDTO.getProductId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// Get Old incentiveOperations by productId
		List<IncentiveOperation> incentiveOperations =
				incentiveOperationRepository.findByProductId(incentiveOperationDTO.getProductId());
		// update enabled / disabled status
		for (IncentiveOperation incentiveRegistration : incentiveOperations) {
			incentiveRegistration.setEnabled(incentiveOperationDTO.getEnabled());
			incentiveOperationRepository.save(incentiveRegistration);
		}
		// init list
		List<IncentiveOperationDTO> incentiveOperationsDTOs = new ArrayList<>();
		incentiveOperations.forEach(incentiveOperation -> incentiveOperationsDTOs
				.add(mapper.map(incentiveOperation, IncentiveOperationDTO.class)));
		return incentiveOperationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveOperationService#find(com.acm.utils.dtos.pagination.
	 * IncentiveOperationPaginationDTO)
	 */
	@Override
	public IncentiveOperationPaginationDTO find(
			IncentiveOperationPaginationDTO incentiveOperationPaginationDTO) {

		Preconditions.checkNotNull(incentiveOperationPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(incentiveOperationPaginationDTO.getPageNumber())) {
			incentiveOperationPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(incentiveOperationPaginationDTO.getPageSize())) {
			incentiveOperationPaginationDTO.setPageSize(10);
		}
		// setting default data
		incentiveOperationPaginationDTO.setResultsIncentiveOperations(new ArrayList<>());
		// setting default totals pages
		incentiveOperationPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		incentiveOperationPaginationDTO.setTotalPages(0);
		// init QIncentiveOperation
		QIncentiveOperation qIncentiveOperation = QIncentiveOperation.incentiveOperation;
		// build Predicate using given params
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveOperation.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveOperationPaginationDTO.getParams().getProductId())) {
			predicate.and(qIncentiveOperation.productId
					.eq(incentiveOperationPaginationDTO.getParams().getProductId()));
		}
		// find by frequency
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveOperationPaginationDTO.getParams().getFrequency())
				&& !ACMValidationUtils.isNullOrEmpty(
						incentiveOperationPaginationDTO.getParams().getFrequency().getId())) {
			predicate.and(qIncentiveOperation.frequency.id
					.eq(incentiveOperationPaginationDTO.getParams().getFrequency().getId()));
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(incentiveOperationPaginationDTO.getSortDirection()) && !ACMValidationUtils
				.isNullOrEmpty(incentiveOperationPaginationDTO.getSortField())) {
			pageable = PageRequest.of(incentiveOperationPaginationDTO.getPageNumber(),
					incentiveOperationPaginationDTO.getPageSize(), Sort.Direction.ASC,
					incentiveOperationPaginationDTO.getSortField());
		}
		else if ("-1".equals(incentiveOperationPaginationDTO.getSortDirection())
				&& !ACMValidationUtils
						.isNullOrEmpty(incentiveOperationPaginationDTO.getSortField())) {
			pageable = PageRequest.of(incentiveOperationPaginationDTO.getPageNumber(),
					incentiveOperationPaginationDTO.getPageSize(), Sort.Direction.DESC,
					incentiveOperationPaginationDTO.getSortField());
		}
		else {
			// default sort by code : ASC
			pageable = PageRequest.of(incentiveOperationPaginationDTO.getPageNumber(),
					incentiveOperationPaginationDTO.getPageSize(), Sort.by(Direction.ASC, "ordre"));
		}

		// load data
		Page<IncentiveOperation> pagedResult =
				incentiveOperationRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<IncentiveOperation> incentiveOperations = pagedResult.getContent();
			logger.info("{} : incentiveRegistration was founded (PageNumber = {} / PageSize = {} )",
					incentiveOperations.size(), incentiveOperationPaginationDTO.getPageNumber(),
					incentiveOperationPaginationDTO.getPageSize());
			List<IncentiveOperationDTO> incentiveOperationDTOs = new ArrayList<>();
			incentiveOperations.forEach(card -> incentiveOperationDTOs
					.add(mapper.map(card, IncentiveOperationDTO.class)));
			// setting data
			incentiveOperationPaginationDTO.setResultsIncentiveOperations(incentiveOperationDTOs);
			// setting totals pages
			incentiveOperationPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			incentiveOperationPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return incentiveOperationPaginationDTO;
	}

}
