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
import com.acm.repository.IncentiveRepaymentRepository;
import com.acm.service.IncentiveRepaymentService;
import com.acm.utils.dtos.IncentiveRepaymentDTO;
import com.acm.utils.dtos.pagination.IncentiveRepaymentPaginationDTO;
import com.acm.utils.models.IncentiveRepayment;
import com.acm.utils.models.QIncentiveRepayment;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveRepaymentServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveRepaymentServiceImpl implements IncentiveRepaymentService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveRepaymentServiceImpl.class);

	/** The incentive repayment repository. */
	@Autowired
	private IncentiveRepaymentRepository incentiveRepaymentRepository;

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
	 * @see com.acm.service.IncentiveRepaymentServiceImp#find(java.lang.Long)
	 */
	@Override
	public IncentiveRepaymentDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find incentiveRepayment by ID : {}", id);
		IncentiveRepayment incentiveRepayment =
				incentiveRepaymentRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveRepayment)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveRepayment.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ IncentiveRepayment.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(incentiveRepayment, IncentiveRepaymentDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentServiceImp#find(com.acm.utils.dtos.
	 * IncentiveRepaymentDTO)
	 */
	@Override
	public List<IncentiveRepaymentDTO> find(IncentiveRepaymentDTO incentiveRepaymentDto) {

		Preconditions.checkNotNull(incentiveRepaymentDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QIncentiveRepayment
		QIncentiveRepayment qIncentiveRepayment = QIncentiveRepayment.incentiveRepayment;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveRepayment.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRepaymentDto.getProductId())) {
			predicate.and(qIncentiveRepayment.productId.eq(incentiveRepaymentDto.getProductId()));
		}
		// find by frequency
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRepaymentDto.getFrequency())
				&& !ACMValidationUtils
						.isNullOrEmpty(incentiveRepaymentDto.getFrequency().getId())) {
			predicate.and(qIncentiveRepayment.frequency.id
					.eq(incentiveRepaymentDto.getFrequency().getId()));
		}
		Iterable<IncentiveRepayment> iterable = incentiveRepaymentRepository.findAll(predicate);
		List<IncentiveRepayment> incentiveRepayments = new ArrayList<>();
		iterable.forEach(incentiveRepayments::add);
		// mapping data
		List<IncentiveRepaymentDTO> incentiveRepaymentDTOs = new ArrayList<>();
		incentiveRepayments.forEach(incentiveRepayment -> incentiveRepaymentDTOs
				.add(mapper.map(incentiveRepayment, IncentiveRepaymentDTO.class)));
		logger.info("{} : incentiveRepayment was founded", incentiveRepayments.size());
		return incentiveRepaymentDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentServiceImp#save(com.acm.utils.dtos.
	 * IncentiveRepaymentDTO)
	 */
	@Override
	public IncentiveRepaymentDTO save(IncentiveRepaymentDTO incentiveRepaymentDto)
			throws IncentiveRegistrationException {

		Preconditions.checkNotNull(incentiveRepaymentDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		checkUniquiness(incentiveRepaymentDto);
		IncentiveRepayment incentiveRepayment =
				mapper.map(incentiveRepaymentDto, IncentiveRepayment.class);
		CommonFunctions.mapperToSave(incentiveRepayment, userClient, logger);

		IncentiveRepayment newIncentiveRepayment =
				incentiveRepaymentRepository.save(incentiveRepayment);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				IncentiveRepayment.class.getSimpleName());
		return mapper.map(newIncentiveRepayment, IncentiveRepaymentDTO.class);
	}

	/**
	 * Check uniquiness.
	 *
	 * @author idridi
	 * @param incentiveRepaymentDTO the incentive repayment DTO
	 * @return the boolean
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	private Boolean checkUniquiness(IncentiveRepaymentDTO incentiveRepaymentDTO)
			throws IncentiveRegistrationException {

		Preconditions.checkNotNull(incentiveRepaymentDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<IncentiveRepayment> incentiveRepayments = incentiveRepaymentRepository
				.findByRoleAndActiveCustomerIdAndProductivityIdAndRiskLevelId(
						incentiveRepaymentDTO.getRole(),
						incentiveRepaymentDTO.getActiveCustomerId(),
						incentiveRepaymentDTO.getProductivityId(),
						incentiveRepaymentDTO.getRiskLevelId());
		// throw exception if the setting is already exist in DB
		if (!ACMValidationUtils.isNullOrEmpty(incentiveRepayments)) {
			throw new IncentiveRegistrationException(
					new ExceptionResponseMessage(CommonErrorCode.INCENTIVE_SETTING_EXIST,
							CommonExceptionsMessage.INCENTIVE_SETTING_EXIST_ALREADY,
							new TechnicalException()),
					CommonExceptionsMessage.INCENTIVE_SETTING_EXIST_ALREADY);
		}
		return Boolean.TRUE;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.IncentiveRepaymentDTO)
	 */
	@Override
	public IncentiveRepaymentDTO save(Long id, IncentiveRepaymentDTO incentiveRepaymentDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(incentiveRepaymentDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update IncentiveRepayment  with ID = {}", id);
		IncentiveRepayment oldIncentiveRepayment =
				incentiveRepaymentRepository.findById(id).orElse(null);
		// check if object is null
		if (oldIncentiveRepayment == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveRepayment.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveRepayment.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		checkUniquiness(incentiveRepaymentDTO);
		// Init FK reference
		oldIncentiveRepayment.setIncentiveType(null);
		oldIncentiveRepayment.setFrequency(null);
		oldIncentiveRepayment.setBasedOn(null);
		// mapping new data with existing data (oldIncentiveRepayment)
		mapper.map(incentiveRepaymentDTO, oldIncentiveRepayment);
		CommonFunctions.mapperToUpdate(oldIncentiveRepayment, userClient, logger);
		IncentiveRepayment newIncentiveRepayment =
				incentiveRepaymentRepository.save(oldIncentiveRepayment);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveRepayment.class.getSimpleName());
		return mapper.map(newIncentiveRepayment, IncentiveRepaymentDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentService#find(com.acm.utils.dtos.pagination.
	 * IncentiveRepaymentPaginationDTO)
	 */
	@Override
	public IncentiveRepaymentPaginationDTO find(
			IncentiveRepaymentPaginationDTO incentiveRepaymentPaginationDTO) {

		Preconditions.checkNotNull(incentiveRepaymentPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(incentiveRepaymentPaginationDTO.getPageNumber())) {
			incentiveRepaymentPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(incentiveRepaymentPaginationDTO.getPageSize())) {
			incentiveRepaymentPaginationDTO.setPageSize(10);
		}
		// setting default data
		incentiveRepaymentPaginationDTO.setResultsIncentiveRepayments(new ArrayList<>());
		// setting default totals pages
		incentiveRepaymentPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		incentiveRepaymentPaginationDTO.setTotalPages(0);
		// init QIncentiveRepayment
		QIncentiveRepayment qIncentiveRepayment = QIncentiveRepayment.incentiveRepayment;
		// build Predicate using given params
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveRepayment.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getProductId())) {
			predicate.and(qIncentiveRepayment.productId
					.eq(incentiveRepaymentPaginationDTO.getParams().getProductId()));
		}
		// find by frequency
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getFrequency())
				&& !ACMValidationUtils.isNullOrEmpty(
						incentiveRepaymentPaginationDTO.getParams().getFrequency().getId())) {
			predicate.and(qIncentiveRepayment.frequency.id
					.eq(incentiveRepaymentPaginationDTO.getParams().getFrequency().getId()));
		}
		// find by incentive type
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getIncentiveType())
				&& !ACMValidationUtils.isNullOrEmpty(
						incentiveRepaymentPaginationDTO.getParams().getIncentiveType().getId())) {
			predicate.and(qIncentiveRepayment.incentiveType.id
					.eq(incentiveRepaymentPaginationDTO.getParams().getIncentiveType().getId()));
		}
		// find by basedon
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getBasedOnId())
				&& !ACMValidationUtils.isNullOrEmpty(
						incentiveRepaymentPaginationDTO.getParams().getBasedOnId().getId())) {
			predicate.and(qIncentiveRepayment.basedOn.id
					.eq(incentiveRepaymentPaginationDTO.getParams().getBasedOnId().getId()));
		}
		// find by active cutstomer
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getActiveCustomerId())) {
			predicate.and(qIncentiveRepayment.activeCustomerId
					.eq(incentiveRepaymentPaginationDTO.getParams().getActiveCustomerId()));
		}
		// find by productivity
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getProductivityId())) {
			predicate.and(qIncentiveRepayment.productivityId
					.eq(incentiveRepaymentPaginationDTO.getParams().getProductivityId()));
		}
		// find by risklevel
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getRiskLevelId())) {
			predicate.and(qIncentiveRepayment.riskLevelId
					.eq(incentiveRepaymentPaginationDTO.getParams().getRiskLevelId()));
		}
		// find by role
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getRole())) {
			predicate.and(qIncentiveRepayment.role
					.eq(incentiveRepaymentPaginationDTO.getParams().getRole()));
		}
		// find by frequency
		if (!ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getParams().getFrequency())
				&& !ACMValidationUtils.isNullOrEmpty(
						incentiveRepaymentPaginationDTO.getParams().getFrequency().getId())) {
			predicate.and(qIncentiveRepayment.frequency.id
					.eq(incentiveRepaymentPaginationDTO.getParams().getFrequency().getId()));
		}
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(incentiveRepaymentPaginationDTO.getSortDirection()) && !ACMValidationUtils
				.isNullOrEmpty(incentiveRepaymentPaginationDTO.getSortField())) {
			pageable = PageRequest.of(incentiveRepaymentPaginationDTO.getPageNumber(),
					incentiveRepaymentPaginationDTO.getPageSize(), Sort.Direction.ASC,
					incentiveRepaymentPaginationDTO.getSortField());
		}
		else if ("-1".equals(incentiveRepaymentPaginationDTO.getSortDirection())
				&& !ACMValidationUtils
						.isNullOrEmpty(incentiveRepaymentPaginationDTO.getSortField())) {
			pageable = PageRequest.of(incentiveRepaymentPaginationDTO.getPageNumber(),
					incentiveRepaymentPaginationDTO.getPageSize(), Sort.Direction.DESC,
					incentiveRepaymentPaginationDTO.getSortField());
		}
		else {
			// default sort by code : ASC
			pageable = PageRequest.of(incentiveRepaymentPaginationDTO.getPageNumber(),
					incentiveRepaymentPaginationDTO.getPageSize(), Sort.by(Direction.ASC, "ordre"));
		}

		// load data
		Page<IncentiveRepayment> pagedResult =
				incentiveRepaymentRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<IncentiveRepayment> incentiveRepayments = pagedResult.getContent();
			logger.info("{} : incentiveRegistration was founded (PageNumber = {} / PageSize = {} )",
					incentiveRepayments.size(), incentiveRepaymentPaginationDTO.getPageNumber(),
					incentiveRepaymentPaginationDTO.getPageSize());
			List<IncentiveRepaymentDTO> incentiveRepaymentDTOs = new ArrayList<>();
			incentiveRepayments.forEach(card -> incentiveRepaymentDTOs
					.add(mapper.map(card, IncentiveRepaymentDTO.class)));
			// setting data
			incentiveRepaymentPaginationDTO.setResultsIncentiveRepayments(incentiveRepaymentDTOs);
			// setting totals pages
			incentiveRepaymentPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			incentiveRepaymentPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return incentiveRepaymentPaginationDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveRepaymentService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Incentive Repayment Settinge with ID = {}", id);
		// find Incentive Repayment by id
		IncentiveRepayment deletedIncentiveRepayment =
				incentiveRepaymentRepository.findById(id).orElse(null);
		// update ordre after deleteing an IncentiveRepayment
		Long oldOrdre =
				deletedIncentiveRepayment != null ? deletedIncentiveRepayment.getOrdre() : 0L;

		// delete old Incentive Repayment Type
		incentiveRepaymentRepository.deleteById(id);
		// find IncentiveSetting by ordre greater than the ordre of deleted Incentive Setting
		List<IncentiveRepayment> incentiveRepayments =
				incentiveRepaymentRepository.findByOrdreGreaterThan(oldOrdre);
		// updated Ordre
		for (IncentiveRepayment incentiveRepayment : incentiveRepayments) {
			incentiveRepayment.setOrdre(incentiveRepayment.getOrdre() - 1);
			incentiveRepaymentRepository.save(incentiveRepayment);
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				IncentiveRepayment.class.getSimpleName());
	}

}
