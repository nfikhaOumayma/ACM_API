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
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantsIncentive;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.IncentiveSettingConstantRepository;
import com.acm.service.IncentiveSettingConstantService;
import com.acm.utils.dtos.IncentiveSettingConstantDTO;
import com.acm.utils.models.IncentiveSettingConstant;
import com.acm.utils.models.QIncentiveSettingConstant;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveSettingConstantServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveSettingConstantServiceImpl implements IncentiveSettingConstantService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveSettingConstantServiceImpl.class);

	/** The incentive setting constant repository. */
	@Autowired
	private IncentiveSettingConstantRepository incentiveSettingConstantRepository;

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
	 * @see com.acm.service.IncentiveSettingConstantServiceImp#find(java.lang.Long)
	 */
	@Override
	public IncentiveSettingConstantDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find incentiveSettingConstant by ID : {}", id);
		IncentiveSettingConstant incentiveSettingConstant =
				incentiveSettingConstantRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingConstant)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingConstant.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ IncentiveSettingConstant.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(incentiveSettingConstant, IncentiveSettingConstantDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingConstantServiceImp#find(com.acm.utils.dtos.
	 * IncentiveSettingConstantDTO)
	 */
	@Override
	public List<IncentiveSettingConstantDTO> find(
			IncentiveSettingConstantDTO incentiveSettingConstantDto) {

		Preconditions.checkNotNull(incentiveSettingConstantDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// return empty list if no category passed OR the value not in
		// INCENTIVE_SETTING_CONSTANT_CATEGORY list
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getCategory())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CONSTANT_CATEGORY
						.contains(incentiveSettingConstantDto.getCategory())) {
			return new ArrayList<>();
		}

		// init QIncentiveSettingConstant
		QIncentiveSettingConstant qIncentiveSettingConstant =
				QIncentiveSettingConstant.incentiveSettingConstant;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveSettingConstant.enabled.eq(Boolean.TRUE));
		// find by category
		predicate.and(
				qIncentiveSettingConstant.category.eq(incentiveSettingConstantDto.getCategory()));
		Iterable<IncentiveSettingConstant> iterable =
				incentiveSettingConstantRepository.findAll(predicate);
		List<IncentiveSettingConstant> incentiveSettingConstants = new ArrayList<>();
		iterable.forEach(incentiveSettingConstants::add);
		// mapping data
		List<IncentiveSettingConstantDTO> incentiveSettingConstantDTOs = new ArrayList<>();
		incentiveSettingConstants.forEach(incentiveSettingConstant -> incentiveSettingConstantDTOs
				.add(mapper.map(incentiveSettingConstant, IncentiveSettingConstantDTO.class)));
		logger.info("{} : incentiveSettingConstant was founded", incentiveSettingConstants.size());
		return incentiveSettingConstantDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingConstantServiceImp#save(com.acm.utils.dtos.
	 * IncentiveSettingConstantDTO)
	 */
	@Override
	public IncentiveSettingConstantDTO save(
			IncentiveSettingConstantDTO incentiveSettingConstantDto) {

		Preconditions.checkNotNull(incentiveSettingConstantDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getCode())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getDescription())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getCategory())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CONSTANT_CATEGORY
						.contains(incentiveSettingConstantDto.getCategory())) {
			logger.warn("Failed to INSERT new row : check sended DATA");
			return null;
		}
		IncentiveSettingConstant incentiveSettingConstant =
				mapper.map(incentiveSettingConstantDto, IncentiveSettingConstant.class);
		CommonFunctions.mapperToSave(incentiveSettingConstant, userClient, logger);

		IncentiveSettingConstant newIncentiveSettingConstant =
				incentiveSettingConstantRepository.save(incentiveSettingConstant);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				IncentiveSettingConstant.class.getSimpleName());
		return mapper.map(newIncentiveSettingConstant, IncentiveSettingConstantDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingConstantServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.IncentiveSettingConstantDTO)
	 */
	@Override
	public IncentiveSettingConstantDTO save(Long id,
			IncentiveSettingConstantDTO incentiveSettingConstantDto)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(incentiveSettingConstantDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getCode())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getDescription())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingConstantDto.getCategory())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CONSTANT_CATEGORY
						.contains(incentiveSettingConstantDto.getCategory())) {
			logger.warn("Failed to UPDATE : check sended DATA");
			return null;
		}
		logger.info("Update IncentiveSettingConstant  with ID = {}", id);
		IncentiveSettingConstant oldIncentiveSettingConstant =
				incentiveSettingConstantRepository.findById(id).orElse(null);
		// check if object is null
		if (oldIncentiveSettingConstant == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingConstant.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND
							+ IncentiveSettingConstant.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldIncentiveSettingConstant)
		mapper.map(incentiveSettingConstantDto, oldIncentiveSettingConstant);
		CommonFunctions.mapperToUpdate(oldIncentiveSettingConstant, userClient, logger);
		IncentiveSettingConstant newIncentiveSettingConstant =
				incentiveSettingConstantRepository.save(oldIncentiveSettingConstant);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveSettingConstant.class.getSimpleName());
		return mapper.map(newIncentiveSettingConstant, IncentiveSettingConstantDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingConstantService#findByCategories(java.util.List)
	 */
	@Override
	public List<IncentiveSettingConstantDTO> findByCategories(List<String> categories) {

		// init list
		List<IncentiveSettingConstantDTO> incentiveSettingConstantDTOs = new ArrayList<>();
		// check if categories not empty
		if (ACMValidationUtils.isNullOrEmpty(categories)) {
			return incentiveSettingConstantDTOs;
		}
		else {
			// find incentiveSettingsConstants by categories
			List<IncentiveSettingConstant> incentiveSettingConstants =
					incentiveSettingConstantRepository.findByCategoryInAndEnabled(categories,
							Boolean.TRUE);
			// mapping data
			incentiveSettingConstants
					.forEach(incentiveSettingConstant -> incentiveSettingConstantDTOs.add(mapper
							.map(incentiveSettingConstant, IncentiveSettingConstantDTO.class)));
			logger.info("Returning founded data NB: {} ...", incentiveSettingConstantDTOs.size());
			return incentiveSettingConstantDTOs;
		}
	}

}
