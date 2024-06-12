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
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.IncentiveSettingBranchProdLevelRepository;
import com.acm.service.IncentiveSettingBranchProdLevelService;
import com.acm.utils.dtos.IncentiveSettingBranchProdLevelDTO;
import com.acm.utils.models.IncentiveSettingBranchProdLevel;
import com.acm.utils.models.QIncentiveSettingBranchProdLevel;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveSettingBranchProdLevelServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveSettingBranchProdLevelServiceImpl
		implements IncentiveSettingBranchProdLevelService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveSettingBranchProdLevelServiceImpl.class);

	/** The incentive setting branch prod level repository. */
	@Autowired
	private IncentiveSettingBranchProdLevelRepository incentiveSettingBranchProdLevelRepository;

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
	 * @see com.acm.service.IncentiveSettingBranchProdLevelServiceImp#find(java.lang.Long)
	 */
	@Override
	public IncentiveSettingBranchProdLevelDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find incentiveSettingBranchProdLevel by ID : {}", id);
		IncentiveSettingBranchProdLevel incentiveSettingBranchProdLevel =
				incentiveSettingBranchProdLevelRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingBranchProdLevel)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingBranchProdLevel.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ IncentiveSettingBranchProdLevel.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(incentiveSettingBranchProdLevel,
				IncentiveSettingBranchProdLevelDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingBranchProdLevelServiceImp#find(com.acm.utils.dtos.
	 * IncentiveSettingBranchProdLevelDTO)
	 */
	@Override
	public List<IncentiveSettingBranchProdLevelDTO> find(
			IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO) {

		Preconditions.checkNotNull(incentiveSettingBranchProdLevelDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QIncentiveSettingBranchProdLevel
		QIncentiveSettingBranchProdLevel qIncentiveSettingBranchProdLevel =
				QIncentiveSettingBranchProdLevel.incentiveSettingBranchProdLevel;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveSettingBranchProdLevel.enabled.eq(Boolean.TRUE));
		// find by product ID
		if (!ACMValidationUtils.isNullOrEmpty(incentiveSettingBranchProdLevelDTO.getProductId())) {
			predicate.and(qIncentiveSettingBranchProdLevel.productId
					.eq(incentiveSettingBranchProdLevelDTO.getProductId()));
		}
		Iterable<IncentiveSettingBranchProdLevel> iterable =
				incentiveSettingBranchProdLevelRepository.findAll(predicate);
		List<IncentiveSettingBranchProdLevel> incentiveSettingBranchProdLevels = new ArrayList<>();
		iterable.forEach(incentiveSettingBranchProdLevels::add);
		// mapping data
		List<IncentiveSettingBranchProdLevelDTO> incentiveSettingBranchProdLevelDTOs =
				new ArrayList<>();
		incentiveSettingBranchProdLevels
				.forEach(incentiveSettingBranchProdLevel -> incentiveSettingBranchProdLevelDTOs
						.add(mapper.map(incentiveSettingBranchProdLevel,
								IncentiveSettingBranchProdLevelDTO.class)));
		logger.info("{} : incentiveSettingBranchProdLevel was founded",
				incentiveSettingBranchProdLevels.size());
		return incentiveSettingBranchProdLevelDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingBranchProdLevelServiceImp#save(com.acm.utils.dtos.
	 * IncentiveSettingBranchProdLevelDTO)
	 */
	@Override
	public IncentiveSettingBranchProdLevelDTO save(
			IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO) {

		Preconditions.checkNotNull(incentiveSettingBranchProdLevelDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		IncentiveSettingBranchProdLevel incentiveSettingBranchProdLevel = mapper
				.map(incentiveSettingBranchProdLevelDTO, IncentiveSettingBranchProdLevel.class);
		CommonFunctions.mapperToSave(incentiveSettingBranchProdLevel, userClient, logger);

		IncentiveSettingBranchProdLevel newIncentiveSettingBranchProdLevel =
				incentiveSettingBranchProdLevelRepository.save(incentiveSettingBranchProdLevel);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				IncentiveSettingBranchProdLevel.class.getSimpleName());
		return mapper.map(newIncentiveSettingBranchProdLevel,
				IncentiveSettingBranchProdLevelDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingBranchProdLevelServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.IncentiveSettingBranchProdLevelDTO)
	 */
	@Override
	public IncentiveSettingBranchProdLevelDTO save(Long id,
			IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(incentiveSettingBranchProdLevelDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update IncentiveSettingBranchProdLevel  with ID = {}", id);
		IncentiveSettingBranchProdLevel oldIncentiveSettingBranchProdLevel =
				incentiveSettingBranchProdLevelRepository.findById(id).orElse(null);

		// check if object is null
		if (oldIncentiveSettingBranchProdLevel == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingBranchProdLevel.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND
							+ IncentiveSettingBranchProdLevel.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldIncentiveSettingBranchProdLevel)
		mapper.map(incentiveSettingBranchProdLevelDTO, oldIncentiveSettingBranchProdLevel);
		CommonFunctions.mapperToUpdate(oldIncentiveSettingBranchProdLevel, userClient, logger);
		IncentiveSettingBranchProdLevel newIncentiveSettingBranchProdLevel =
				incentiveSettingBranchProdLevelRepository.save(oldIncentiveSettingBranchProdLevel);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveSettingBranchProdLevel.class.getSimpleName());
		return mapper.map(newIncentiveSettingBranchProdLevel,
				IncentiveSettingBranchProdLevelDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingBranchProdLevelService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Incentive branch prod level Settinge with ID = {}", id);
		// delete old Incentive Branch Prod Level Type
		incentiveSettingBranchProdLevelRepository.deleteById(id);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				IncentiveSettingBranchProdLevel.class.getSimpleName());

	}

}
