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
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantsIncentive;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.IncentiveSettingRunRepository;
import com.acm.service.IncentiveSettingRunService;
import com.acm.utils.dtos.IncentiveSettingRunDTO;
import com.acm.utils.models.IncentiveSettingRun;
import com.acm.utils.models.QIncentiveSettingRun;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveSettingRunServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveSettingRunServiceImpl implements IncentiveSettingRunService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(IncentiveSettingRunServiceImpl.class);

	/** The incentive setting run repository. */
	@Autowired
	private IncentiveSettingRunRepository incentiveSettingRunRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingRunServiceImp#find(com.acm.utils.dtos.
	 * IncentiveSettingRunDTO)
	 */
	@Override
	public List<IncentiveSettingRunDTO> find(IncentiveSettingRunDTO incentiveSettingRunDto) {

		Preconditions.checkNotNull(incentiveSettingRunDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QIncentiveSettingRun
		QIncentiveSettingRun qIncentiveSettingRun = QIncentiveSettingRun.incentiveSettingRun;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveSettingRun.enabled.eq(Boolean.TRUE));

		Iterable<IncentiveSettingRun> iterable = incentiveSettingRunRepository.findAll(predicate);
		List<IncentiveSettingRun> incentiveSettingRuns = new ArrayList<>();
		iterable.forEach(incentiveSettingRuns::add);
		// mapping data
		List<IncentiveSettingRunDTO> incentiveSettingRunDTOs = new ArrayList<>();
		incentiveSettingRuns.forEach(incentiveSettingRun -> incentiveSettingRunDTOs
				.add(mapper.map(incentiveSettingRun, IncentiveSettingRunDTO.class)));
		logger.info("{} : incentiveSettingRun was founded", incentiveSettingRuns.size());
		return incentiveSettingRunDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingRunService#save(
	 * com.acm.utils.dtos.IncentiveSettingRunDTO)
	 */
	@Override
	public IncentiveSettingRunDTO updateStatus(IncentiveSettingRunDTO incentiveSettingRunDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(incentiveSettingRunDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Status IncentiveSettingRun with CODE = {}",
				incentiveSettingRunDTO.getCode());
		// return null if enabled is null or no category passed OR the value not in
		// INCENTIVE_SETTING_CONSTANT_CATEGORY list
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingRunDTO.getEnabled())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingRunDTO.getCode())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CONSTANT_CATEGORY
						.contains(incentiveSettingRunDTO.getCode())) {
			logger.error("Check recived parms");
			return null;
		}
		// find row by given code
		List<IncentiveSettingRun> incentiveSettingRuns =
				incentiveSettingRunRepository.findByCode(incentiveSettingRunDTO.getCode());
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingRuns)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingRun.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveSettingRun.class.getSimpleName()
							+ "with CODE = " + incentiveSettingRunDTO.getCode());
		}
		// get object from DB
		IncentiveSettingRun oldIncentiveSettingRun = incentiveSettingRuns.get(0);
		// setting new status
		oldIncentiveSettingRun.setEnabled(incentiveSettingRunDTO.getEnabled());
		CommonFunctions.mapperToUpdate(oldIncentiveSettingRun, userClient, logger);
		IncentiveSettingRun newIncentiveSettingRun =
				incentiveSettingRunRepository.save(oldIncentiveSettingRun);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveSettingRun.class.getSimpleName());
		return mapper.map(newIncentiveSettingRun, IncentiveSettingRunDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingRunService#findByCode(java.lang.String)
	 */
	@Override
	public IncentiveSettingRunDTO findByCode(String code) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(code, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("find IncentiveSettingRun with CODE = {}", code);
		// find row by given code
		List<IncentiveSettingRun> incentiveSettingRuns =
				incentiveSettingRunRepository.findByCode(code);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingRuns)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingRun.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveSettingRun.class.getSimpleName()
							+ "with CODE = " + code);
		}
		return mapper.map(incentiveSettingRuns.get(0), IncentiveSettingRunDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.IncentiveSettingRunService#updateApplyDiscountOrBranch(com.acm.utils.dtos.
	 * IncentiveSettingRunDTO)
	 */
	@Override
	public IncentiveSettingRunDTO updateApplyDiscountOrBranch(
			IncentiveSettingRunDTO incentiveSettingRunDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(incentiveSettingRunDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Status IncentiveSettingRun with CODE = {}",
				incentiveSettingRunDTO.getCode());
		// return null if enabled is null or no category passed OR the value not in
		// INCENTIVE_SETTING_CONSTANT_CATEGORY list
		if ((ACMValidationUtils.isNullOrEmpty(incentiveSettingRunDTO.getApplayDiscountRule())
				&& ACMValidationUtils
						.isNullOrEmpty(incentiveSettingRunDTO.getAppalyBranchProdLevel()))
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingRunDTO.getCode())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CONSTANT_CATEGORY
						.contains(incentiveSettingRunDTO.getCode())) {
			logger.error("Check recived parms");
			return null;
		}
		// find row by given code
		List<IncentiveSettingRun> incentiveSettingRuns =
				incentiveSettingRunRepository.findByCode(incentiveSettingRunDTO.getCode());
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingRuns)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSettingRun.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveSettingRun.class.getSimpleName()
							+ "with CODE = " + incentiveSettingRunDTO.getCode());
		}
		// get object from DB
		IncentiveSettingRun oldIncentiveSettingRun = incentiveSettingRuns.get(0);
		// setting new apply discount if is not empty
		if (!ACMValidationUtils.isNullOrEmpty(incentiveSettingRunDTO.getApplayDiscountRule())) {
			oldIncentiveSettingRun
					.setApplayDiscountRule(incentiveSettingRunDTO.getApplayDiscountRule());
		}
		// setting new apply branch if is not empty
		if (!ACMValidationUtils.isNullOrEmpty(incentiveSettingRunDTO.getAppalyBranchProdLevel())) {
			oldIncentiveSettingRun
					.setAppalyBranchProdLevel(incentiveSettingRunDTO.getAppalyBranchProdLevel());
		}
		CommonFunctions.mapperToUpdate(oldIncentiveSettingRun, userClient, logger);
		IncentiveSettingRun newIncentiveSettingRun =
				incentiveSettingRunRepository.save(oldIncentiveSettingRun);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				IncentiveSettingRun.class.getSimpleName());
		return mapper.map(newIncentiveSettingRun, IncentiveSettingRunDTO.class);
	}

}
