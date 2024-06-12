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
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantsIncentive;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.IncentiveSettingRepository;
import com.acm.service.IncentiveSettingService;
import com.acm.utils.dtos.IncentiveSettingDTO;
import com.acm.utils.models.IncentiveSetting;
import com.acm.utils.models.QIncentiveSetting;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link IncentiveSettingServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class IncentiveSettingServiceImpl implements IncentiveSettingService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(IncentiveSettingServiceImpl.class);

	/** The incentive setting repository. */
	@Autowired
	private IncentiveSettingRepository incentiveSettingRepository;

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
	 * @see com.acm.service.IncentiveSettingServiceImp#find(java.lang.Long)
	 */
	@Override
	public IncentiveSettingDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find incentiveSetting by ID : {}", id);
		IncentiveSetting incentiveSetting = incentiveSettingRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(incentiveSetting)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSetting.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ IncentiveSetting.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(incentiveSetting, IncentiveSettingDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingServiceImp#find(com.acm.utils.dtos.IncentiveSettingDTO)
	 */
	@Override
	public List<IncentiveSettingDTO> find(IncentiveSettingDTO incentiveSettingDTO) {

		Preconditions.checkNotNull(incentiveSettingDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// return empty if no category passed OR the value not in INCENTIVE_SETTING_CATEGORY list
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getCategory())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CATEGORY
						.contains(incentiveSettingDTO.getCategory())) {
			return new ArrayList<>();
		}
		// init QIncentiveSetting
		QIncentiveSetting qIncentiveSetting = QIncentiveSetting.incentiveSetting;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qIncentiveSetting.enabled.eq(Boolean.TRUE));
		// find by category
		predicate.and(qIncentiveSetting.category.eq(incentiveSettingDTO.getCategory()));
		// find by product ID
		if (!ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getProductId())) {
			predicate.and(qIncentiveSetting.productId.eq(incentiveSettingDTO.getProductId()));
		}
		Iterable<IncentiveSetting> iterable =
				incentiveSettingRepository.findAll(predicate, Sort.by(Direction.ASC, "ordre"));
		List<IncentiveSetting> incentiveSettings = new ArrayList<>();
		iterable.forEach(incentiveSettings::add);
		// mapping data
		List<IncentiveSettingDTO> incentiveSettingDTOs = new ArrayList<>();
		incentiveSettings.forEach(incentiveSetting -> incentiveSettingDTOs
				.add(mapper.map(incentiveSetting, IncentiveSettingDTO.class)));
		logger.info("{} : incentiveSetting was founded", incentiveSettings.size());
		return incentiveSettingDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingServiceImp#save(com.acm.utils.dtos.IncentiveSettingDTO)
	 */
	@Override
	public IncentiveSettingDTO save(IncentiveSettingDTO incentiveSettingDTO) {

		Preconditions.checkNotNull(incentiveSettingDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// CHECK DATA
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getProductId())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getFrequency())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getCategory())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CATEGORY
						.contains(incentiveSettingDTO.getCategory())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getFrom())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getTo())) {
			logger.warn("Failed to INSERT new row : check sended DATA");
			return null;
		}
		IncentiveSetting incentiveSetting = mapper.map(incentiveSettingDTO, IncentiveSetting.class);
		CommonFunctions.mapperToSave(incentiveSetting, userClient, logger);

		IncentiveSetting newIncentiveSetting = incentiveSettingRepository.save(incentiveSetting);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, IncentiveSetting.class.getSimpleName());
		return mapper.map(newIncentiveSetting, IncentiveSettingDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.IncentiveSettingDTO)
	 */
	@Override
	public IncentiveSettingDTO save(Long id, IncentiveSettingDTO incentiveSettingDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(incentiveSettingDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// CHECK DATA
		if (ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getProductId())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getFrequency())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getCategory())
				|| !CommonConstantsIncentive.INCENTIVE_SETTING_CATEGORY
						.contains(incentiveSettingDTO.getCategory())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getFrom())
				|| ACMValidationUtils.isNullOrEmpty(incentiveSettingDTO.getTo())) {
			logger.warn("Failed to UPDATE row : check sended DATA");
			return null;
		}
		logger.info("Update IncentiveSetting  with ID = {}", id);
		IncentiveSetting oldIncentiveSetting = incentiveSettingRepository.findById(id).orElse(null);

		// check if object is null
		if (oldIncentiveSetting == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					IncentiveSetting.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + IncentiveSetting.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldIncentiveSetting)
		mapper.map(incentiveSettingDTO, oldIncentiveSetting);
		CommonFunctions.mapperToUpdate(oldIncentiveSetting, userClient, logger);
		IncentiveSetting newIncentiveSetting = incentiveSettingRepository.save(oldIncentiveSetting);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, IncentiveSetting.class.getSimpleName());
		return mapper.map(newIncentiveSetting, IncentiveSettingDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IncentiveSettingService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Incentive Setting with ID = {}", id);
		// fin Incentive Setting by id
		IncentiveSetting deletedIncentiveSetting =
				incentiveSettingRepository.findById(id).orElse(null);
		// update ordre after deleteing an IncentiveSetting
		Long oldOrdre = deletedIncentiveSetting != null ? deletedIncentiveSetting.getOrdre() : 0L;
		// delete old Incentive Setting Type
		incentiveSettingRepository.deleteById(id);
		// find IncentiveSetting by ordre greater than the ordre of deleted Incentive Setting
		List<IncentiveSetting> incentiveSettings =
				incentiveSettingRepository.findByOrdreGreaterThan(oldOrdre);
		// updated Ordre
		for (IncentiveSetting incentiveSetting : incentiveSettings) {
			incentiveSetting.setOrdre(incentiveSetting.getOrdre() - 1);
			incentiveSettingRepository.save(incentiveSetting);
		}
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, IncentiveSetting.class.getSimpleName());

	}

}
