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

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingRequiredStepRepository;
import com.acm.service.SettingHistoriqueService;
import com.acm.service.SettingRequiredStepService;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.models.QSettingRequiredStep;
import com.acm.utils.models.SettingRequiredStep;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingRequiredStepServiceImpl}.
 *
 * @author YesserSomai
 * @since 1.0.3
 */
@Service
public class SettingRequiredStepServiceImpl implements SettingRequiredStepService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingRequiredStepServiceImpl.class);

	/** The acmEnvironnement repository. */
	@Autowired
	private SettingRequiredStepRepository settingRequiredStepRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRequiredStepService#find(com.acm.utils.dtos.
	 * SettingRequiredStepDTO)
	 */
	@Override
	public List<SettingRequiredStepDTO> find(SettingRequiredStepDTO settingRequiredStepDTO) {

		// init QSettingRequiredStep
		QSettingRequiredStep qSettingRequiredStep = QSettingRequiredStep.settingRequiredStep;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingRequiredStep.enabled.eq(Boolean.TRUE));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTO.getId())) {
			predicate.and(qSettingRequiredStep.id.eq(settingRequiredStepDTO.getId()));
		}

		// find by code FIELD VISIT / AUDIT REVIEW / RISK REVIEW
		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTO.getCode())) {
			predicate.and(qSettingRequiredStep.code.eq(settingRequiredStepDTO.getCode()));
		}

		// find by id product
		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTO.getProductId())) {
			predicate.and(qSettingRequiredStep.productId.eq(settingRequiredStepDTO.getProductId()));
		}

		// find by mandatory setting
		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTO.getMandatory())) {
			predicate.and(qSettingRequiredStep.mandatory.eq(settingRequiredStepDTO.getMandatory()));
		}

		// QueryDSL using springDATA
		Iterable<SettingRequiredStep> iterable = settingRequiredStepRepository.findAll(predicate);
		List<SettingRequiredStep> settingRequiredSteps = new ArrayList<>();
		iterable.forEach(settingRequiredSteps::add);
		logger.info("{} : Setting Gurantor Collateral was founded", settingRequiredSteps.size());

		// mapping returned list
		List<SettingRequiredStepDTO> settingRequiredStepDTOs = new ArrayList<>();
		settingRequiredSteps.forEach(settingRequiredStep -> settingRequiredStepDTOs
				.add(mapper.map(settingRequiredStep, SettingRequiredStepDTO.class)));

		logger.info("Returning founded data ...");
		return settingRequiredStepDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRequiredStepService#save(com.acm.utils.dtos.
	 * SettingRequiredStepDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingRequiredStepDTO save(SettingRequiredStepDTO settingRequiredStepDTO) {

		Preconditions.checkNotNull(settingRequiredStepDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingRequiredStep settingRequiredStep =
				mapper.map(settingRequiredStepDTO, SettingRequiredStep.class);

		CommonFunctions.mapperToSave(settingRequiredStep, userClient, logger);
		SettingRequiredStep newSettingRequiredStep =
				settingRequiredStepRepository.save(settingRequiredStep);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingRequiredStep.class.getSimpleName());
		return mapper.map(newSettingRequiredStep, SettingRequiredStepDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRequiredStepService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingRequiredStepDTO)
	 */
	@Override
	public SettingRequiredStepDTO save(Long id, SettingRequiredStepDTO settingRequiredStepDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingRequiredStepDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingRequiredStep with ID = {}", id);
		SettingRequiredStep oldSettingRequiredStep =
				settingRequiredStepRepository.findById(id).orElse(null);
		// check if object is null
		if (oldSettingRequiredStep == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingRequiredStep.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingRequiredStep.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init SettingRequiredStep object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingRequiredStep.class),
				CommonAOPConstants.UPDATE, id,
				CommonFunctions.convertObjectToJSONString(oldSettingRequiredStep));

		// mapping new data with existing data (oldSettingRequiredStep)
		mapper.map(settingRequiredStepDTO, oldSettingRequiredStep);
		CommonFunctions.mapperToUpdate(oldSettingRequiredStep, userClient, logger);

		// update & persist data in DB
		SettingRequiredStep newSettingRequiredStep =
				settingRequiredStepRepository.save(oldSettingRequiredStep);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingRequiredStep.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newSettingRequiredStep));
		settingHistoriqueService.save(settingHistoriqueDTO);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingRequiredStep.class.getSimpleName());
		return mapper.map(newSettingRequiredStep, SettingRequiredStepDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRequiredStepService#find()
	 */
	@Override
	public List<SettingRequiredStepDTO> find() {

		List<SettingRequiredStep> settingRequiredSteps = settingRequiredStepRepository.findAll();
		List<SettingRequiredStepDTO> settingRequiredStepsDTOs = new ArrayList<>();
		settingRequiredSteps.forEach(settingRequiredStep -> settingRequiredStepsDTOs
				.add(mapper.map(settingRequiredStep, SettingRequiredStepDTO.class)));
		return settingRequiredStepsDTOs;
	}
}
