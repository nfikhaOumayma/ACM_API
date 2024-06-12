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
import com.acm.repository.SettingGurantorCollateralRepository;
import com.acm.service.SettingGurantorCollateralService;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.models.QSettingGurantorCollateral;
import com.acm.utils.models.SettingGurantorCollateral;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingGurantorCollateralServiceImpl}.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
@Service
public class SettingGurantorCollateralServiceImpl implements SettingGurantorCollateralService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingGurantorCollateralServiceImpl.class);

	/** The acmEnvironnement repository. */
	@Autowired
	private SettingGurantorCollateralRepository settingGurantorCollateralRepository;

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
	 * @see com.acm.service.SettingGurantorCollateralService#find(com.acm.utils.dtos.
	 * SettingGurantorCollateralDTO)
	 */
	@Override
	public List<SettingGurantorCollateralDTO> find(
			SettingGurantorCollateralDTO settingGurantorCollateralDTO) {

		// init QSettingGurantorCollateral
		QSettingGurantorCollateral qSettingGurantorCollateral =
				QSettingGurantorCollateral.settingGurantorCollateral;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingGurantorCollateral.enabled.eq(Boolean.TRUE));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTO.getId())) {
			predicate.and(qSettingGurantorCollateral.id.eq(settingGurantorCollateralDTO.getId()));
		}

		// find by code -- GUARANTOR || -- COLLATERAL.
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTO.getCode())) {
			predicate.and(
					qSettingGurantorCollateral.code.eq(settingGurantorCollateralDTO.getCode()));
		}

		// find by id product
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTO.getProductId())) {
			predicate.and(qSettingGurantorCollateral.productId
					.eq(settingGurantorCollateralDTO.getProductId()));
		}

		// find by mandatory setting
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTO.getMandatory())) {
			predicate.and(qSettingGurantorCollateral.mandatory
					.eq(settingGurantorCollateralDTO.getMandatory()));
		}

		// QueryDSL using springDATA
		Iterable<SettingGurantorCollateral> iterable =
				settingGurantorCollateralRepository.findAll(predicate);
		List<SettingGurantorCollateral> settingGurantorCollaterals = new ArrayList<>();
		iterable.forEach(settingGurantorCollaterals::add);
		logger.info("{} : Setting Gurantor Collateral was founded",
				settingGurantorCollaterals.size());

		// mapping returned list
		List<SettingGurantorCollateralDTO> settingGurantorCollateralDTOs = new ArrayList<>();
		settingGurantorCollaterals
				.forEach(settingGurantorCollateral -> settingGurantorCollateralDTOs.add(
						mapper.map(settingGurantorCollateral, SettingGurantorCollateralDTO.class)));

		logger.info("Returning founded data ...");
		return settingGurantorCollateralDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingGurantorCollateralService#save(com.acm.utils.dtos.
	 * SettingGurantorCollateralDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingGurantorCollateralDTO save(
			SettingGurantorCollateralDTO settingGurantorCollateralDTO) {

		Preconditions.checkNotNull(settingGurantorCollateralDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingGurantorCollateral settingGurantorCollateral =
				mapper.map(settingGurantorCollateralDTO, SettingGurantorCollateral.class);

		CommonFunctions.mapperToSave(settingGurantorCollateral, userClient, logger);
		SettingGurantorCollateral newSettingGurantorCollateral =
				settingGurantorCollateralRepository.save(settingGurantorCollateral);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingGurantorCollateral.class.getSimpleName());
		return mapper.map(newSettingGurantorCollateral, SettingGurantorCollateralDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingGurantorCollateralService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingGurantorCollateralDTO)
	 */
	@Override
	public SettingGurantorCollateralDTO save(Long id,
			SettingGurantorCollateralDTO settingGurantorCollateralDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingGurantorCollateralDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingGurantorCollateral with ID = {}", id);
		SettingGurantorCollateral oldSettingGurantorCollateral =
				settingGurantorCollateralRepository.findById(id).orElse(null);
		// check if object is null
		if (oldSettingGurantorCollateral == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingGurantorCollateral.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND
							+ SettingGurantorCollateral.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingGurantorCollateral.class),
				CommonAOPConstants.UPDATE, id,
				CommonFunctions.convertObjectToJSONString(oldSettingGurantorCollateral));

		// mapping new data with existing data (oldSettingGurantorCollateral)
		mapper.map(settingGurantorCollateralDTO, oldSettingGurantorCollateral);
		CommonFunctions.mapperToUpdate(oldSettingGurantorCollateral, userClient, logger);

		// update & persist data in DB
		SettingGurantorCollateral newSettingGurantorCollateral =
				settingGurantorCollateralRepository.save(oldSettingGurantorCollateral);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingGurantorCollateral.getUpdatedBy());
		settingHistoriqueDTO.setNewData(
				CommonFunctions.convertObjectToJSONString(newSettingGurantorCollateral));
		settingHistoriqueService.save(settingHistoriqueDTO);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingGurantorCollateral.class.getSimpleName());
		return mapper.map(newSettingGurantorCollateral, SettingGurantorCollateralDTO.class);
	}
}
