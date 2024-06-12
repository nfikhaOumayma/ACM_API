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
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.EchelleRiskTypeRepository;
import com.acm.repository.SettingRiskTypeRepository;
import com.acm.service.SettingRiskTypeService;
import com.acm.utils.dtos.SettingTypeRiskDTO;
import com.acm.utils.models.ProductDetails;
import com.acm.utils.models.SettingTypeRisk;
import com.google.common.base.Preconditions;

/**
 * {@link SettingRiskTypeServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class SettingRiskTypeServiceImpl implements SettingRiskTypeService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingRiskTypeServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The setting risk type repository. */
	@Autowired
	private SettingRiskTypeRepository settingRiskTypeRepository;

	/** The echelle risk type repository. */
	@Autowired
	private EchelleRiskTypeRepository echelleRiskTypeRepository;

	/** The setting type risk update. */
	SettingTypeRisk settingTypeRiskUpdate;

	/** The setting type risk. */
	SettingTypeRisk settingTypeRisk;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRiskTypeService#find()
	 */
	@Override
	public List<SettingTypeRiskDTO> find() {

		List<SettingTypeRisk> settingTypeRisks = settingRiskTypeRepository.findAll();
		List<SettingTypeRiskDTO> settingTypeRiskDTOs = new ArrayList<>();
		settingTypeRisks.forEach(settingTypeRisk -> settingTypeRiskDTOs
				.add(mapper.map(settingTypeRisk, SettingTypeRiskDTO.class)));
		return settingTypeRiskDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRiskTypeService#save(com.acm.utils.dtos.SettingTypeRiskDTO)
	 */
	@Override
	public SettingTypeRiskDTO save(SettingTypeRiskDTO settingTypeRiskDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(settingTypeRiskDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		settingTypeRisk = mapper.map(settingTypeRiskDTO, SettingTypeRisk.class);

		settingTypeRisk.getEchelleTypeRisks().forEach(item -> {
			item.setSettingTypeRisk(settingTypeRisk);
		});
		CommonFunctions.mapperToSave(settingTypeRisk, userClient, logger);
		settingTypeRisk = settingRiskTypeRepository.save(settingTypeRisk);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ProductDetails.class.getSimpleName());
		settingTypeRiskDTO = mapper.map(settingTypeRisk, SettingTypeRiskDTO.class);

		return mapper.map(settingTypeRisk, SettingTypeRiskDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRiskTypeService#update(com.acm.utils.dtos.SettingTypeRiskDTO)
	 */
	@Override
	public SettingTypeRiskDTO update(SettingTypeRiskDTO settingTypeRiskDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(settingTypeRiskDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		settingTypeRiskUpdate = mapper.map(settingTypeRiskDTO, SettingTypeRisk.class);
		echelleRiskTypeRepository.deleteBySettingTypeRisk(settingTypeRiskUpdate);
		settingTypeRiskUpdate.getEchelleTypeRisks().forEach(item -> {
			item.setSettingTypeRisk(settingTypeRiskUpdate);
		});
		CommonFunctions.mapperToUpdate(settingTypeRiskUpdate, userClient, logger);
		settingTypeRiskUpdate = settingRiskTypeRepository.save(settingTypeRiskUpdate);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ProductDetails.class.getSimpleName());
		settingTypeRiskDTO = mapper.map(settingTypeRiskUpdate, SettingTypeRiskDTO.class);

		return settingTypeRiskDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingRiskTypeService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long idRiskType) {

		this.settingRiskTypeRepository.deleteById(idRiskType);

	}

}
