/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingHistoriqueRepository;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.models.QSettingHistorique;
import com.acm.utils.models.SettingHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingHistoriqueServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Service
public class SettingHistoriqueServiceImpl implements SettingHistoriqueService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingHistoriqueServiceImpl.class);

	/** The settingHistorique repository. */
	@Autowired
	private SettingHistoriqueRepository settingHistoriqueRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingHistoriqueService#find(java.lang.Long)
	 */
	@Override
	public SettingHistoriqueDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find SettingHistorique by ID : {}", id);
		SettingHistorique settingHistorique = settingHistoriqueRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingHistorique)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingHistorique.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(settingHistorique, SettingHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingHistoriqueService#save(com.acm.utils.dtos.SettingHistoriqueDTO)
	 */
	@Override
	public SettingHistoriqueDTO save(SettingHistoriqueDTO settingHistoriqueDTO) {

		Preconditions.checkNotNull(settingHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingHistorique settingHistorique =
				mapper.map(settingHistoriqueDTO, SettingHistorique.class);
		// setting date update
		settingHistorique.setDateUpdate(new Date());
		SettingHistorique newSettingHistorique =
				settingHistoriqueRepository.save(settingHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, SettingHistorique.class.getSimpleName());
		return mapper.map(newSettingHistorique, SettingHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingHistoriqueService#find(com.acm.utils.dtos.SettingHistoriqueDTO)
	 */
	@Override
	public List<SettingHistoriqueDTO> find(SettingHistoriqueDTO settingHistoriqueDTO) {

		// init QSettingHistorique
		QSettingHistorique qSettingHistorique = QSettingHistorique.settingHistorique;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by table name
		if (!ACMValidationUtils.isNullOrEmpty(settingHistoriqueDTO.getTableName())) {
			predicate.and(qSettingHistorique.tableName.eq(settingHistoriqueDTO.getTableName()));
		}
		// QueryDSL using springDATA
		Iterable<SettingHistorique> iterable = settingHistoriqueRepository.findAll(predicate);
		List<SettingHistorique> settingHistoriques = new ArrayList<>();
		iterable.forEach(settingHistoriques::add);
		logger.info("{} : setting Historique was founded", settingHistoriques.size());

		// mapping returned list
		List<SettingHistoriqueDTO> settingHistoriqueDTOs = new ArrayList<>();
		settingHistoriques.forEach(settingHistorique -> settingHistoriqueDTOs
				.add(mapper.map(settingHistorique, SettingHistoriqueDTO.class)));

		logger.info("Returning founded data ...");
		return settingHistoriqueDTOs;
	}
}
