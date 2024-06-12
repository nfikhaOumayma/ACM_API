/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

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
import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingMotifRejetsRepository;
import com.acm.service.SettingHistoriqueService;
import com.acm.service.SettingMotifRejetsService;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.models.QSettingMotifRejets;
import com.acm.utils.models.SettingMotifRejets;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingMotifRejetsServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Service
public class SettingMotifRejetsServiceImpl implements SettingMotifRejetsService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingMotifRejetsServiceImpl.class);

	/** The settingMotifRejets repository. */
	@Autowired
	private SettingMotifRejetsRepository settingMotifRejetsRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#find(com.acm.utils.dtos.
	 * SettingMotifRejetsDTO)
	 */
	@Override
	public List<SettingMotifRejetsDTO> find(SettingMotifRejetsDTO settingMotifRejetsDTO) {

		// init QSettingMotifRejets
		QSettingMotifRejets qSettingMotifRejets = QSettingMotifRejets.settingMotifRejets;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by enabled
		if (Boolean.TRUE.equals(settingMotifRejetsDTO.getEnabled())) {
			predicate.and(qSettingMotifRejets.enabled.eq(settingMotifRejetsDTO.getEnabled()));
		}

		// find by categorie
		if (!ACMValidationUtils.isNullOrEmpty(settingMotifRejetsDTO.getCategorie())) {
			predicate.and(qSettingMotifRejets.categorie.eq(settingMotifRejetsDTO.getCategorie()));
		}

		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(settingMotifRejetsDTO.getId())) {
			predicate.and(qSettingMotifRejets.id.eq(settingMotifRejetsDTO.getId()));
		}

		// QueryDSL using springDATA
		Iterable<SettingMotifRejets> iterable = settingMotifRejetsRepository.findAll(predicate);
		List<SettingMotifRejets> settingMotifRejetss = new ArrayList<>();
		iterable.forEach(settingMotifRejetss::add);
		logger.info("{} : setting Motif Rejets was founded", settingMotifRejetss.size());

		// mapping returned list
		List<SettingMotifRejetsDTO> settingMotifRejetsDTOs = new ArrayList<>();
		settingMotifRejetss.forEach(settingMotifRejets -> settingMotifRejetsDTOs
				.add(mapper.map(settingMotifRejets, SettingMotifRejetsDTO.class)));

		logger.info("Returning founded data ...");
		return settingMotifRejetsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#save(com.acm.utils.dtos.SettingMotifRejetsDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingMotifRejetsDTO save(SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws CodeSettingExistException {

		Preconditions.checkNotNull(settingMotifRejetsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingMotifRejets settingMotifRejets =
				mapper.map(settingMotifRejetsDTO, SettingMotifRejets.class);

		// check code setting if exist
		checkCodeSetting(settingMotifRejetsDTO);

		CommonFunctions.mapperToSave(settingMotifRejets, userClient, logger);
		SettingMotifRejets newSettingMotifRejets =
				settingMotifRejetsRepository.save(settingMotifRejets);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingMotifRejets.class.getSimpleName());
		return mapper.map(newSettingMotifRejets, SettingMotifRejetsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingMotifRejetsDTO)
	 */
	@Override
	public SettingMotifRejetsDTO save(Long id, SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws ResourcesNotFoundException, CodeSettingExistException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingMotifRejetsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingMotifRejets with ID = {}", id);
		SettingMotifRejets oldSettingMotifRejets =
				settingMotifRejetsRepository.findById(id).orElse(null);
		// check if object is null
		if (oldSettingMotifRejets == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingMotifRejets.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingMotifRejets.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingMotifRejets.class),
				CommonAOPConstants.UPDATE, id,
				CommonFunctions.convertObjectToJSONString(oldSettingMotifRejets));

		// check code setting if exist
		if (Boolean.compare(oldSettingMotifRejets.getEnabled(),
				settingMotifRejetsDTO.getEnabled()) == 0) {
			checkCodeSetting(settingMotifRejetsDTO);
		}

		// mapping new data with existing data (oldSettingMotifRejets)
		mapper.map(settingMotifRejetsDTO, oldSettingMotifRejets);
		CommonFunctions.mapperToUpdate(oldSettingMotifRejets, userClient, logger);

		// update & persist data in DB
		SettingMotifRejets newSettingMotifRejets =
				settingMotifRejetsRepository.save(oldSettingMotifRejets);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingMotifRejets.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newSettingMotifRejets));
		settingHistoriqueService.save(settingHistoriqueDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingMotifRejets.class.getSimpleName());
		return mapper.map(newSettingMotifRejets, SettingMotifRejetsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#find()
	 */
	@Override
	public List<SettingMotifRejetsDTO> find() {

		List<SettingMotifRejets> settingMotifRejetss = settingMotifRejetsRepository.findAll();
		List<SettingMotifRejetsDTO> settingMotifRejetssDTOs = new ArrayList<>();
		settingMotifRejetss.forEach(settingMotifRejets -> settingMotifRejetssDTOs
				.add(mapper.map(settingMotifRejets, SettingMotifRejetsDTO.class)));
		return settingMotifRejetssDTOs;
	}

	/**
	 * Check code setting if exist.
	 *
	 * @author YesserSomai
	 * @param settingMotifRejetsDTO the setting motif rejets DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	private void checkCodeSetting(SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws CodeSettingExistException {

		List<SettingMotifRejetsDTO> settingMotifRejetsDTOs = find();
		settingMotifRejetsDTOs.removeIf(m -> m.getId().equals(settingMotifRejetsDTO.getId()));
		settingMotifRejetsDTOs =
				settingMotifRejetsDTOs.stream()
						.filter(settingDocumentTypeDTO -> settingDocumentTypeDTO.getCode()
								.equals(settingMotifRejetsDTO.getCode()))
						.collect(Collectors.toList());

		if (!ACMValidationUtils.isNullOrEmpty(settingMotifRejetsDTOs)) {
			logger.error(CommonLoggerMessage.CODE_SETTING_EXIST,
					SettingMotifRejets.class.getSimpleName());
			throw new CodeSettingExistException(CommonExceptionsMessage.CODE_SETTING_EXIST,
					CommonExceptionsMessage.CODE_SETTING_EXIST
							+ SettingMotifRejets.class.getSimpleName());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Motif Rejet Type with ID = {}", id);
		SettingMotifRejets oldSettingMotifRejets =
				settingMotifRejetsRepository.findById(id).orElse(null);

		// check if object is null
		if (oldSettingMotifRejets == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingMotifRejets.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingMotifRejets.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// delete old Motif Rejet type
		settingMotifRejetsRepository.delete(oldSettingMotifRejets);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				SettingMotifRejets.class.getSimpleName());

	}
}
