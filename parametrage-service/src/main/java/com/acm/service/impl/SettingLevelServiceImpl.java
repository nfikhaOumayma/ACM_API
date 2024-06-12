/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingLevelRepository;
import com.acm.service.SettingHistoriqueService;
import com.acm.service.SettingLevelProcessService;
import com.acm.service.SettingLevelService;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.models.QSettingLevel;
import com.acm.utils.models.ReportVisit;
import com.acm.utils.models.SettingLevel;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingLevelServiceImpl}.
 *
 * @author YesserSOmai
 * @since 0.3.0
 */
@Service
public class SettingLevelServiceImpl implements SettingLevelService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingLevelServiceImpl.class);

	/** The acmEnvironnement repository. */
	@Autowired
	private SettingLevelRepository settingLevelRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The settingLevelProcess Service. */
	@Autowired
	private SettingLevelProcessService settingLevelProcessService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelService#find(java.lang.Long)
	 */
	@Override
	public SettingLevelDTO find(Long idSettingLevel) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idSettingLevel, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find SettingLevel by ID : {}", idSettingLevel);
		SettingLevel settingLevel = settingLevelRepository.findById(idSettingLevel).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingLevel)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ReportVisit.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingLevel.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ idSettingLevel);
		}
		return mapper.map(settingLevel, SettingLevelDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelService#find(com.acm.utils.dtos.SettingLevelDTO)
	 */
	@Override
	public List<SettingLevelDTO> find(SettingLevelDTO settingLevelDTO) {

		// init QSettingLevel
		QSettingLevel qSettingLevel = QSettingLevel.settingLevel;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingLevel.enabled.eq(Boolean.TRUE));

		// QueryDSL using springDATA
		Iterable<SettingLevel> iterable =
				settingLevelRepository.findAll(predicate, Sort.by(Direction.ASC, "levelOrder"));
		List<SettingLevel> settingLevels = new ArrayList<>();
		iterable.forEach(settingLevels::add);
		logger.info("{} : Setting Level was founded", settingLevels.size());

		// mapping returned list
		List<SettingLevelDTO> settingLevelDTOs = new ArrayList<>();
		settingLevels.forEach(settingLevel -> settingLevelDTOs
				.add(mapper.map(settingLevel, SettingLevelDTO.class)));

		logger.info("Returning founded data ...");
		return settingLevelDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelService#save(com.acm.utils.dtos.SettingLevelDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingLevelDTO save(SettingLevelDTO settingLevelDTO) {

		Preconditions.checkNotNull(settingLevelDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingLevel settingLevel = mapper.map(settingLevelDTO, SettingLevel.class);

		CommonFunctions.mapperToSave(settingLevel, userClient, logger);
		settingLevel.setLevelOrder(changeLevelOrder());
		SettingLevel newSettingLevel = settingLevelRepository.save(settingLevel);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, SettingLevel.class.getSimpleName());
		return mapper.map(newSettingLevel, SettingLevelDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingLevelDTO)
	 */
	@Override
	public SettingLevelDTO save(Long id, SettingLevelDTO settingLevelDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingLevelDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingLevel with ID = {}", id);
		SettingLevel oldSettingLevel = settingLevelRepository.findById(id).orElse(null);
		// check if object is null
		if (oldSettingLevel == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, SettingLevel.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingLevel.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO =
				new SettingHistoriqueDTO(CommonFunctions.getTableNameFromClass(SettingLevel.class),
						CommonAOPConstants.UPDATE, id, CommonFunctions.convertObjectToJSONString(
								mapper.map(oldSettingLevel, SettingLevelDTO.class)));

		// mapping new data with existing data (oldSettingLevel)
		mapper.map(settingLevelDTO, oldSettingLevel);
		CommonFunctions.mapperToUpdate(oldSettingLevel, userClient, logger);

		// update & persist data in DB
		SettingLevel newSettingLevel = settingLevelRepository.save(oldSettingLevel);
		// mapping data
		SettingLevelDTO newSettingLevelDTO = mapper.map(newSettingLevel, SettingLevelDTO.class);
		SettingLevelProcessDTO settingLevelProcessDTO =
				new SettingLevelProcessDTO(newSettingLevelDTO);
		settingLevelProcessService.updateSettingLevelProcess(settingLevelProcessDTO);
		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingLevel.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newSettingLevelDTO));
		settingHistoriqueService.save(settingHistoriqueDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, SettingLevel.class.getSimpleName());
		return newSettingLevelDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelService#find()
	 */
	@Override
	public List<SettingLevelDTO> find() {

		List<SettingLevel> settingLevels = settingLevelRepository.findAll();
		List<SettingLevelDTO> settingLevelDTOs = new ArrayList<>();
		settingLevels.forEach(settingLevel -> settingLevelDTOs
				.add(mapper.map(settingLevel, SettingLevelDTO.class)));
		return settingLevelDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelService#updateOrder(java.util.List)
	 */
	@Override
	public List<SettingLevelDTO> updateOrder(List<SettingLevelDTO> settingLevelDTO)
			throws ResourcesNotFoundException {

		int i = 1;
		List<SettingLevelDTO> newSettingLevelDTO = new ArrayList<>();
		for (SettingLevelDTO levelDTO : settingLevelDTO) {
			levelDTO.setLevelOrder(i);
			newSettingLevelDTO.add(save(levelDTO.getId(), levelDTO));
			i++;
		}
		return newSettingLevelDTO;
	}

	/**
	 * Change level order.
	 * 
	 * @author AbdelkarimTurki
	 * @return the integer
	 */
	private Integer changeLevelOrder() {

		List<SettingLevel> settingLevels = settingLevelRepository.findAll();
		if (!ACMValidationUtils.isNullOrEmpty(settingLevels)) {
			Integer max = settingLevels.stream().mapToInt(level -> level.getLevelOrder()).max()
					.orElseThrow(NoSuchElementException::new);
			return max + 1;
		}
		else {
			return 1;
		}
	}
}
