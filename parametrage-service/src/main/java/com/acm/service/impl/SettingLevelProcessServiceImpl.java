/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.CheckLevelProcessException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingLevelProcessRepository;
import com.acm.service.SettingHistoriqueService;
import com.acm.service.SettingLevelProcessService;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.models.QSettingLevelProcess;
import com.acm.utils.models.SettingLevel;
import com.acm.utils.models.SettingLevelProcess;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingLevelProcessServiceImpl} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
@Service
public class SettingLevelProcessServiceImpl implements SettingLevelProcessService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingLevelProcessServiceImpl.class);

	/** The settingLevelProcess repository. */
	@Autowired
	private SettingLevelProcessRepository settingLevelProcessRepository;

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
	 * @see
	 * com.acm.service.SettingLevelProcessService#find(com.acm.utils.dtos.SettingLevelProcessDTO)
	 */
	@Override
	public List<SettingLevelProcessDTO> find(SettingLevelProcessDTO settingLevelProcessDTO) {

		Preconditions.checkNotNull(settingLevelProcessDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(settingLevelProcessDTO.getIdProduct(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(settingLevelProcessDTO.getAmount(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QSettingLevelProcess
		QSettingLevelProcess qSettingLevelProcess = QSettingLevelProcess.settingLevelProcess;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingLevelProcess.enabled.eq(Boolean.TRUE));

		// find by id product (required)
		predicate.and(qSettingLevelProcess.idProduct.eq(settingLevelProcessDTO.getIdProduct()));

		// QueryDSL using springDATA
		Iterable<SettingLevelProcess> iterable =
				settingLevelProcessRepository.findAll(predicate, Sort.by(Direction.ASC, "amount"));
		List<SettingLevelProcess> settingLevelProcesss = new ArrayList<>();
		iterable.forEach(settingLevelProcesss::add);
		// mapping returned list
		List<SettingLevelProcessDTO> settingLevelProcessDTOs = new ArrayList<>();
		// filter by amount
		for (SettingLevelProcess settingLevelProcess : settingLevelProcesss) {
			settingLevelProcessDTOs
					.add(mapper.map(settingLevelProcess, SettingLevelProcessDTO.class));
			if (settingLevelProcess.getAmount()
					.compareTo(settingLevelProcessDTO.getAmount()) >= 0) {
				break;
			}
		}
		logger.info("Returning founded data ...");
		return settingLevelProcessDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingLevelProcessService#save(com.acm.utils.dtos.SettingLevelProcessDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingLevelProcessDTO save(SettingLevelProcessDTO settingLevelProcessDTO) {

		Preconditions.checkNotNull(settingLevelProcessDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingLevelProcess settingLevelProcess =
				mapper.map(settingLevelProcessDTO, SettingLevelProcess.class);

		CommonFunctions.mapperToSave(settingLevelProcess, userClient, logger);
		SettingLevelProcess newSettingLevelProcess =
				settingLevelProcessRepository.save(settingLevelProcess);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingLevelProcess.class.getSimpleName());
		return mapper.map(newSettingLevelProcess, SettingLevelProcessDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelProcessService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingLevelProcessDTO)
	 */
	@Override
	public SettingLevelProcessDTO save(Long id, SettingLevelProcessDTO settingLevelProcessDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingLevelProcessDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingLevelProcess with ID = {}", id);
		SettingLevelProcess oldSettingLevelProcess =
				settingLevelProcessRepository.findById(id).orElse(null);
		// check if object is null
		if (oldSettingLevelProcess == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingLevelProcess.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingLevelProcess.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingLevelProcess.class),
				CommonAOPConstants.UPDATE, id, CommonFunctions.convertObjectToJSONString(
						mapper.map(oldSettingLevelProcess, SettingLevelProcessDTO.class)));

		// mapping new data with existing data (oldSettingLevelProcess)
		mapper.map(settingLevelProcessDTO, oldSettingLevelProcess);
		CommonFunctions.mapperToUpdate(oldSettingLevelProcess, userClient, logger);

		// update & persist data in DB
		SettingLevelProcess newSettingLevelProcess =
				settingLevelProcessRepository.save(oldSettingLevelProcess);
		SettingLevelProcessDTO newSettingLevelProcessDTO =
				mapper.map(newSettingLevelProcess, SettingLevelProcessDTO.class);
		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingLevelProcess.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newSettingLevelProcessDTO));
		settingHistoriqueService.save(settingHistoriqueDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingLevelProcess.class.getSimpleName());
		return newSettingLevelProcessDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelProcessService#findSetting(com.acm.utils.dtos.
	 * SettingLevelProcessDTO)
	 */
	@Override
	public List<SettingLevelProcessDTO> findSetting(SettingLevelProcessDTO settingLevelProcessDTO) {

		Preconditions.checkNotNull(settingLevelProcessDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(settingLevelProcessDTO.getIdProduct(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QSettingLevelProcess
		QSettingLevelProcess qSettingLevelProcess = QSettingLevelProcess.settingLevelProcess;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by id product (required)
		predicate.and(qSettingLevelProcess.idProduct.eq(settingLevelProcessDTO.getIdProduct()));

		// QueryDSL using springDATA
		Iterable<SettingLevelProcess> iterable = settingLevelProcessRepository.findAll(predicate);
		List<SettingLevelProcess> settingLevelProcesss = new ArrayList<>();
		iterable.forEach(settingLevelProcesss::add);
		// mapping returned list
		List<SettingLevelProcessDTO> settingLevelProcessDTOs = new ArrayList<>();
		for (SettingLevelProcess settingLevelProcess : settingLevelProcesss) {
			settingLevelProcessDTOs
					.add(mapper.map(settingLevelProcess, SettingLevelProcessDTO.class));
		}
		logger.info("Returning founded data ...");
		return settingLevelProcessDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelProcessService#updateAmount(java.util.List)
	 */
	@Override
	public Boolean updateAmount(List<SettingLevelProcessDTO> settingLevelProcessDTOs)
			throws ResourcesNotFoundException, CheckLevelProcessException {

		List<SettingLevelProcessDTO> sortedOrder = settingLevelProcessDTOs.stream()
				.sorted((o1, o2) -> o1.getSettingLevelDTO().getLevelOrder()
						.compareTo(o2.getSettingLevelDTO().getLevelOrder()))
				.collect(Collectors.toList());
		List<BigDecimal> amounts = sortedOrder.stream().map(SettingLevelProcessDTO::getAmount)
				.collect(Collectors.toList());
		Boolean checkSorted = isSorted(amounts);
		if (Boolean.TRUE.equals(checkSorted)) {
			for (SettingLevelProcessDTO level : sortedOrder) {
				save(level.getId(), level);
			}
		}
		else {
			logger.error(CommonLoggerMessage.LEVEL_PROCESS_AMOUNT_NOT_SORTED,
					SettingLevelProcess.class.getSimpleName());
			throw new CheckLevelProcessException(
					CommonExceptionsMessage.LEVEL_PROCESS_AMOUNT_NOT_SORTED,
					CommonExceptionsMessage.LEVEL_PROCESS_AMOUNT_NOT_SORTED
							+ SettingLevelProcess.class.getSimpleName());
		}
		return checkSorted;
	}

	/**
	 * Checks if is sorted.
	 *
	 * @param amounts the amounts
	 * @return true, if is sorted
	 */
	private boolean isSorted(List<BigDecimal> amounts) {

		for (int i = 0; i < amounts.size() - 1; i++) {
			if (amounts.get(i).compareTo(amounts.get(i + 1)) > 0) {
				return false;
			}
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingLevelProcessService#updateSettingLevelProcess(com.acm.utils.dtos.
	 * SettingLevelProcessDTO)
	 */
	@Override
	public void updateSettingLevelProcess(SettingLevelProcessDTO settingLevelProcessDTO) {

		Preconditions.checkNotNull(settingLevelProcessDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (!ACMValidationUtils.isNullOrEmpty(settingLevelProcessDTO.getSettingLevelDTO())
				&& !ACMValidationUtils
						.isNullOrEmpty(settingLevelProcessDTO.getSettingLevelDTO().getId())) {
			// find by setting level
			List<SettingLevelProcess> settingLevelProcesses =
					settingLevelProcessRepository.findBySettingLevel(
							new SettingLevel(settingLevelProcessDTO.getSettingLevelDTO().getId()));
			// update all founded data
			for (SettingLevelProcess model : settingLevelProcesses) {
				model.setEnabled(settingLevelProcessDTO.getSettingLevelDTO().getEnabled());
				CommonFunctions.mapperToUpdate(model, userClient, logger);
				// update & persist data in DB
				SettingLevelProcess updatedSettingLevelProcess =
						settingLevelProcessRepository.save(model);
				logger.debug("{}", updatedSettingLevelProcess);
				logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
						SettingLevelProcess.class.getSimpleName());
			}
		}
	}
}
