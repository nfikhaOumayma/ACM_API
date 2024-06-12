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
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.CreditClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingNotificationsRepository;
import com.acm.service.SettingHistoriqueService;
import com.acm.service.SettingNotificationsService;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingNotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.QSettingNotifications;
import com.acm.utils.models.ReportVisit;
import com.acm.utils.models.SettingNotifications;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingNotificationsServiceImpl2}.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@Service
public class SettingNotificationsServiceImpl implements SettingNotificationsService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingNotificationsServiceImpl.class);

	/** The acmEnvironnement repository. */
	@Autowired
	private SettingNotificationsRepository settingNotificationsRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The simp messaging template. */
	@Autowired
	private SimpMessagingTemplate simpMessagingTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingNotificationsService#find(java.lang.Long)
	 */
	@Override
	public SettingNotificationsDTO find(Long idSettingNotifications)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idSettingNotifications,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find SettingNotifications by ID : {}", idSettingNotifications);
		SettingNotifications settingNotifications =
				settingNotificationsRepository.findById(idSettingNotifications).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingNotifications)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ReportVisit.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingNotifications.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + idSettingNotifications);
		}
		return mapper.map(settingNotifications, SettingNotificationsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingNotificationsService#find(com.acm.utils.dtos.SettingNotificationsDTO)
	 */
	@Override
	public List<SettingNotificationsDTO> find(SettingNotificationsDTO settingNotificationsDTO) {

		// init QSettingNotifications
		QSettingNotifications qSettingNotifications = QSettingNotifications.settingNotifications;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingNotifications.enabled.eq(Boolean.TRUE));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(settingNotificationsDTO.getIdSettingNotification())) {
			predicate.and(qSettingNotifications.idSettingNotification
					.eq(settingNotificationsDTO.getIdSettingNotification()));
		}

		// find by category
		if (!ACMValidationUtils.isNullOrEmpty(settingNotificationsDTO.getCategory())) {
			predicate.and(qSettingNotifications.category.eq(settingNotificationsDTO.getCategory()));
		}

		// find by type notif
		if (!ACMValidationUtils.isNullOrEmpty(settingNotificationsDTO.getTypeNotif())) {
			predicate.and(
					qSettingNotifications.typeNotif.eq(settingNotificationsDTO.getTypeNotif()));
		}

		// QueryDSL using springDATA
		Iterable<SettingNotifications> iterable = settingNotificationsRepository.findAll(predicate);
		List<SettingNotifications> settingNotifications = new ArrayList<>();
		iterable.forEach(settingNotifications::add);
		logger.info("{} : setting notifications was founded", settingNotifications.size());

		// mapping returned list
		List<SettingNotificationsDTO> settingNotificationsDTOs = new ArrayList<>();
		settingNotifications.forEach(settingNotification -> settingNotificationsDTOs
				.add(mapper.map(settingNotification, SettingNotificationsDTO.class)));

		logger.info("Returning founded data ...");
		return settingNotificationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingNotificationsService#save(com.acm.utils.dtos.SettingNotificationsDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingNotificationsDTO save(SettingNotificationsDTO settingNotificationsDTO) {

		Preconditions.checkNotNull(settingNotificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingNotifications settingNotifications =
				mapper.map(settingNotificationsDTO, SettingNotifications.class);

		CommonFunctions.mapperToSave(settingNotifications, userClient, logger);
		SettingNotifications newSettingNotifications =
				settingNotificationsRepository.save(settingNotifications);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingNotifications.class.getSimpleName());
		return mapper.map(newSettingNotifications, SettingNotificationsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingNotificationsService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingNotificationsDTO)
	 */
	@Override
	public SettingNotificationsDTO save(Long id, SettingNotificationsDTO settingNotificationsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingNotificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingNotifications with ID = {}", id);
		SettingNotifications oldSettingNotifications =
				settingNotificationsRepository.findById(id).orElse(null);
		// check if object is null
		if (oldSettingNotifications == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingNotifications.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingNotifications.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingNotifications.class),
				CommonAOPConstants.UPDATE, id, CommonFunctions.convertObjectToJSONString(
						mapper.map(oldSettingNotifications, SettingNotificationsDTO.class)));

		// setting enabled value (oldSettingNotifications)
		oldSettingNotifications.setEnabled(settingNotificationsDTO.getEnabled());
		CommonFunctions.mapperToUpdate(oldSettingNotifications, userClient, logger);

		// update & persist data in DB
		SettingNotifications newSettingNotifications =
				settingNotificationsRepository.save(oldSettingNotifications);
		SettingNotificationsDTO newSettingNotificationsDTO =
				mapper.map(newSettingNotifications, SettingNotificationsDTO.class);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingNotifications.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newSettingNotificationsDTO));
		settingHistoriqueService.save(settingHistoriqueDTO);

		// Update notification config for All users in cas we enable a notifaction
		// if UpdateUserNotification=True => Force to update all notification
		if (Boolean.TRUE.equals(settingNotificationsDTO.getEnabled())
				&& Boolean.TRUE.equals(settingNotificationsDTO.getUpdateUserNotification())) {
			creditClient.updateAll(id, Boolean.TRUE);
		}

		// update notification config for All users in cas we disable
		if (Boolean.FALSE.equals(settingNotificationsDTO.getEnabled())) {
			creditClient.updateAll(id, Boolean.FALSE);
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingNotifications.class.getSimpleName());
		return newSettingNotificationsDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingNotificationsService#find()
	 */
	@Override
	public List<SettingNotificationsDTO> find(String type) {

		List<SettingNotifications> settingNotificationss =
				settingNotificationsRepository.findByType(type);
		List<SettingNotificationsDTO> settingNotificationssDTOs = new ArrayList<>();
		settingNotificationss.forEach(settingNotifications -> settingNotificationssDTOs
				.add(mapper.map(settingNotifications, SettingNotificationsDTO.class)));
		return settingNotificationssDTOs;
	}

	@Override
	public List<SettingNotificationsDTO> findByTypeAndEnabledTrue(String type) {

		List<SettingNotifications> settingNotificationss =
				settingNotificationsRepository.findByTypeAndEnabled(type, true);
		List<SettingNotificationsDTO> settingNotificationssDTOs = new ArrayList<>();
		settingNotificationss.forEach(settingNotifications -> settingNotificationssDTOs
				.add(mapper.map(settingNotifications, SettingNotificationsDTO.class)));
		return settingNotificationssDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingNotificationsService#addSessionId(java.lang.String)
	 */
	@Override
	public void addSessionId(String sessionId) {

		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		CommonFunctions.addSessionId(userDTO.getLogin(), sessionId);

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingNotificationsService#getSessionId(java.lang.String)
	 */
	@Override
	public String getSessionId(String username) {

		return CommonFunctions.getSessionId(username);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingNotificationsService#sendNotificationViaWebSocket(java.lang.String,
	 * com.acm.utils.dtos.NotificationsDTO)
	 */
	@Override
	public void sendNotificationViaWebSocket(String username, NotificationsDTO notificationsDTO) {

		String sessionId = getSessionId(username);
		if (!ACMValidationUtils.isNullOrEmpty(sessionId)) {
			simpMessagingTemplate.convertAndSend("/queue/progress-" + sessionId, notificationsDTO);
		}
	}

}
