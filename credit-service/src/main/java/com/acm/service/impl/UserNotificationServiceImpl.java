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
import com.acm.repository.UserNotificationRepository;
import com.acm.service.UserNotificationService;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UsersNotificationsDTO;
import com.acm.utils.models.QUsersNotifications;
import com.acm.utils.models.SettingNotifications;
import com.acm.utils.models.UsersNotifications;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The {@link UserNotificationServiceImpl} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
@Service
public class UserNotificationServiceImpl implements UserNotificationService {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(UserNotificationServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user notification repository. */
	@Autowired
	private UserNotificationRepository userNotificationRepository;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserNotificationService#find()
	 */
	@Override
	public List<UsersNotificationsDTO> find() {

		// find connected user details
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);

		// init QHabilitation
		QUsersNotifications qUsersNotifications = QUsersNotifications.usersNotifications;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qUsersNotifications.enabled.eq(Boolean.TRUE));

		// find only statut valid data
		predicate.and(qUsersNotifications.statut.eq(Boolean.TRUE));

		// find by username
		predicate.and(qUsersNotifications.user.username.eq(connectedUser.getLogin()));
		// QueryDSL using springDATA
		Iterable<UsersNotifications> iterable = userNotificationRepository.findAll(predicate);
		List<UsersNotifications> usersNotifications = new ArrayList<>();
		iterable.forEach(usersNotifications::add);
		logger.info("{} : users Notifications for connected user was founded",
				usersNotifications.size());

		// mapping returned list
		List<UsersNotificationsDTO> usersNotificationsDTOs = new ArrayList<>();
		usersNotifications.forEach(usersNotification -> usersNotificationsDTOs
				.add(mapper.map(usersNotification, UsersNotificationsDTO.class)));
		return usersNotificationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserNotificationService#find(com.acm.utils.dtos.HabilitationDTO)
	 */
	@Override
	public List<UsersNotificationsDTO> find(UsersNotificationsDTO usersNotificationsDTO) {

		Preconditions.checkNotNull(usersNotificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// return empty list if connected user not given
		if (ACMValidationUtils.isNullOrEmpty(usersNotificationsDTO.getUserDTO())
				|| ACMValidationUtils
						.isNullOrEmpty(usersNotificationsDTO.getUserDTO().getLogin())) {
			return new ArrayList<>();
		}

		// init QUsersNotifications
		QUsersNotifications qUsersNotifications = QUsersNotifications.usersNotifications;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qUsersNotifications.enabled.eq(Boolean.TRUE));
		// find by status = TRUE
		predicate.and(qUsersNotifications.statut.eq(Boolean.TRUE));

		// find by username
		predicate.and(qUsersNotifications.user.username
				.eq(usersNotificationsDTO.getUserDTO().getLogin()));

		// QueryDSL using springDATA
		Iterable<UsersNotifications> iterable = userNotificationRepository.findAll(predicate);
		List<UsersNotifications> usersNotifications = new ArrayList<>();
		iterable.forEach(usersNotifications::add);
		logger.info("{} : users Notifications was founded", usersNotifications.size());

		// mapping returned list
		List<UsersNotificationsDTO> usersNotificationsDTOs = new ArrayList<>();
		usersNotifications.forEach(usersNotification -> usersNotificationsDTOs
				.add(mapper.map(usersNotification, UsersNotificationsDTO.class)));
		return usersNotificationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.UserNotificationService#findByUserForSettingNotification(com.acm.utils.dtos.
	 * UsersNotificationsDTO)
	 */
	@Override
	public List<UsersNotificationsDTO> findByUser(UsersNotificationsDTO usersNotificationsDTO) {

		Preconditions.checkNotNull(usersNotificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// return empty list if connected user not given
		if (ACMValidationUtils.isNullOrEmpty(usersNotificationsDTO.getUserDTO())
				|| ACMValidationUtils
						.isNullOrEmpty(usersNotificationsDTO.getUserDTO().getLogin())) {
			return new ArrayList<>();
		}
		// init QHabilitation
		QUsersNotifications qUsersNotifications = QUsersNotifications.usersNotifications;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by given username
		predicate.and(qUsersNotifications.user.username
				.eq(usersNotificationsDTO.getUserDTO().getLogin()));

		// QueryDSL using springDATA
		Iterable<UsersNotifications> iterable = userNotificationRepository.findAll(predicate);
		List<UsersNotifications> usersNotifications = new ArrayList<>();
		iterable.forEach(usersNotifications::add);
		logger.info("{} : users Notifications was founded", usersNotifications.size());

		// mapping returned list
		List<UsersNotificationsDTO> usersNotificationsDTOs = new ArrayList<>();
		usersNotifications.forEach(usersNotification -> usersNotificationsDTOs
				.add(mapper.map(usersNotification, UsersNotificationsDTO.class)));
		return usersNotificationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserNotificationService#updateMySettingNotification(java.lang.Long,
	 * com.acm.utils.dtos.UsersNotificationsDTO)
	 */
	@Override
	public UsersNotificationsDTO save(Long id, UsersNotificationsDTO usersNotificationsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(usersNotificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Address with ID = {}", id);
		UsersNotifications oldUserNotifications =
				userNotificationRepository.findById(id).orElse(null);

		// check if object is null
		if (oldUserNotifications == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UsersNotifications.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + UsersNotifications.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldAddress)
		CommonFunctions.mapperToUpdate(oldUserNotifications, userClient, logger);

		oldUserNotifications.setStatut(usersNotificationsDTO.getStatut());
		oldUserNotifications.setEnabled(usersNotificationsDTO.getStatut());

		// update & persist data in DB
		UsersNotifications newUserNotification =
				userNotificationRepository.save(oldUserNotifications);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				UsersNotifications.class.getSimpleName());
		return mapper.map(newUserNotification, UsersNotificationsDTO.class);

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserNotificationService#updateAll(java.lang.Long, java.lang.Boolean)
	 */
	@Override
	public List<UsersNotificationsDTO> updateAll(Long idSettingNotifications, Boolean statut)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(idSettingNotifications,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(statut, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// find all user config by notification type
		List<UsersNotifications> usersNotifications =
				userNotificationRepository.findByEnabledNotAndStatutNotAndSettingNotification(
						statut, statut, new SettingNotifications(idSettingNotifications));

		List<UsersNotificationsDTO> usersNotificationsDTOs = new ArrayList<>();
		// update data for user notification
		for (UsersNotifications usersNotification : usersNotifications) {
			UsersNotificationsDTO usersNotificationDTO = new UsersNotificationsDTO();
			usersNotificationDTO.setStatut(statut);
			usersNotificationDTO.setEnabled(statut);
			// update data
			usersNotificationsDTOs
					.add(save(usersNotification.getIdUsersNotification(), usersNotificationDTO));
		}
		// returning data
		return usersNotificationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserNotificationService#save(com.acm.utils.dtos.UsersNotificationsDTO)
	 */
	@Override
	public UsersNotificationsDTO save(UsersNotificationsDTO usersNotificationsDTO) {

		Preconditions.checkNotNull(usersNotificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		UsersNotifications usersNotifications =
				mapper.map(usersNotificationsDTO, UsersNotifications.class);
		CommonFunctions.mapperToSave(usersNotifications, userClient, logger);
		UsersNotifications newusersNotifications =
				userNotificationRepository.save(usersNotifications);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				UsersNotifications.class.getSimpleName());
		return mapper.map(newusersNotifications, UsersNotificationsDTO.class);
	}

}
