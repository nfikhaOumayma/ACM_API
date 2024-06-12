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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.NotificationsRepository;
import com.acm.service.NotificationsServices;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.pagination.NotificationsPaginationDTO;
import com.acm.utils.enums.NotificationStatut;
import com.acm.utils.models.Notifications;
import com.acm.utils.models.QNotifications;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link NotificationsServicesImpl} class.
 *
 * @author YesserSomai
 * @since 0.10.0
 */
@Service
@Transactional
public class NotificationsServicesImpl implements NotificationsServices {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(NotificationsServicesImpl.class);

	/** The notifications repository. */
	@Autowired
	private NotificationsRepository notificationsRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.NotificationsServices#find(com.acm.utils.dtos.pagination.
	 * NotificationsPaginationDTO)
	 */
	@Override
	public NotificationsPaginationDTO find(NotificationsPaginationDTO notificationsPaginationDTO) {

		Preconditions.checkNotNull(notificationsPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(notificationsPaginationDTO.getPageNumber())) {
			notificationsPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(notificationsPaginationDTO.getPageSize())) {
			notificationsPaginationDTO.setPageSize(10);
		}
		// setting default data
		notificationsPaginationDTO.setResultsNotifications(new ArrayList<>());
		// setting default totals pages
		notificationsPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		notificationsPaginationDTO.setTotalPages(0);

		// build Predicate using given params
		// init QNotifications
		QNotifications qNotifications = QNotifications.notifications;
		BooleanBuilder predicate = new BooleanBuilder();

		predicate.and(
				qNotifications.username.eq(CommonFunctions.getConnectedUser(logger).getLogin()));

		// init pageable params (oage number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("+".equals(notificationsPaginationDTO.getSortDirection())) {
			pageable = PageRequest.of(notificationsPaginationDTO.getPageNumber(),
					notificationsPaginationDTO.getPageSize(), Sort.Direction.ASC, "idNotification");
		}
		else if ("-".equals(notificationsPaginationDTO.getSortDirection())) {
			pageable = PageRequest.of(notificationsPaginationDTO.getPageNumber(),
					notificationsPaginationDTO.getPageSize(), Sort.Direction.DESC,
					"idNotification");
		}
		else {
			// default sort by applyDate : DESC
			pageable = PageRequest.of(notificationsPaginationDTO.getPageNumber(),
					notificationsPaginationDTO.getPageSize(), Sort.Direction.DESC, "creactionDate");
		}

		// load data
		Page<Notifications> pagedResult = notificationsRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Notifications> notifications = pagedResult.getContent();
			logger.info("{} : Notifications was founded", notifications.size());
			List<NotificationsDTO> notificationsDTOs = new ArrayList<>();
			notifications.forEach(notification -> notificationsDTOs
					.add(mapper.map(notification, NotificationsDTO.class)));
			// setting data
			notificationsPaginationDTO.setResultsNotifications(notificationsDTOs);
			// setting totals pages
			notificationsPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			notificationsPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return notificationsPaginationDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.NotificationsServices#save(com.acm.utils.dtos.NotificationsDTO)
	 */
	@Override
	public NotificationsDTO save(NotificationsDTO notificationsDTO) {

		Preconditions.checkNotNull(notificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// check if username is null => auto assign to "Super.Admin"
		if (ACMValidationUtils.isNullOrEmpty(notificationsDTO.getUsername())) {
			notificationsDTO.setUsername(CommonConstants.DEFAULT_USER);
		}
		Notifications notifications = mapper.map(notificationsDTO, Notifications.class);
		CommonFunctions.mapperToSave(notifications, userClient, logger);
		notifications.setCreactionDate(new Date());
		// setting status Notifications to NEW
		notifications.setStatusNotif(NotificationStatut.NEW.name());
		Notifications newNotifications = notificationsRepository.save(notifications);
		NotificationsDTO notificationsDTOResult =
				mapper.map(newNotifications, NotificationsDTO.class);
		String login = notificationsDTOResult.getUsername();
		// send Notification to webSocket subscribers
		parametrageClient.sendNotificationViaWebSocket(login, notificationsDTOResult);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Notifications.class.getSimpleName());
		return notificationsDTOResult;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.NotificationsServices#save(java.lang.Long,
	 * com.acm.utils.dtos.NotificationsDTO)
	 */
	@Override
	public NotificationsDTO save(Long id, NotificationsDTO notificationsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(notificationsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update loan  with ID = {}", id);
		Notifications oldNotifications = notificationsRepository.findById(id).orElse(null);

		// check if object is null
		if (oldNotifications == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Notifications.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Notifications.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// update status Notifications to READ
		if (Boolean.TRUE.equals(notificationsDTO.getActionStatueNotif())) {
			if (oldNotifications.getStatusNotif().equals(NotificationStatut.NEW.name())) {
				oldNotifications.setStatusNotif(NotificationStatut.READ.name());
			}
			else {
				oldNotifications.setStatusNotif(NotificationStatut.NEW.name());
			}
		}
		else if (oldNotifications.getStatusNotif().equals(NotificationStatut.NEW.name())) {
			oldNotifications.setStatusNotif(NotificationStatut.READ.name());
		}
		else {
			return mapper.map(oldNotifications, NotificationsDTO.class);
		}
		CommonFunctions.mapperToUpdate(oldNotifications, userClient, logger);
		Notifications newNotifications = notificationsRepository.save(oldNotifications);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Notifications.class.getSimpleName());
		return mapper.map(newNotifications, NotificationsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.NotificationsServices#find()
	 */
	@Override
	public List<NotificationsDTO> find() {

		List<Notifications> notifications =
				notificationsRepository.findFirst10ByStatusNotifAndUsernameOrderByCreactionDateDesc(
						NotificationStatut.NEW.name(),
						CommonFunctions.getConnectedUser(logger).getLogin());
		List<NotificationsDTO> notificationsDTOs = new ArrayList<>();
		notifications.forEach(notification -> notificationsDTOs
				.add(mapper.map(notification, NotificationsDTO.class)));
		logger.info("Returning {} New Notifications", notificationsDTOs.size());
		return notificationsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.NotificationsServices#count()
	 */
	@Override
	public Long count() {

		return notificationsRepository.countByStatusNotifAndUsernameOrderByCreactionDateDesc(
				NotificationStatut.NEW.name(), CommonFunctions.getConnectedUser(logger).getLogin());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.NotificationsServices#count(java.lang.String)
	 */
	@Override
	public Long count(String loginConnectedUser) {

		return notificationsRepository.countByStatusNotifAndUsernameOrderByCreactionDateDesc(
				NotificationStatut.NEW.name(), CommonFunctions.getConnectedUser(logger).getLogin());
	}
}
