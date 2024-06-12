/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.pagination.NotificationsPaginationDTO;

/**
 * {@link NotificationsServices} class.
 *
 * @author YesserSomai
 * @since 0.10.0
 */
public interface NotificationsServices {

	/**
	 * Find By Pagination.
	 * 
	 * @author HaythemBenizid
	 * @param notificationsPaginationDTO the notifications pagination DTO
	 * @return the notifications pagination DTO
	 */
	NotificationsPaginationDTO find(NotificationsPaginationDTO notificationsPaginationDTO);

	/**
	 * Save.
	 *
	 * @param notificationsDTO the notifications DTO
	 * @return the notifications DTO
	 */
	NotificationsDTO save(NotificationsDTO notificationsDTO);

	/**
	 * The method used for updating the given {@link NotificationsDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param notificationsDTO the notifications DTO
	 * @return the NotificationsService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	NotificationsDTO save(Long id, NotificationsDTO notificationsDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find first 10 row by status NOTIF get NEW notifications for connected user.
	 * 
	 * @author YesserSomai
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<NotificationsDTO> find();

	/**
	 * Count new NOTIF for connected user.
	 * 
	 * @author HaythemBenizid
	 * @return the long
	 */
	Long count();

	/**
	 * Count new NOTIF for connected user (USED in WebSocket call).
	 *
	 * @author idridi
	 * @param loginConnectedUser the login connected user
	 * @return the long
	 */
	Long count(String loginConnectedUser);
}
