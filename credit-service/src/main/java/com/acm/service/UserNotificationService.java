/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.UsersNotificationsDTO;

/**
 * The {@link UserNotificationService} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
public interface UserNotificationService {

	/**
	 * Find all user notifications in db relative to connected user.
	 *
	 * @author MoezMhiri
	 * @return an array {@link List} of the specified object {@link UsersNotificationsDTO}
	 */
	List<UsersNotificationsDTO> find();

	/**
	 * search for user notifications by given params.
	 * 
	 * @author MoezMhiri
	 * @param usersNotificationsDTO the search user notification
	 * @return the list
	 */
	List<UsersNotificationsDTO> find(UsersNotificationsDTO usersNotificationsDTO);

	/**
	 * Find by user for setting notification.
	 *
	 * @author ManelLamloum
	 * @param usersNotificationsDTO the users notifications DTO
	 * @return the list
	 */
	List<UsersNotificationsDTO> findByUser(UsersNotificationsDTO usersNotificationsDTO);

	/**
	 * Update my setting notification.
	 *
	 * @author ManelLamloum
	 * @param id the id
	 * @param usersNotificationsDTO the users notifications DTO
	 * @return the users notifications DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UsersNotificationsDTO save(Long id, UsersNotificationsDTO usersNotificationsDTO)
			throws ResourcesNotFoundException;

	/**
	 * Update all.
	 *
	 * @author ManelLamloum
	 * @param idSettingNotifications the id setting notifications
	 * @param statut the statut
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<UsersNotificationsDTO> updateAll(Long idSettingNotifications, Boolean statut)
			throws ResourcesNotFoundException;

	/**
	 * Save the userNotification.
	 * 
	 * @author ManelLamloum
	 * @param usersNotificationsDTO the users notifications DTO
	 * @return the users notifications DTO
	 */
	UsersNotificationsDTO save(UsersNotificationsDTO usersNotificationsDTO);

}
