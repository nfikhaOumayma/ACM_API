/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.SettingNotificationsDTO;

/**
 * {@link SettingNotificationsService} class.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public interface SettingNotificationsService {

	/**
	 * Find.
	 *
	 * @author HaythemBenizid
	 * @param idSettingNotifications the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingNotificationsDTO find(Long idSettingNotifications) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingNotificationsDTO the setting level DTO
	 * @return the list
	 */
	List<SettingNotificationsDTO> find(SettingNotificationsDTO settingNotificationsDTO);

	/**
	 * The method used for saving the given {@link SettingNotificationsDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingNotificationsDTO the settingNotifications DTO
	 * @return the SettingNotificationsService DTO
	 */
	SettingNotificationsDTO save(SettingNotificationsDTO settingNotificationsDTO);

	/**
	 * The method used for updating the given {@link SettingNotificationsDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingNotificationsDTO the settingNotifications DTO
	 * @return the SettingNotificationsService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingNotificationsDTO save(Long id, SettingNotificationsDTO settingNotificationsDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link SettingNotificationsDTO}.
	 *
	 * @author YesserSomai
	 * @param type the type
	 * @return the list
	 */
	List<SettingNotificationsDTO> find(String type);

	/**
	 * Adds the session id.
	 *
	 * @param sessionId the session id
	 */
	void addSessionId(String sessionId);

	/**
	 * Gets the session id.
	 *
	 * @param username the username
	 * @return the session id
	 */
	String getSessionId(String username);

	/**
	 * Send notification via web socket.
	 * 
	 * @author mlamloum
	 * @param username the username
	 * @param notificationsDTO the notifications DTO
	 */
	void sendNotificationViaWebSocket(String username, NotificationsDTO notificationsDTO);

	/**
	 * Find by type and enabled true.
	 *
	 * @author kouali
	 * @param type the type
	 * @return the list
	 */
	List<SettingNotificationsDTO> findByTypeAndEnabledTrue(String type);
}
