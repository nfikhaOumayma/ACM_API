/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingNotificationsService;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.SettingNotificationsDTO;

/**
 * This class @{link SettingNotificationsController} used to control all the SettingNotifications
 * requests.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@RestController
@RequestMapping("/setting-notifications")
public class SettingNotificationsController {

	/** The SettingNotifications service. */
	@Autowired
	private SettingNotificationsService settingNotificationsService;

	/**
	 * Find setting by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingNotificationsDTO the setting level process DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingNotificationsDTO> find(
			@RequestBody SettingNotificationsDTO settingNotificationsDTO) {

		return settingNotificationsService.find(settingNotificationsDTO);
	}

	/**
	 * Create the SettingNotifications.
	 *
	 * @author HaythemBenizid
	 * @param settingNotificationsDTO the settingNotifications DTO
	 * @return the SettingNotifications DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingNotificationsDTO create(
			@RequestBody SettingNotificationsDTO settingNotificationsDTO)
			throws ResourcesNotFoundException {

		return settingNotificationsService.save(settingNotificationsDTO);
	}

	/**
	 * Update the SettingNotifications by id.
	 *
	 * @author HaythemBenizid
	 * @param settingNotificationsDTO the settingNotifications DTO
	 * @return the settingNotifications DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingNotificationsDTO update(
			@RequestBody SettingNotificationsDTO settingNotificationsDTO)
			throws ResourcesNotFoundException {

		return settingNotificationsService.save(settingNotificationsDTO.getIdSettingNotification(),
				settingNotificationsDTO);
	}

	/**
	 * Find {@link List} of {@link SettingNotificationsDTO} list.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingNotificationsDTO> findSettingNotifications() {

		return settingNotificationsService.find(CommonConstants.GLOBAL_SETTING_NOTIF);
	}

	/**
	 * Save session id.
	 *
	 * @param sessionId the session id
	 */
	@GetMapping("/save-session-id/{sessionId}")
	public void saveSessionId(@PathVariable("sessionId") String sessionId) {

		settingNotificationsService.addSessionId(sessionId);
	}

	/**
	 * Gets the session id.
	 *
	 * @param username the username
	 * @return the session id
	 */
	@GetMapping("/get-session-id/{username}")
	public String getSessionId(@PathVariable("username") String username) {

		return settingNotificationsService.getSessionId(username);
	}

	/**
	 * Send notification via web socket.
	 * 
	 * @author mlamloum
	 * @param username the username
	 * @param notificationsDTO the notifications DTO
	 */
	@PostMapping("/send-notification-via-websocket/{username}")
	public void sendNotificationViaWebSocket(@PathVariable("username") String username,
			@RequestBody NotificationsDTO notificationsDTO) {

		settingNotificationsService.sendNotificationViaWebSocket(username, notificationsDTO);
	}

	/**
	 * Find setting notifications synchro calendar.
	 *
	 * @return the list
	 */
	@GetMapping("/find-synchro-calendar-setting")
	public List<SettingNotificationsDTO> findSettingNotificationsSynchroCalendar() {

		return settingNotificationsService.find(CommonConstants.SYNCHRO_CALENDAR_SETTING);
	}

	/**
	 * Find by type and enabled.
	 *
	 * @return the list
	 */
	@GetMapping("/find-by-type-enabled")
	public List<SettingNotificationsDTO> findByTypeAndEnabled() {

		return settingNotificationsService
				.findByTypeAndEnabledTrue(CommonConstants.SYNCHRO_CALENDAR_SETTING);
	}
}
