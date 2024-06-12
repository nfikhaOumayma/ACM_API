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

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.UserNotificationService;
import com.acm.utils.dtos.UsersNotificationsDTO;

/**
 * {@link UserNotificationController} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
@RestController
@RequestMapping("/user-notifications")
public class UserNotificationController {

	/** The user notification service. */
	@Autowired
	private UserNotificationService userNotificationService;

	/**
	 * find the user notifications for the CONNECTED USER {@link UsersNotificationsDTO}.
	 *
	 * @author MoezMhiri
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/")
	public List<UsersNotificationsDTO> find() throws ResourcesNotFoundException {

		return userNotificationService.find();
	}

	/**
	 * Find my setting notification.
	 *
	 * @author LamloumManel
	 * @param usersNotificationsDTO the users notifications DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/findByUser")
	public List<UsersNotificationsDTO> findByUser(
			@RequestBody UsersNotificationsDTO usersNotificationsDTO)
			throws ResourcesNotFoundException {

		return userNotificationService.findByUser(usersNotificationsDTO);

	}

	/**
	 * Update.
	 *
	 * @author ManelLamloum
	 * @param userNotificationDTO the user notification DTO
	 * @return the users notifications DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public UsersNotificationsDTO update(@RequestBody UsersNotificationsDTO userNotificationDTO)
			throws ResourcesNotFoundException {

		return userNotificationService.save(userNotificationDTO.getIdUsersNotification(),
				userNotificationDTO);
	}

	/**
	 * Creates the userNotification.
	 *
	 * @author ManelLamloum
	 * @param usersNotificationsDTO the users notifications DTO
	 * @return the users notifications DTO
	 */
	@PostMapping("/create")
	public UsersNotificationsDTO create(@RequestBody UsersNotificationsDTO usersNotificationsDTO) {

		return userNotificationService.save(usersNotificationsDTO);
	}

	/**
	 * update config for all user => Enable/disable notification.
	 *
	 * @author ManelLamloum
	 * @param idSettingNotifications the id setting notifications
	 * @param statut the statut
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-all/{idSettingNotifications}/{statut}")
	public List<UsersNotificationsDTO> updateAll(
			@PathVariable("idSettingNotifications") Long idSettingNotifications,
			@PathVariable("statut") Boolean statut) throws ResourcesNotFoundException {

		return userNotificationService.updateAll(idSettingNotifications, statut);
	}

}
