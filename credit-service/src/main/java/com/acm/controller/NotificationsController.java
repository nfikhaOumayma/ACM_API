/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.NotificationsServices;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.pagination.NotificationsPaginationDTO;

/**
 * {@link NotificationsController} class.
 *
 * @author YesserSomai
 * @since 0.10.0
 */
@RestController
@RequestMapping("/notifications")
public class NotificationsController {

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/**
	 * Find by Pagination.
	 * 
	 * @author YesserSomai
	 * @param notificationsPaginationDTO the notifications pagination DTO
	 * @return the notifications pagination DTO
	 */
	@PostMapping("/")
	public NotificationsPaginationDTO find(
			@RequestBody NotificationsPaginationDTO notificationsPaginationDTO) {

		return notificationsServices.find(notificationsPaginationDTO);
	}

	/**
	 * Creates Notification.
	 * 
	 * @author YesserSomai
	 * @param notificationsDTO the notifications DTO
	 * @return the notifications DTO
	 */
	@PostMapping("/create")
	public NotificationsDTO create(@RequestBody NotificationsDTO notificationsDTO) {

		return notificationsServices.save(notificationsDTO);
	}

	/**
	 * Update Notification.
	 *
	 * @author YesserSomai
	 * @param notificationsDTO the notifications DTO
	 * @return the notifications DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public NotificationsDTO update(@RequestBody NotificationsDTO notificationsDTO)
			throws ResourcesNotFoundException {

		return notificationsServices.save(notificationsDTO.getIdNotification(), notificationsDTO);
	}

	/**
	 * Find only New Notifications for connected user.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/find-notifications")
	public List<NotificationsDTO> find() {

		return notificationsServices.find();
	}

	/**
	 * Count New Notifications for connected user.
	 * 
	 * @author HaythemBenizid
	 * @return the long
	 */
	@GetMapping("/count-notifications")
	public Long count() {

		return notificationsServices.count();
	}
}
