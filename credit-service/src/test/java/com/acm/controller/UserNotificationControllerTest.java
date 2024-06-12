/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.UserNotificationService;
import com.acm.utils.dtos.UsersNotificationsDTO;

/**
 * The class {@link UserNotificationControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class UserNotificationControllerTest {

	/** The user notification controller. */
	@InjectMocks
	private UserNotificationController userNotificationController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The user notification service. */
	@Mock
	private UserNotificationService userNotificationService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(userNotificationController).build();
	}

	/**
	 * Creates the users notifications DTO.
	 *
	 * @return the users notifications DTO
	 */
	private UsersNotificationsDTO createUsersNotificationsDTO() {

		UsersNotificationsDTO usersNotificationsDTO = new UsersNotificationsDTO();
		usersNotificationsDTO.setIdUsersNotification(new Long(1));
		return usersNotificationsDTO;
	}

	/**
	 * Should return list users notifications.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListUsersNotifications() throws Exception {

		// GIVEN
		UsersNotificationsDTO usersNotificationsDTO = new UsersNotificationsDTO();
		given(userNotificationService.find())
				.willReturn(Collections.singletonList(usersNotificationsDTO));

		// WHEN
		this.mockMvc
				.perform(get("/user-notifications/").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userNotificationService, times(1)).find();
		verifyNoMoreInteractions(userNotificationService);
	}

	/**
	 * Should return find by user.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindByUser() throws Exception {

		// GIVEN
		UsersNotificationsDTO usersNotificationsDTO = new UsersNotificationsDTO();
		given(userNotificationService.findByUser(any(UsersNotificationsDTO.class)))
				.willReturn(Collections.singletonList(usersNotificationsDTO));

		// WHEN
		this.mockMvc
				.perform(post("/user-notifications/findByUser")
						.content(CommonFunctions.toJson(usersNotificationsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userNotificationService, times(1)).findByUser(any(UsersNotificationsDTO.class));
		verifyNoMoreInteractions(userNotificationService);
	}

	/**
	 * Should success save users notifications.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveUsersNotifications() throws Exception {

		// GIVEN
		given(userNotificationService.save(any(UsersNotificationsDTO.class)))
				.willReturn(new UsersNotificationsDTO());

		// WHEN
		this.mockMvc
				.perform(post("/user-notifications/create")
						.content(CommonFunctions.toJson(new UsersNotificationsDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userNotificationService, times(1)).save(any(UsersNotificationsDTO.class));
		verifyNoMoreInteractions(userNotificationService);
	}

	/**
	 * Should success update.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		UsersNotificationsDTO usersNotificationsDTO = createUsersNotificationsDTO();
		given(userNotificationService.save(usersNotificationsDTO.getIdUsersNotification(),
				usersNotificationsDTO)).willReturn(usersNotificationsDTO);

		// WHEN
		this.mockMvc
				.perform(put("/user-notifications/update")
						.content(CommonFunctions.toJson(usersNotificationsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userNotificationService, times(1)).save(any(Long.class),
				any(UsersNotificationsDTO.class));
		verifyNoMoreInteractions(userNotificationService);
	}

	/**
	 * Should success update all.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateAll() throws Exception {

		// GIVEN
		List<UsersNotificationsDTO> usersNotificationsDTOs = new ArrayList<>();
		UsersNotificationsDTO usersNotificationsDTO = createUsersNotificationsDTO();
		given(userNotificationService.updateAll(usersNotificationsDTO.getIdUsersNotification(),
				Boolean.TRUE)).willReturn(usersNotificationsDTOs);

		// WHEN
		this.mockMvc
				.perform(put("/user-notifications/update-all/1/0")
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userNotificationService, times(1)).updateAll(any(Long.class), any(Boolean.class));
		verifyNoMoreInteractions(userNotificationService);
	}

}
