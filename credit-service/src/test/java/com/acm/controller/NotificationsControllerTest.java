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
import com.acm.service.NotificationsServices;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.pagination.NotificationsPaginationDTO;

/**
 * The class {@link NotificationsControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class NotificationsControllerTest {

	/** The notifications controller. */
	@InjectMocks
	private NotificationsController notificationsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The notifications services. */
	@Mock
	private NotificationsServices notificationsServices;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(notificationsController).build();
	}

	/**
	 * Creates the notifications DTO.
	 *
	 * @return the notifications DTO
	 */
	private NotificationsDTO createNotificationsDTO() {

		NotificationsDTO notificationsDTO = new NotificationsDTO();
		notificationsDTO.setIdNotification(new Long(1));
		return notificationsDTO;
	}

	/**
	 * Should return list notifications pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListNotificationsPagination() throws Exception {

		// GIVEN
		NotificationsPaginationDTO notificationsPaginationDTO = new NotificationsPaginationDTO();
		given(notificationsServices.find(any(NotificationsPaginationDTO.class)))
				.willReturn(new NotificationsPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/notifications/")
						.content(CommonFunctions.toJson(notificationsPaginationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(notificationsServices, times(1)).find(any(NotificationsPaginationDTO.class));
		verifyNoMoreInteractions(notificationsServices);
	}

	/**
	 * Should success save notifications.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveNotifications() throws Exception {

		// GIVEN
		given(notificationsServices.save(any(NotificationsDTO.class)))
				.willReturn(new NotificationsDTO());

		// WHEN
		this.mockMvc
				.perform(post("/notifications/create")
						.content(CommonFunctions.toJson(new NotificationsDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(notificationsServices, times(1)).save(any(NotificationsDTO.class));
		verifyNoMoreInteractions(notificationsServices);
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
		NotificationsDTO notificationsDTO = createNotificationsDTO();
		given(notificationsServices.save(notificationsDTO.getIdNotification(), notificationsDTO))
				.willReturn(notificationsDTO);

		// WHEN
		this.mockMvc
				.perform(put("/notifications/update")
						.content(CommonFunctions.toJson(notificationsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(notificationsServices, times(1)).save(any(Long.class), any(NotificationsDTO.class));
		verifyNoMoreInteractions(notificationsServices);
	}

	/**
	 * Should return find notifications.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindNotifications() throws Exception {

		// GIVEN
		List<NotificationsDTO> notificationsDTOs = new ArrayList<>();
		given(notificationsServices.find()).willReturn(notificationsDTOs);

		// WHEN
		this.mockMvc
				.perform(
						get("/notifications/find-notifications").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(notificationsServices, times(1)).find();
		verifyNoMoreInteractions(notificationsServices);
	}

	/**
	 * Should return count notifications.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnCountNotifications() throws Exception {

		// GIVEN
		Long count = 1L;
		given(notificationsServices.count()).willReturn(count);

		// WHEN
		this.mockMvc
				.perform(get("/notifications/count-notifications")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(notificationsServices, times(1)).count();
		verifyNoMoreInteractions(notificationsServices);
	}
}
