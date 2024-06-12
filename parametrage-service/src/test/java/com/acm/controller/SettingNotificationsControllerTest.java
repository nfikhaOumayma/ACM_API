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

import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.SettingNotificationsService;
import com.acm.utils.dtos.SettingNotificationsDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link SettingNotificationsControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingNotificationsControllerTest {

	/** The setting notifications controller. */
	@InjectMocks
	private SettingNotificationsController settingNotificationsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting notifications service. */
	@Mock
	private SettingNotificationsService settingNotificationsService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingNotificationsController).build();
	}

	/**
	 * To json.
	 *
	 * @param r the r
	 * @return the byte[]
	 * @throws Exception the exception
	 */
	private byte[] toJson(Object r) throws Exception {

		ObjectMapper map = new ObjectMapper();
		return map.writeValueAsBytes(r);
	}

	/**
	 * Creates the setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting notifications DTO
	 */
	private SettingNotificationsDTO createSettingNotificationsDTO() {

		SettingNotificationsDTO settingNotificationsDTO = new SettingNotificationsDTO();
		settingNotificationsDTO.setIdSettingNotification(new Long(1));
		return settingNotificationsDTO;
	}

	/**
	 * Should return list setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingNotificationsDTO() throws Exception {

		// GIVEN
		SettingNotificationsDTO settingNotificationsDTO = new SettingNotificationsDTO();
		given(settingNotificationsService.find(any(SettingNotificationsDTO.class)))
				.willReturn(Collections.singletonList(settingNotificationsDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-notifications/").content(toJson(settingNotificationsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingNotificationsService, times(1)).find(any(SettingNotificationsDTO.class));
	}

	/**
	 * Should success save setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveSettingNotificationsDTO() throws Exception {

		// GIVEN
		given(settingNotificationsService.save(any(SettingNotificationsDTO.class)))
				.willReturn(new SettingNotificationsDTO());
		// WHEN
		this.mockMvc
				.perform(post("/setting-notifications/create")
						.content(toJson(new SettingNotificationsDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingNotificationsService, times(1)).save(any(SettingNotificationsDTO.class));
		verifyNoMoreInteractions(settingNotificationsService);
	}

	/**
	 * Should success update setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateSettingNotificationsDTO() throws Exception {

		// GIVEN
		SettingNotificationsDTO settingNotificationsDTO = createSettingNotificationsDTO();
		given(settingNotificationsService.save(settingNotificationsDTO.getIdSettingNotification(),
				settingNotificationsDTO)).willReturn(settingNotificationsDTO);

		// WHEN
		this.mockMvc
				.perform(put("/setting-notifications/update")
						.content(toJson(settingNotificationsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingNotificationsService, times(1)).save(any(Long.class),
				any(SettingNotificationsDTO.class));
		verifyNoMoreInteractions(settingNotificationsService);
	}

	/**
	 * Should success find all.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAll() throws Exception {

		// GIVEN
		SettingNotificationsDTO settingNotificationsDTO = new SettingNotificationsDTO();
		given(settingNotificationsService.find("GLOBAL"))
				.willReturn(Collections.singletonList(settingNotificationsDTO));
		// WHERE
		this.mockMvc
				.perform(get("/setting-notifications/find-all")
						.content(toJson(settingNotificationsDTO))
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingNotificationsService, times(1)).find("GLOBAL");
		verifyNoMoreInteractions(settingNotificationsService);
	}

}
