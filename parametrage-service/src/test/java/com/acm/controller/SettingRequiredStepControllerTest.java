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

import com.acm.service.SettingRequiredStepService;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link SettingRequiredStepControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingRequiredStepControllerTest {

	/** The setting required step controller. */
	@InjectMocks
	private SettingRequiredStepController settingRequiredStepController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting required step service. */
	@Mock
	private SettingRequiredStepService settingRequiredStepService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingRequiredStepController).build();
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
	 * Creates the setting required step DTO.
	 *
	 * @return the setting required step DTO
	 */
	private SettingRequiredStepDTO createSettingRequiredStepDTO() {

		SettingRequiredStepDTO settingRequiredStepDTO = new SettingRequiredStepDTO();
		settingRequiredStepDTO.setId(new Long(1));
		return settingRequiredStepDTO;
	}

	/**
	 * Should return list setting required step DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingRequiredStepDTO() throws Exception {

		// GIVEN
		SettingRequiredStepDTO settingRequiredStepDTO = new SettingRequiredStepDTO();
		given(settingRequiredStepService.find(any(SettingRequiredStepDTO.class)))
				.willReturn(Collections.singletonList(settingRequiredStepDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-required-step/").content(toJson(settingRequiredStepDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingRequiredStepService, times(1)).find(any(SettingRequiredStepDTO.class));
	}

	/**
	 * Should succes create setting required step DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccesCreateSettingRequiredStepDTO() throws Exception {

		// GIVEN
		given(settingRequiredStepService.save(any(SettingRequiredStepDTO.class)))
				.willReturn(new SettingRequiredStepDTO());

		// WHEN
		this.mockMvc
				.perform(post("/setting-required-step/create")
						.content(toJson(new SettingRequiredStepDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingRequiredStepService, times(1)).save(any(SettingRequiredStepDTO.class));
		verifyNoMoreInteractions(settingRequiredStepService);
	}

	/**
	 * Should success update setting required step DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateSettingRequiredStepDTO() throws Exception {

		// GIVEN
		SettingRequiredStepDTO settingRequiredStepDTO = createSettingRequiredStepDTO();
		given(settingRequiredStepService.save(settingRequiredStepDTO.getId(),
				settingRequiredStepDTO)).willReturn(settingRequiredStepDTO);

		// WHEN
		this.mockMvc.perform(put("/setting-required-step/update")
				.content(toJson(settingRequiredStepDTO)).contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingRequiredStepService, times(1)).save(any(Long.class),
				any(SettingRequiredStepDTO.class));
		verifyNoMoreInteractions(settingRequiredStepService);
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
		SettingRequiredStepDTO settingRequiredStepDTO = new SettingRequiredStepDTO();
		given(settingRequiredStepService.find())
				.willReturn(Collections.singletonList(settingRequiredStepDTO));

		// WHEN
		this.mockMvc.perform(get("/setting-required-step/find-all")).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(settingRequiredStepService, times(1)).find();
		verifyNoMoreInteractions(settingRequiredStepService);
	}
}
