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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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

import com.acm.service.SettingStatutWorkflowService;
import com.acm.utils.dtos.SettingStatutWorkflowDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link SettingStatutWorkflowControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingStatutWorkflowControllerTest {

	/** The setting statut workflow controller. */
	@InjectMocks
	private SettingStatutWorkflowController settingStatutWorkflowController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting statut workflow service. */
	@Mock
	private SettingStatutWorkflowService settingStatutWorkflowService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingStatutWorkflowController).build();
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
	 * Should return list setting statut workflow DTO.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingStatutWorkflowDTO() throws Exception {

		// GIVEN
		SettingStatutWorkflowDTO settingStatutWorkflowDTO = new SettingStatutWorkflowDTO();
		given(settingStatutWorkflowService.find(any(SettingStatutWorkflowDTO.class)))
				.willReturn(Collections.singletonList(settingStatutWorkflowDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-statut-workflows/")
						.content(toJson(settingStatutWorkflowDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingStatutWorkflowService, times(1)).find(any(SettingStatutWorkflowDTO.class));
	}

	/**
	 * Should success find loan process.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanProcess() throws Exception {

		// GIVEN
		SettingStatutWorkflowDTO settingStatutWorkflowDTO = new SettingStatutWorkflowDTO();
		given(settingStatutWorkflowService.find(any(SettingStatutWorkflowDTO.class)))
				.willReturn(Collections.singletonList(settingStatutWorkflowDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-statut-workflows/find-loan-process")
						.content(toJson(settingStatutWorkflowDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingStatutWorkflowService, times(1)).find(any(SettingStatutWorkflowDTO.class));
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
		SettingStatutWorkflowDTO settingStatutWorkflowDTO = new SettingStatutWorkflowDTO();
		given(settingStatutWorkflowService.findAll(any(SettingStatutWorkflowDTO.class)))
				.willReturn(Collections.singletonList(settingStatutWorkflowDTO));

		// WHEN
		this.mockMvc
				.perform(post("/setting-statut-workflows/find-all")
						.content(toJson(settingStatutWorkflowDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(settingStatutWorkflowService, times(1)).findAll(any(SettingStatutWorkflowDTO.class));
		verifyNoMoreInteractions(settingStatutWorkflowService);
	}
}
