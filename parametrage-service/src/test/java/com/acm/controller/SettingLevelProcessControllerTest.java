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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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

import com.acm.constants.common.CommonFunctions;
import com.acm.service.SettingLevelProcessService;
import com.acm.utils.dtos.SettingLevelProcessDTO;

/**
 * {@link SettingLevelProcessControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingLevelProcessControllerTest {

	/** The setting level process controller. */
	@InjectMocks
	private SettingLevelProcessController settingLevelProcessController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting level process service. */
	@Mock
	private SettingLevelProcessService settingLevelProcessService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingLevelProcessController).build();
	}

	/**
	 * Creates the setting level process DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting level process DTO
	 */
	private SettingLevelProcessDTO createSettingLevelProcessDTO() {

		SettingLevelProcessDTO settingLevelProcessDTO = new SettingLevelProcessDTO();
		settingLevelProcessDTO.setId(new Long(1));
		settingLevelProcessDTO.setIdProduct(1L);
		settingLevelProcessDTO.setAmount(BigDecimal.TEN);
		return settingLevelProcessDTO;
	}

	/**
	 * Should return list groupe DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingLevelProcessDTO() throws Exception {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = createSettingLevelProcessDTO();
		given(settingLevelProcessService.find(settingLevelProcessDTO))
				.willReturn(Collections.singletonList(settingLevelProcessDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-level-process/")
						.content(CommonFunctions.toJson(settingLevelProcessDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingLevelProcessService, times(1)).find(any(SettingLevelProcessDTO.class));
	}

	@Test
	void shouldSuccessFindSetting() throws Exception {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = createSettingLevelProcessDTO();
		given(settingLevelProcessService.findSetting(settingLevelProcessDTO))
				.willReturn(Collections.singletonList(settingLevelProcessDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-level-process/find-setting")
						.content(CommonFunctions.toJson(settingLevelProcessDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingLevelProcessService, times(1)).findSetting(any(SettingLevelProcessDTO.class));
	}

	@Test
	void shouldScceessSaveSettingLevelProcessDTO() throws Exception {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = createSettingLevelProcessDTO();
		given(settingLevelProcessService.save(settingLevelProcessDTO))
				.willReturn(settingLevelProcessDTO);
		// WHEN
		this.mockMvc
				.perform(post("/setting-level-process/create")
						.content(CommonFunctions.toJson(settingLevelProcessDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingLevelProcessService, times(1)).save(any(SettingLevelProcessDTO.class));
	}

	@Test
	void shoulSuccessUpdateSettingLevelProcessDTO() throws Exception {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = createSettingLevelProcessDTO();

		given(settingLevelProcessService.save(settingLevelProcessDTO.getId(),
				settingLevelProcessDTO)).willReturn(settingLevelProcessDTO);

		// WHEN
		this.mockMvc.perform(put("/setting-level-process/update")
				.content(CommonFunctions.toJson(settingLevelProcessDTO))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingLevelProcessService, times(1)).save(any(Long.class),
				any(SettingLevelProcessDTO.class));
		verifyNoMoreInteractions(settingLevelProcessService);
	}

	@SuppressWarnings("unchecked")
	@Test
	void shoulSuccessUpdateAmountSettingLevelProcessDTO() throws Exception {

		// GIVEN
		List<SettingLevelProcessDTO> settingLevelProcessDTOs = new ArrayList<>();

		given(settingLevelProcessService.updateAmount(settingLevelProcessDTOs))
				.willReturn(any(Boolean.class));

		// WHEN
		this.mockMvc.perform(put("/setting-level-process/update-amount")
				.content(CommonFunctions.toJson(settingLevelProcessDTOs))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingLevelProcessService, times(1))
				.updateAmount((List<SettingLevelProcessDTO>) any(Object.class));
		verifyNoMoreInteractions(settingLevelProcessService);
	}

}
