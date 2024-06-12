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
import com.acm.service.SettingLevelService;
import com.acm.utils.dtos.SettingLevelDTO;

/**
 * The Class {@link SettingLevelControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingLevelControllerTest {

	/** The setting level controller. */
	@InjectMocks
	private SettingLevelController settingLevelController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting level service. */
	@Mock
	private SettingLevelService settingLevelService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingLevelController).build();
	}

	/**
	 * Creates the setting level DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting level DTO
	 */
	private SettingLevelDTO createSettingLevelDTO() {

		SettingLevelDTO settingLevelDTO = new SettingLevelDTO();
		settingLevelDTO.setId(new Long(1));
		settingLevelDTO.setEnabled(Boolean.TRUE);
		return settingLevelDTO;
	}

	/**
	 * Should return list setting level DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingLevelDTO() throws Exception {

		// GIVEN
		SettingLevelDTO settingLevelDTO = new SettingLevelDTO();
		given(settingLevelService.find(any(SettingLevelDTO.class)))
				.willReturn(Collections.singletonList(settingLevelDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-level/").content(CommonFunctions.toJson(settingLevelDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingLevelService, times(1)).find(any(SettingLevelDTO.class));
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
		SettingLevelDTO settingLevelDTO = new SettingLevelDTO();
		given(settingLevelService.find()).willReturn(Collections.singletonList(settingLevelDTO));

		// WHEN
		this.mockMvc.perform(get("/setting-level/find-all")).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(settingLevelService, times(1)).find();
		verifyNoMoreInteractions(settingLevelService);
	}

	/**
	 * Should succes create setting level DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccesCreateSettingLevelDTO() throws Exception {

		// GIVEN
		given(settingLevelService.save(any(SettingLevelDTO.class)))
				.willReturn(new SettingLevelDTO());

		// WHEN
		this.mockMvc
				.perform(post("/setting-level/create")
						.content(CommonFunctions.toJson(new SettingLevelDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingLevelService, times(1)).save(any(SettingLevelDTO.class));
		verifyNoMoreInteractions(settingLevelService);
	}

	/**
	 * Should success update setting level DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateSettingLevelDTO() throws Exception {

		// GIVEN
		SettingLevelDTO settingLevelDTO = createSettingLevelDTO();
		given(settingLevelService.save(settingLevelDTO.getId(), settingLevelDTO))
				.willReturn(settingLevelDTO);

		// WHEN
		this.mockMvc.perform(
				put("/setting-level/update").content(CommonFunctions.toJson(settingLevelDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingLevelService, times(1)).save(any(Long.class), any(SettingLevelDTO.class));
		verifyNoMoreInteractions(settingLevelService);
	}

	/**
	 * Should success update order.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateOrder() throws Exception {

		// GIVEN
		List<SettingLevelDTO> settingLevelDTOs = new ArrayList<>();
		given(settingLevelService.updateOrder(settingLevelDTOs)).willReturn(settingLevelDTOs);

		// WHEN
		this.mockMvc.perform(
				put("/setting-level/update-order").content(CommonFunctions.toJson(settingLevelDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingLevelService, times(1)).updateOrder(settingLevelDTOs);
		verifyNoMoreInteractions(settingLevelService);
	}
}
