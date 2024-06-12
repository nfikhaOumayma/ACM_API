/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;

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
import com.acm.service.IncentiveSettingService;
import com.acm.utils.dtos.IncentiveSettingDTO;

/**
 * The class {@link IncentiveSettingControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingControllerTest {

	/** The incentive setting controller. */
	@InjectMocks
	private IncentiveSettingController incentiveSettingController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive setting service. */
	@Mock
	private IncentiveSettingService incentiveSettingService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveSettingController).build();
	}

	/**
	 * Creates the incentive setting DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveSetting DTO
	 */
	private IncentiveSettingDTO createIncentiveSettingDTO() {

		IncentiveSettingDTO incentiveSettingDTO = new IncentiveSettingDTO();
		incentiveSettingDTO.setId(new Long(1));
		return incentiveSettingDTO;
	}

	/**
	 * Should return list DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListDTO() throws Exception {

		// GIVEN
		IncentiveSettingDTO incentiveSettingDTO = new IncentiveSettingDTO();
		given(incentiveSettingService.find(any(IncentiveSettingDTO.class)))
				.willReturn(Collections.singletonList(incentiveSettingDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting/")
						.content(CommonFunctions.toJson(incentiveSettingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveSettingService, times(1)).find(any(IncentiveSettingDTO.class));
		verifyNoMoreInteractions(incentiveSettingService);
	}

	/**
	 * Should success save.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSave() throws Exception {

		// WHEN
		given(incentiveSettingService.save(any(IncentiveSettingDTO.class)))
				.willReturn(new IncentiveSettingDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting/create")
						.content(CommonFunctions.toJson(new IncentiveSettingDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingService, times(1)).save(any(IncentiveSettingDTO.class));
		verifyNoMoreInteractions(incentiveSettingService);
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
		IncentiveSettingDTO incentiveSettingDTO = createIncentiveSettingDTO();
		given(incentiveSettingService.save(incentiveSettingDTO.getId(), incentiveSettingDTO))
				.willReturn(incentiveSettingDTO);

		// WHEN
		this.mockMvc
				.perform(put("/incentive-setting/update")
						.content(CommonFunctions.toJson(incentiveSettingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingService, times(1)).save(any(Long.class),
				any(IncentiveSettingDTO.class));
		verifyNoMoreInteractions(incentiveSettingService);
	}

	/**
	 * Should success delete incentive setting.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteIncentiveSetting() throws Exception {

		// GIVEN
		doNothing().when(incentiveSettingService).delete(new Long(1));

		// WHEN
		this.mockMvc
				.perform(delete("/incentive-setting/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingService).delete(new Long(1));
		verifyNoMoreInteractions(incentiveSettingService);
	}
}
