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
import com.acm.service.IncentiveSettingConstantService;
import com.acm.utils.dtos.IncentiveSettingConstantDTO;

/**
 * The class {@link IncentiveSettingConstantControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingConstantControllerTest {

	/** The incentive setting constant controller. */
	@InjectMocks
	private IncentiveSettingConstantController incentiveSettingConstantController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive setting constant service. */
	@Mock
	private IncentiveSettingConstantService incentiveSettingConstantService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveSettingConstantController).build();
	}

	/**
	 * Creates the incentive setting branch prod level DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveSettingConstant DTO
	 */
	private IncentiveSettingConstantDTO createIncentiveSettingConstantDTO() {

		IncentiveSettingConstantDTO incentiveSettingConstantDTO = new IncentiveSettingConstantDTO();
		incentiveSettingConstantDTO.setId(new Long(1));
		return incentiveSettingConstantDTO;
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
		IncentiveSettingConstantDTO incentiveSettingConstantDTO = new IncentiveSettingConstantDTO();
		given(incentiveSettingConstantService.find(any(IncentiveSettingConstantDTO.class)))
				.willReturn(Collections.singletonList(incentiveSettingConstantDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-constant/")
						.content(CommonFunctions.toJson(incentiveSettingConstantDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveSettingConstantService, times(1))
				.find(any(IncentiveSettingConstantDTO.class));
		verifyNoMoreInteractions(incentiveSettingConstantService);
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
		given(incentiveSettingConstantService.save(any(IncentiveSettingConstantDTO.class)))
				.willReturn(new IncentiveSettingConstantDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-constant/create")
						.content(CommonFunctions.toJson(new IncentiveSettingConstantDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingConstantService, times(1))
				.save(any(IncentiveSettingConstantDTO.class));
		verifyNoMoreInteractions(incentiveSettingConstantService);
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
		IncentiveSettingConstantDTO incentiveSettingConstantDTO =
				createIncentiveSettingConstantDTO();
		given(incentiveSettingConstantService.save(incentiveSettingConstantDTO.getId(),
				incentiveSettingConstantDTO)).willReturn(incentiveSettingConstantDTO);

		// WHEN
		this.mockMvc
				.perform(put("/incentive-setting-constant/update")
						.content(CommonFunctions.toJson(incentiveSettingConstantDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingConstantService, times(1)).save(any(Long.class),
				any(IncentiveSettingConstantDTO.class));
		verifyNoMoreInteractions(incentiveSettingConstantService);
	}

	/**
	 * Should success find by categories.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessFindByCategories() throws Exception {

		// GIVEN
		List<String> categories = new ArrayList<>();
		given(incentiveSettingConstantService.findByCategories((List<String>) any(Object.class)))
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-constant/find-by-categories")
						.content(CommonFunctions.toJson(categories))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveSettingConstantService, times(1))
				.findByCategories((List<String>) any(Object.class));
		verifyNoMoreInteractions(incentiveSettingConstantService);
	}

}
