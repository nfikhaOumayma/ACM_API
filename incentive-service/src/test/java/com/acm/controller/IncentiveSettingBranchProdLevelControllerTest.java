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
import com.acm.service.IncentiveSettingBranchProdLevelService;
import com.acm.utils.dtos.IncentiveSettingBranchProdLevelDTO;

/**
 * The class {@link IncentiveSettingBranchProdLevelControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingBranchProdLevelControllerTest {

	/** The incentive setting branch prod level controller. */
	@InjectMocks
	private IncentiveSettingBranchProdLevelController incentiveSettingBranchProdLevelController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive setting branch prod level service. */
	@Mock
	private IncentiveSettingBranchProdLevelService incentiveSettingBranchProdLevelService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc =
				MockMvcBuilders.standaloneSetup(incentiveSettingBranchProdLevelController).build();
	}

	/**
	 * Creates the incentive setting branch prod level DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveSettingBranchProdLevel DTO
	 */
	private IncentiveSettingBranchProdLevelDTO createIncentiveSettingBranchProdLevelDTO() {

		IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO =
				new IncentiveSettingBranchProdLevelDTO();
		incentiveSettingBranchProdLevelDTO.setId(new Long(1));
		return incentiveSettingBranchProdLevelDTO;
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
		IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO =
				new IncentiveSettingBranchProdLevelDTO();
		given(incentiveSettingBranchProdLevelService
				.find(any(IncentiveSettingBranchProdLevelDTO.class)))
						.willReturn(Collections.singletonList(incentiveSettingBranchProdLevelDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-prod-level/")
						.content(CommonFunctions.toJson(incentiveSettingBranchProdLevelDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveSettingBranchProdLevelService, times(1))
				.find(any(IncentiveSettingBranchProdLevelDTO.class));
		verifyNoMoreInteractions(incentiveSettingBranchProdLevelService);
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
		given(incentiveSettingBranchProdLevelService
				.save(any(IncentiveSettingBranchProdLevelDTO.class)))
						.willReturn(new IncentiveSettingBranchProdLevelDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-prod-level/create")
						.content(CommonFunctions.toJson(new IncentiveSettingBranchProdLevelDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingBranchProdLevelService, times(1))
				.save(any(IncentiveSettingBranchProdLevelDTO.class));
		verifyNoMoreInteractions(incentiveSettingBranchProdLevelService);
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
		IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO =
				createIncentiveSettingBranchProdLevelDTO();
		given(incentiveSettingBranchProdLevelService.save(
				incentiveSettingBranchProdLevelDTO.getId(), incentiveSettingBranchProdLevelDTO))
						.willReturn(incentiveSettingBranchProdLevelDTO);

		// WHEN
		this.mockMvc
				.perform(put("/incentive-setting-prod-level/update")
						.content(CommonFunctions.toJson(incentiveSettingBranchProdLevelDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingBranchProdLevelService, times(1)).save(any(Long.class),
				any(IncentiveSettingBranchProdLevelDTO.class));
		verifyNoMoreInteractions(incentiveSettingBranchProdLevelService);
	}

	/**
	 * Should success delete incentive setting branch prod level.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteIncentiveSettingBranchProdLevel() throws Exception {

		// GIVEN
		doNothing().when(incentiveSettingBranchProdLevelService).delete(new Long(1));

		// WHEN
		this.mockMvc.perform(
				delete("/incentive-setting-prod-level/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingBranchProdLevelService).delete(new Long(1));
		verifyNoMoreInteractions(incentiveSettingBranchProdLevelService);
	}

}
