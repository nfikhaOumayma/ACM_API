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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.IncentiveSettingRunService;
import com.acm.utils.dtos.IncentiveSettingRunDTO;

/**
 * The class {@link IncentiveSettingRunRunControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingRunControllerTest {

	/** The incentive setting run controller. */
	@InjectMocks
	private IncentiveSettingRunController incentiveSettingRunController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive setting run service. */
	@Mock
	private IncentiveSettingRunService incentiveSettingRunService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveSettingRunController).build();
	}

	/**
	 * Creates the incentive repayment DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveSettingRun DTO
	 */
	private IncentiveSettingRunDTO createIncentiveSettingRunDTO() {

		IncentiveSettingRunDTO incentiveSettingRunDTO = new IncentiveSettingRunDTO();
		incentiveSettingRunDTO.setId(new Long(1));
		incentiveSettingRunDTO.setCode("code");
		return incentiveSettingRunDTO;
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
		IncentiveSettingRunDTO incentiveSettingRunDTO = new IncentiveSettingRunDTO();
		given(incentiveSettingRunService.find(any(IncentiveSettingRunDTO.class)))
				.willReturn(Collections.singletonList(incentiveSettingRunDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-run/")
						.content(CommonFunctions.toJson(incentiveSettingRunDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveSettingRunService, times(1)).find(any(IncentiveSettingRunDTO.class));
		verifyNoMoreInteractions(incentiveSettingRunService);
	}

	/**
	 * Should success find by code.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindByCode() throws Exception {

		// WHEN
		given(incentiveSettingRunService.findByCode(any(String.class)))
				.willReturn(new IncentiveSettingRunDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-run/find-by-code")
						.content(CommonFunctions.toJson(createIncentiveSettingRunDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingRunService, times(1)).findByCode(any(String.class));
		verifyNoMoreInteractions(incentiveSettingRunService);
	}

	/**
	 * Should success update status.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateStatus() throws Exception {

		// WHEN
		given(incentiveSettingRunService.updateStatus(any(IncentiveSettingRunDTO.class)))
				.willReturn(new IncentiveSettingRunDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-run/update-status")
						.content(CommonFunctions.toJson(createIncentiveSettingRunDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingRunService, times(1))
				.updateStatus(any(IncentiveSettingRunDTO.class));
		verifyNoMoreInteractions(incentiveSettingRunService);
	}

	/**
	 * Should success update apply discount or branch.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateApplyDiscountOrBranch() throws Exception {

		// WHEN
		given(incentiveSettingRunService
				.updateApplyDiscountOrBranch(any(IncentiveSettingRunDTO.class)))
						.willReturn(new IncentiveSettingRunDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-setting-run/update-apply-discount-branch")
						.content(CommonFunctions.toJson(createIncentiveSettingRunDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveSettingRunService, times(1))
				.updateApplyDiscountOrBranch(any(IncentiveSettingRunDTO.class));
		verifyNoMoreInteractions(incentiveSettingRunService);
	}

}
