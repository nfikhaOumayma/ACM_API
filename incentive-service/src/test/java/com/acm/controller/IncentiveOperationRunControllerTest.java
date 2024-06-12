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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
import com.acm.service.IncentiveOperationRunService;
import com.acm.utils.dtos.IncentiveHistoryDTO;

/**
 * The class {@link IncentiveOperationRunRunControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveOperationRunControllerTest {

	/** The incentive operation run controller. */
	@InjectMocks
	private IncentiveOperationRunController incentiveOperationRunController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive operation run service. */
	@Mock
	private IncentiveOperationRunService incentiveOperationRunService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveOperationRunController).build();
	}

	/**
	 * Should success run incentive calculate.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessRunIncentiveCalculate() throws Exception {

		// GIVEN
		doNothing().when(incentiveOperationRunService).runIncentiveCalculate();

		// WHEN
		this.mockMvc.perform(
				get("/incentive-run-operation/calculate").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationRunService, times(1)).runIncentiveCalculate();
		verifyNoMoreInteractions(incentiveOperationRunService);
	}

	/**
	 * Should success generate incentive report.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessGenerateIncentiveReport() throws Exception {

		// WHEN
		IncentiveHistoryDTO incentiveHistoryDTO = new IncentiveHistoryDTO();
		incentiveHistoryDTO.setYear(2021);
		incentiveHistoryDTO.setMonth(1);
		byte[] reportByte = new byte[1];
		given(incentiveOperationRunService.generateIncentiveReport(any(Integer.class),
				any(Integer.class))).willReturn(reportByte);

		// WHEN
		this.mockMvc
				.perform(post("/incentive-run-operation/report-excel")
						.content(CommonFunctions.toJson(incentiveHistoryDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationRunService, times(1)).generateIncentiveReport(any(Integer.class),
				any(Integer.class));
		verifyNoMoreInteractions(incentiveOperationRunService);
	}

	/**
	 * Should success get run year.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessGetRunYear() throws Exception {

		// GIVEN
		List<Integer> years = new ArrayList<>();
		given(incentiveOperationRunService.getRunYear()).willReturn(years);

		// WHEN
		this.mockMvc.perform(
				get("/incentive-run-operation/get-run-year").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationRunService, times(1)).getRunYear();
		verifyNoMoreInteractions(incentiveOperationRunService);
	}

	/**
	 * Should success get run month.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessGetRunMonth() throws Exception {

		// GIVEN
		List<Integer> months = new ArrayList<>();
		given(incentiveOperationRunService.getRunMonth()).willReturn(months);

		// WHEN
		this.mockMvc.perform(
				get("/incentive-run-operation/get-run-month").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationRunService, times(1)).getRunMonth();
		verifyNoMoreInteractions(incentiveOperationRunService);
	}
}