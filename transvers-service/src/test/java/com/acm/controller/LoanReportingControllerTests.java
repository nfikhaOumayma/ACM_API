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

import java.util.ArrayList;

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
import com.acm.service.LoanReportingService;
import com.acm.utils.dtos.ReportingDTO;

/**
 * {@link LoanReportingControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoanReportingControllerTests {

	@InjectMocks
	private LoanReportingController loanReportingController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	@Mock
	private LoanReportingService loanReportingService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loanReportingController).build();
	}

	/**
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindSchedulesStatus() throws Exception {

		// GIVEN
		given(loanReportingService.findSchedulesStatus(new ReportingDTO()))
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc
				.perform(post("/load-data-abacus/reporting-schedules-status")
						.content(CommonFunctions.toJson(new ReportingDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanReportingService, times(1)).findSchedulesStatus(any(ReportingDTO.class));
		verifyNoMoreInteractions(loanReportingService);
	}

}
