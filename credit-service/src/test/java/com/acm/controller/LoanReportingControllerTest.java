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
import com.acm.service.LoanService;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingListDTO;

/**
 * The class {@link LoanReportingControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class LoanReportingControllerTest {

	/** The loan reporting controller. */
	@InjectMocks
	private LoanReportingController loanReportingController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan service. */
	@Mock
	private LoanService loanService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loanReportingController).build();
	}

	/**
	 * Should return reporting list.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnReportingList() throws Exception {

		// GIVEN
		ReportingDTO reportingDTO = new ReportingDTO();
		given(loanService.find(any(ReportingDTO.class))).willReturn(new ReportingListDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loans-reporting/reporting-loan-application")
						.content(CommonFunctions.toJson(reportingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanService, times(1)).find(any(ReportingDTO.class));
		verifyNoMoreInteractions(loanService);
	}
}
