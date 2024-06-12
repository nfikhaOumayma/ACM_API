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

import com.acm.service.FinancialAnalysisAbacusService;

/**
 * {@link LoadDataFinancialAnalysisControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataFinancialAnalysisControllerTests {

	/** The load data financial analysis controller. */
	@InjectMocks
	private LoadDataFinancialAnalysisController loadDataFinancialAnalysisController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The financial analysis abacus service. */
	@Mock
	private FinancialAnalysisAbacusService financialAnalysisAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataFinancialAnalysisController).build();
	}

	/**
	 * Should success find financial analysis by loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindFinancialAnalysisByLoan() throws Exception {

		// GIVEN
		given(financialAnalysisAbacusService.find(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/financial-analysis/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(financialAnalysisAbacusService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(financialAnalysisAbacusService);
	}

}
