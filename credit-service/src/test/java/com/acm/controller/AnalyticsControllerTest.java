/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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

import com.acm.service.AnalyticsService;
import com.acm.utils.dtos.LoanAnalyticsDTO;

/**
 * The class {@link AnalyticsControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class AnalyticsControllerTest {

	/** The analytics controller. */
	@InjectMocks
	private AnalyticsController analyticsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The analytics service. */
	@Mock
	private AnalyticsService analyticsService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(analyticsController).build();
	}

	/**
	 * Should success total applied loans.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessTotalAppliedLoans() throws Exception {

		// GIVEN
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		given(analyticsService.totalAppliedLoans()).willReturn(loanAnalyticsDTO);

		// WHEN
		this.mockMvc
				.perform(get("/analytics/total-applied-loans")
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(analyticsService, times(1)).totalAppliedLoans();
		verifyNoMoreInteractions(analyticsService);
	}

	/**
	 * Should success total approved loans.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessTotalApprovedLoans() throws Exception {

		// GIVEN
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		given(analyticsService.totalApprovedLoans()).willReturn(loanAnalyticsDTO);

		// WHEN
		this.mockMvc
				.perform(get("/analytics/total-approved-loans")
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(analyticsService, times(1)).totalApprovedLoans();
		verifyNoMoreInteractions(analyticsService);
	}

	/**
	 * Should success total canceled rejected loans.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessTotalCanceledRejectedLoans() throws Exception {

		// GIVEN
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		given(analyticsService.totalCanceledRejectedLoans()).willReturn(loanAnalyticsDTO);

		// WHEN
		this.mockMvc
				.perform(get("/analytics/total-canceled-rejected-loans")
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(analyticsService, times(1)).totalCanceledRejectedLoans();
		verifyNoMoreInteractions(analyticsService);
	}

	/**
	 * Should success total loans by products.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessTotalLoansByProducts() throws Exception {

		// GIVEN
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		given(analyticsService.loansByProducts()).willReturn(loanAnalyticsDTO);

		// WHEN
		this.mockMvc
				.perform(get("/analytics/count-loans-products")
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(analyticsService, times(1)).loansByProducts();
		verifyNoMoreInteractions(analyticsService);
	}

	/**
	 * Should success loans stat by months.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoansStatByMonths() throws Exception {

		// GIVEN
		LoanAnalyticsDTO loanAnalyticsDTO = new LoanAnalyticsDTO();
		given(analyticsService.loansStatByMonths()).willReturn(loanAnalyticsDTO);

		// WHEN
		this.mockMvc
				.perform(get("/analytics/loans-stat-months").contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(analyticsService, times(1)).loansStatByMonths();
		verifyNoMoreInteractions(analyticsService);
	}

}
