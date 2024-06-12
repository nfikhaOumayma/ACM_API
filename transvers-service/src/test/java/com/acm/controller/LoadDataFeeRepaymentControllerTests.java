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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.FeeRepaymentService;

/**
 * {@link LoadDataFeeRepaymentControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataFeeRepaymentControllerTests {

	/** The load data fee repayment controller. */
	@InjectMocks
	private LoadDataFeeRepaymentController loadDataFeeRepaymentController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The fee repayment service. */
	@Mock
	private FeeRepaymentService feeRepaymentService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataFeeRepaymentController).build();
	}

	/**
	 * Should success find fee repayment.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindFeeRepayment() throws Exception {

		// GIVEN
		given(feeRepaymentService.findFeeRepayment(1L)).willReturn(any(Long.class));

		// WHEN
		this.mockMvc
				.perform(
						get("/load-data-abacus/fee-repayment/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(feeRepaymentService, times(1)).findFeeRepayment(any(Long.class));
		verifyNoMoreInteractions(feeRepaymentService);
	}

}
