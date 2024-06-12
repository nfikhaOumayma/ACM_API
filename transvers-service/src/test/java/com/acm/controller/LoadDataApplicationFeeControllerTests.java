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

import com.acm.service.ApplicationFeeService;

/**
 * {@link LoadDataApplicationFeeControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataApplicationFeeControllerTests {

	/** The load data application fee controller. */
	@InjectMocks
	private LoadDataApplicationFeeController loadDataApplicationFeeController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The application fee service. */
	@Mock
	private ApplicationFeeService applicationFeeService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataApplicationFeeController).build();
	}

	/**
	 * Should success find application fee.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindApplicationFee() throws Exception {

		// GIVEN
		given(applicationFeeService.findApplicationFee(1L)).willReturn(any(Long.class));

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/application-fee/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(applicationFeeService, times(1)).findApplicationFee(any(Long.class));
		verifyNoMoreInteractions(applicationFeeService);
	}

}
