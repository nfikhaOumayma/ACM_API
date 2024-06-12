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

import com.acm.service.IScoreAbacusService;

/**
 * {@link LoadDataIScoreControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataIScoreControllerTests {

	/** The load data I score controller. */
	@InjectMocks
	private LoadDataIScoreController loadDataIScoreController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The i score abacus service. */
	@Mock
	private IScoreAbacusService iScoreAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataIScoreController).build();
	}

	/**
	 * Should success find I score list.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIScoreList() throws Exception {

		// GIVEN
		given(iScoreAbacusService.findIScore("", null)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/i-score/").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(iScoreAbacusService, times(1)).findIScore("", null);
		verifyNoMoreInteractions(iScoreAbacusService);
	}

}
