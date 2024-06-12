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
import com.acm.service.ThirdPartyService;
import com.acm.utils.dtos.ScreeningDTO;

/**
 * The class {@link ThirdPartyControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class ThirdPartyControllerTest {

	/** The third party controller. */
	@InjectMocks
	private ThirdPartyController thirdPartyController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The third party service. */
	@Mock
	private ThirdPartyService thirdPartyService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(thirdPartyController).build();
	}

	/**
	 * Should success send AML request.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSendAMLRequest() throws Exception {

		// GIVEN
		ScreeningDTO screeningDTO = new ScreeningDTO();
		given(thirdPartyService.checkAML(any(ScreeningDTO.class))).willReturn(screeningDTO);

		// WHEN
		this.mockMvc
				.perform(post("/third-party/check-aml")
						.content(CommonFunctions.toJson(screeningDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(thirdPartyService, times(1)).checkAML(any(ScreeningDTO.class));
		verifyNoMoreInteractions(thirdPartyService);
	}

	/**
	 * Should success send I score request.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSendIScoreRequest() throws Exception {

		// GIVEN
		ScreeningDTO screeningDTO = new ScreeningDTO();
		given(thirdPartyService.checkIScore(any(ScreeningDTO.class))).willReturn(screeningDTO);

		// WHEN
		this.mockMvc
				.perform(post("/third-party/check-iscore")
						.content(CommonFunctions.toJson(screeningDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(thirdPartyService, times(1)).checkIScore(any(ScreeningDTO.class));
		verifyNoMoreInteractions(thirdPartyService);
	}

}
