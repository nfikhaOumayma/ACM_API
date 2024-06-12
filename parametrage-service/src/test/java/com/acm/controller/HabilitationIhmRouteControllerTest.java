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
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.HabilitationIHMRouteService;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;

/**
 * The Class {@link HabilitationIhmRouteControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class HabilitationIhmRouteControllerTest {

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The habilitation IHM route controller. */
	@InjectMocks
	private HabilitationIHMRouteController habilitationIHMRouteController;

	/** The habilitation IHM route service. */
	@Mock
	private HabilitationIHMRouteService habilitationIHMRouteService;

	/**
	 * Sets the up.
	 * 
	 * @author ManelLamloum
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(habilitationIHMRouteController).build();
	}

	/**
	 * Should return list acm environment DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListAcmEnvironmentDTO() throws Exception {

		// GIVEN
		HabilitationIHMRouteDTO habilitationIHMRouteDTO = new HabilitationIHMRouteDTO();
		given(habilitationIHMRouteService.find(any(HabilitationIHMRouteDTO.class)))
				.willReturn(Collections.singletonList(habilitationIHMRouteDTO));
		// WHEN
		this.mockMvc
				.perform(post("/ihm-route/")
						.content(CommonFunctions.toJson(habilitationIHMRouteDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(habilitationIHMRouteService, times(1)).find(any(HabilitationIHMRouteDTO.class));
		verifyNoMoreInteractions(habilitationIHMRouteService);

	}
}
