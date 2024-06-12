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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.HabilitationIHMRouteService;
import com.acm.service.HabilitationService;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;

/**
 * Unit test class for the controller {@link HabilitationControllerTest}.
 * 
 * @author Haythem benizid
 * @since 0.1.7
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class HabilitationControllerTest {

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The habilitation controller. */
	@InjectMocks
	private HabilitationController habilitationController;

	/** The habilitation service. */
	@Mock
	private HabilitationService habilitationService;

	/** The habilitation IHM route service. */
	@Mock
	private HabilitationIHMRouteService habilitationIHMRouteService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(habilitationController).build();
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Creates the habilitation DTO.
	 *
	 * @return the habilitation DTO
	 */
	private HabilitationDTO createHabilitationDTO() {

		HabilitationDTO habilitationDTO = new HabilitationDTO();
		habilitationDTO.setId(new Long(1));
		habilitationDTO.setIdGroupe(new Long(1));
		habilitationDTO.setAcmHabilitation("PERS_MOR");
		habilitationDTO.setActions("fullControl");
		return habilitationDTO;
	}

	/**
	 * Should return the habilitation of connected user.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnTheHabilitationOfConnectedUser() throws Exception {

		// Given
		HabilitationDTO habilitationDTO = createHabilitationDTO();

		// WHEN
		given(habilitationService.find()).willReturn(Collections.singletonList(habilitationDTO));

		// THEN
		this.mockMvc.perform(get("/habilitations/").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		verify(habilitationService, times(1)).find();
		verifyNoMoreInteractions(habilitationService);
	}

	/**
	 * Should fail when wrong url.
	 * 
	 * @throws Exception the exception
	 */
	@Test
	void shouldFailWhenWrongUrl() throws Exception {

		// THEN
		this.mockMvc.perform(get("/api/habilitations").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().is(404));
	}

	/**
	 * Should success find habilitation IHM route.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindHabilitationIHMRoute() throws Exception {

		// Given
		HabilitationIHMRouteDTO habilitationIHMRouteDTO = new HabilitationIHMRouteDTO();

		// WHEN
		given(habilitationIHMRouteService.find(any(HabilitationIHMRouteDTO.class)))
				.willReturn(Collections.singletonList(habilitationIHMRouteDTO));

		// THEN
		this.mockMvc
				.perform(post("/habilitations/find-ihm-route")
						.content(CommonFunctions.toJson(habilitationIHMRouteDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		verify(habilitationIHMRouteService, times(1)).find(any(HabilitationIHMRouteDTO.class));
		verifyNoMoreInteractions(habilitationIHMRouteService);
	}

	/**
	 * Should success find all.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAll() throws Exception {

		// Given
		HabilitationDTO habilitationDTO = new HabilitationDTO();

		// WHEN
		given(habilitationService.findAll(any(HabilitationDTO.class)))
				.willReturn(Collections.singletonList(habilitationDTO));

		// THEN
		this.mockMvc
				.perform(post("/habilitations/find-all")
						.content(CommonFunctions.toJson(habilitationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		verify(habilitationService, times(1)).findAll(any(HabilitationDTO.class));
		verifyNoMoreInteractions(habilitationService);
	}

	/**
	 * Should success save habilitation DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveHabilitationDTO() throws Exception {

		// GIVEN
		given(habilitationService.save(any(HabilitationDTO.class)))
				.willReturn(new HabilitationDTO());
		// WHEN
		this.mockMvc
				.perform(post("/habilitations/create")
						.content(CommonFunctions.toJson(new HabilitationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(habilitationService, times(1)).save(any(HabilitationDTO.class));
		verifyNoMoreInteractions(habilitationService);
	}

	/**
	 * Should success update all.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessUpdateAll() throws Exception {

		// GIVEN
		List<HabilitationDTO> habilitationDTO = new ArrayList<>();
		given(habilitationService.updateAll(habilitationDTO))
				.willReturn(Collections.singletonList(any(HabilitationDTO.class)));

		// WHEN
		this.mockMvc
				.perform(put("/habilitations/update-all")
						.content(CommonFunctions.toJson(habilitationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(habilitationService, times(1)).updateAll((List<HabilitationDTO>) any(Object.class));
		verifyNoMoreInteractions(habilitationService);
	}
}
