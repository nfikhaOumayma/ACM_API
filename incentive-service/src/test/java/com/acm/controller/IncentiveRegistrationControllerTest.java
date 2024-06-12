/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;

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
import com.acm.service.IncentiveRegistrationService;
import com.acm.utils.dtos.IncentiveRegistrationDTO;
import com.acm.utils.dtos.pagination.IncentiveRegistrationPaginationDTO;

/**
 * The class {@link IncentiveRegistrationControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveRegistrationControllerTest {

	/** The incentive registration controller. */
	@InjectMocks
	private IncentiveRegistrationController incentiveRegistrationController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive registration service. */
	@Mock
	private IncentiveRegistrationService incentiveRegistrationService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveRegistrationController).build();
	}

	/**
	 * Creates the incentive registration DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveRegistration DTO
	 */
	private IncentiveRegistrationDTO createIncentiveRegistrationDTO() {

		IncentiveRegistrationDTO incentiveRegistrationDTO = new IncentiveRegistrationDTO();
		incentiveRegistrationDTO.setId(new Long(1));
		return incentiveRegistrationDTO;
	}

	/**
	 * Should return list incentive registration DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListDTO() throws Exception {

		// GIVEN
		IncentiveRegistrationDTO incentiveRegistrationDTO = new IncentiveRegistrationDTO();
		given(incentiveRegistrationService.find(any(IncentiveRegistrationDTO.class)))
				.willReturn(Collections.singletonList(incentiveRegistrationDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-registration/")
						.content(CommonFunctions.toJson(incentiveRegistrationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveRegistrationService, times(1)).find(any(IncentiveRegistrationDTO.class));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}

	/**
	 * Should success save.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSave() throws Exception {

		// WHEN
		given(incentiveRegistrationService.save(any(IncentiveRegistrationDTO.class)))
				.willReturn(new IncentiveRegistrationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-registration/create")
						.content(CommonFunctions.toJson(new IncentiveRegistrationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRegistrationService, times(1)).save(any(IncentiveRegistrationDTO.class));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}

	/**
	 * Should success update.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		IncentiveRegistrationDTO incentiveRegistrationDTO = createIncentiveRegistrationDTO();
		given(incentiveRegistrationService.save(incentiveRegistrationDTO.getId(),
				incentiveRegistrationDTO)).willReturn(incentiveRegistrationDTO);

		// WHEN
		this.mockMvc
				.perform(put("/incentive-registration/update")
						.content(CommonFunctions.toJson(incentiveRegistrationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRegistrationService, times(1)).save(any(Long.class),
				any(IncentiveRegistrationDTO.class));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}

	/**
	 * Should success find incentive registration pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIncentiveRegistrationPagination() throws Exception {

		// GIVEN
		given(incentiveRegistrationService.find(any(IncentiveRegistrationPaginationDTO.class)))
				.willReturn(new IncentiveRegistrationPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-registration/find-pagination")
						.content(CommonFunctions.toJson(new IncentiveRegistrationPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRegistrationService, times(1))
				.find(any(IncentiveRegistrationPaginationDTO.class));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}

	/**
	 * Should success find incentive registration by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIncentiveRegistrationById() throws Exception {

		// GIVEN
		IncentiveRegistrationDTO incentiveRegistrationDTO = createIncentiveRegistrationDTO();
		given(incentiveRegistrationService.find(any(Long.class)))
				.willReturn(incentiveRegistrationDTO);

		// WHEN
		this.mockMvc.perform(get("/incentive-registration/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRegistrationService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}

	/**
	 * Should success delete incentive registration.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteIncentiveRegistration() throws Exception {

		// GIVEN
		doNothing().when(incentiveRegistrationService).delete(new Long(1));

		// WHEN
		this.mockMvc.perform(
				delete("/incentive-registration/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRegistrationService).delete(new Long(1));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}

	/**
	 * Should success save status by product id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveStatusByProductId() throws Exception {

		// GIVEN
		IncentiveRegistrationDTO incentiveRegistrationDTO = createIncentiveRegistrationDTO();
		given(incentiveRegistrationService.saveStatus(incentiveRegistrationDTO))
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-registration/enable")
						.content(CommonFunctions.toJson(incentiveRegistrationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRegistrationService, times(1))
				.saveStatus(any(IncentiveRegistrationDTO.class));
		verifyNoMoreInteractions(incentiveRegistrationService);
	}
}
