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
import com.acm.service.IncentiveOperationService;
import com.acm.utils.dtos.IncentiveOperationDTO;
import com.acm.utils.dtos.pagination.IncentiveOperationPaginationDTO;

/**
 * The class {@link IncentiveOperationControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveOperationControllerTest {

	/** The incentive operation controller. */
	@InjectMocks
	private IncentiveOperationController incentiveOperationController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive operation service. */
	@Mock
	private IncentiveOperationService incentiveOperationService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveOperationController).build();
	}

	/**
	 * Creates the incentive operation DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveOperation DTO
	 */
	private IncentiveOperationDTO createIncentiveOperationDTO() {

		IncentiveOperationDTO incentiveOperationDTO = new IncentiveOperationDTO();
		incentiveOperationDTO.setId(new Long(1));
		return incentiveOperationDTO;
	}

	/**
	 * Should return list incentive operation DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListDTO() throws Exception {

		// GIVEN
		IncentiveOperationDTO incentiveOperationDTO = new IncentiveOperationDTO();
		given(incentiveOperationService.find(any(IncentiveOperationDTO.class)))
				.willReturn(Collections.singletonList(incentiveOperationDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-operation/")
						.content(CommonFunctions.toJson(incentiveOperationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveOperationService, times(1)).find(any(IncentiveOperationDTO.class));
		verifyNoMoreInteractions(incentiveOperationService);
	}

	/**
	 * Should success save product category.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSave() throws Exception {

		// WHEN
		given(incentiveOperationService.save(any(IncentiveOperationDTO.class)))
				.willReturn(new IncentiveOperationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-operation/create")
						.content(CommonFunctions.toJson(new IncentiveOperationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationService, times(1)).save(any(IncentiveOperationDTO.class));
		verifyNoMoreInteractions(incentiveOperationService);
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
		IncentiveOperationDTO incentiveOperationDTO = createIncentiveOperationDTO();
		given(incentiveOperationService.save(incentiveOperationDTO.getId(), incentiveOperationDTO))
				.willReturn(incentiveOperationDTO);

		// WHEN
		this.mockMvc
				.perform(put("/incentive-operation/update")
						.content(CommonFunctions.toJson(incentiveOperationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationService, times(1)).save(any(Long.class),
				any(IncentiveOperationDTO.class));
		verifyNoMoreInteractions(incentiveOperationService);
	}

	/**
	 * Should success find incentive operation pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIncentiveOperationPagination() throws Exception {

		// GIVEN
		given(incentiveOperationService.find(any(IncentiveOperationPaginationDTO.class)))
				.willReturn(new IncentiveOperationPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-operation/find-pagination")
						.content(CommonFunctions.toJson(new IncentiveOperationPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationService, times(1))
				.find(any(IncentiveOperationPaginationDTO.class));
		verifyNoMoreInteractions(incentiveOperationService);
	}

	/**
	 * Should success find incentive operation by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIncentiveOperationById() throws Exception {

		// GIVEN
		IncentiveOperationDTO incentiveOperationDTO = createIncentiveOperationDTO();
		given(incentiveOperationService.find(any(Long.class))).willReturn(incentiveOperationDTO);

		// WHEN
		this.mockMvc.perform(get("/incentive-operation/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(incentiveOperationService);
	}

	/**
	 * Should success delete incentive operation.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteIncentiveOperation() throws Exception {

		// GIVEN
		doNothing().when(incentiveOperationService).delete(new Long(1));

		// WHEN
		this.mockMvc
				.perform(delete("/incentive-operation/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationService).delete(new Long(1));
		verifyNoMoreInteractions(incentiveOperationService);
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
		IncentiveOperationDTO incentiveOperationDTO = createIncentiveOperationDTO();
		given(incentiveOperationService.saveStatus(incentiveOperationDTO))
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-operation/enable")
						.content(CommonFunctions.toJson(incentiveOperationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveOperationService, times(1)).saveStatus(any(IncentiveOperationDTO.class));
		verifyNoMoreInteractions(incentiveOperationService);
	}
}
