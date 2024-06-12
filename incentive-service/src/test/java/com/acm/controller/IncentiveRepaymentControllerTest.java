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
import com.acm.service.IncentiveRepaymentService;
import com.acm.utils.dtos.IncentiveRepaymentDTO;
import com.acm.utils.dtos.pagination.IncentiveRepaymentPaginationDTO;

/**
 * The class {@link IncentiveRepaymentControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveRepaymentControllerTest {

	/** The incentive repayment controller. */
	@InjectMocks
	private IncentiveRepaymentController incentiveRepaymentController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The incentive repayment service. */
	@Mock
	private IncentiveRepaymentService incentiveRepaymentService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(incentiveRepaymentController).build();
	}

	/**
	 * Creates the incentive repayment DTO.
	 *
	 * @author HaythemBenizid
	 * @return the incentiveRepayment DTO
	 */
	private IncentiveRepaymentDTO createIncentiveRepaymentDTO() {

		IncentiveRepaymentDTO incentiveRepaymentDTO = new IncentiveRepaymentDTO();
		incentiveRepaymentDTO.setId(new Long(1));
		return incentiveRepaymentDTO;
	}

	/**
	 * Should return list incentive repayment DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListDTO() throws Exception {

		// GIVEN
		IncentiveRepaymentDTO incentiveRepaymentDTO = new IncentiveRepaymentDTO();
		given(incentiveRepaymentService.find(any(IncentiveRepaymentDTO.class)))
				.willReturn(Collections.singletonList(incentiveRepaymentDTO));

		// WHEN
		this.mockMvc
				.perform(post("/incentive-repayment/")
						.content(CommonFunctions.toJson(incentiveRepaymentDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(incentiveRepaymentService, times(1)).find(any(IncentiveRepaymentDTO.class));
		verifyNoMoreInteractions(incentiveRepaymentService);
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
		given(incentiveRepaymentService.save(any(IncentiveRepaymentDTO.class)))
				.willReturn(new IncentiveRepaymentDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-repayment/create")
						.content(CommonFunctions.toJson(new IncentiveRepaymentDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRepaymentService, times(1)).save(any(IncentiveRepaymentDTO.class));
		verifyNoMoreInteractions(incentiveRepaymentService);
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
		IncentiveRepaymentDTO incentiveRepaymentDTO = createIncentiveRepaymentDTO();
		given(incentiveRepaymentService.save(incentiveRepaymentDTO.getId(), incentiveRepaymentDTO))
				.willReturn(incentiveRepaymentDTO);

		// WHEN
		this.mockMvc
				.perform(put("/incentive-repayment/update")
						.content(CommonFunctions.toJson(incentiveRepaymentDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRepaymentService, times(1)).save(any(Long.class),
				any(IncentiveRepaymentDTO.class));
		verifyNoMoreInteractions(incentiveRepaymentService);
	}

	/**
	 * Should success find incentive repayment pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIncentiveRepaymentPagination() throws Exception {

		// GIVEN
		given(incentiveRepaymentService.find(any(IncentiveRepaymentPaginationDTO.class)))
				.willReturn(new IncentiveRepaymentPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/incentive-repayment/find-pagination")
						.content(CommonFunctions.toJson(new IncentiveRepaymentPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRepaymentService, times(1))
				.find(any(IncentiveRepaymentPaginationDTO.class));
		verifyNoMoreInteractions(incentiveRepaymentService);
	}

	/**
	 * Should success find incentive repayment by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIncentiveRepaymentById() throws Exception {

		// GIVEN
		IncentiveRepaymentDTO incentiveRepaymentDTO = createIncentiveRepaymentDTO();
		given(incentiveRepaymentService.find(any(Long.class))).willReturn(incentiveRepaymentDTO);

		// WHEN
		this.mockMvc.perform(get("/incentive-repayment/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRepaymentService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(incentiveRepaymentService);
	}

	/**
	 * Should success delete incentive repayment.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteIncentiveRepayment() throws Exception {

		// GIVEN
		doNothing().when(incentiveRepaymentService).delete(new Long(1));

		// WHEN
		this.mockMvc
				.perform(delete("/incentive-repayment/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(incentiveRepaymentService).delete(new Long(1));
		verifyNoMoreInteractions(incentiveRepaymentService);
	}

}
