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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
import com.acm.service.ExpensesLimitsService;
import com.acm.utils.dtos.ExpensesLimitDTO;

/**
 * The class {@link ExpensesLimitsControllerTest}.
 *
 * @author MoezMhiri
 * @since 1.0.8
 */
public class ExpensesLimitsControllerTest {

	/** The expensesLimits controller. */
	@InjectMocks
	private ExpensesLimitsController expensesLimitsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The expensesLimits service. */
	@Mock
	private ExpensesLimitsService expensesLimitsService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(expensesLimitsController).build();
	}

	/**
	 * Creates the expensesLimits DTO.
	 * 
	 * @author MoezMhiri
	 * @return the expensesLimits DTO
	 */
	private ExpensesLimitDTO createExpensesLimitDTO() {

		ExpensesLimitDTO expensesLimitsDTO = new ExpensesLimitDTO();
		expensesLimitsDTO.setId(new Long(1));
		return expensesLimitsDTO;
	}

	/**
	 * Should return list expensesLimits DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListExpensesLimitDTO() throws Exception {

		// GIVEN
		ExpensesLimitDTO expensesLimitsDTO = new ExpensesLimitDTO();
		given(expensesLimitsService.find(any(ExpensesLimitDTO.class)))
				.willReturn(Collections.singletonList(expensesLimitsDTO));

		// WHEN
		this.mockMvc
				.perform(post("/expenses-limit/").content(CommonFunctions.toJson(expensesLimitsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(expensesLimitsService, times(1)).find(any(ExpensesLimitDTO.class));
		verifyNoMoreInteractions(expensesLimitsService);
	}

	/**
	 * Should success save expensesLimits.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessSaveExpensesLimit() throws Exception {

		// GIVEN
		ExpensesLimitDTO expensesLimitDTO = createExpensesLimitDTO();
		List<ExpensesLimitDTO> expensesLimitDTOs = new ArrayList<>();
		expensesLimitDTOs.add(expensesLimitDTO);
		given(expensesLimitsService.save(expensesLimitDTOs)).willReturn(expensesLimitDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/expenses-limit/save")
						.content(CommonFunctions.toJson(expensesLimitDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesLimitsService, times(1)).save((List<ExpensesLimitDTO>) any(Object.class));
		verifyNoMoreInteractions(expensesLimitsService);
	}

	/**
	 * Should success update.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		ExpensesLimitDTO expensesLimitsDTO = createExpensesLimitDTO();
		given(expensesLimitsService.save(expensesLimitsDTO.getId(), expensesLimitsDTO))
				.willReturn(expensesLimitsDTO);

		// WHEN
		this.mockMvc
				.perform(post("/expenses-limit/update")
						.content(CommonFunctions.toJson(expensesLimitsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesLimitsService, times(1)).save(any(Long.class), any(ExpensesLimitDTO.class));
		verifyNoMoreInteractions(expensesLimitsService);
	}

}
