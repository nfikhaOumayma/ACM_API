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
import com.acm.service.ExpensesService;
import com.acm.utils.dtos.ExpensesCountDTO;
import com.acm.utils.dtos.ExpensesDTO;
import com.acm.utils.dtos.pagination.ExpensesPaginationDTO;

/**
 * The class {@link ExpensesControllerTest}.
 *
 * @author MoezMhiri
 * @since 1.0.8
 */
public class ExpensesControllerTest {

	/** The expenses controller. */
	@InjectMocks
	private ExpensesController expensesController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The expenses service. */
	@Mock
	private ExpensesService expensesService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(expensesController).build();
	}

	/**
	 * Creates the expenses DTO.
	 * 
	 * @author MoezMhiri
	 * @return the expenses DTO
	 */
	private ExpensesDTO createExpensesDTO() {

		ExpensesDTO expensesDTO = new ExpensesDTO();
		expensesDTO.setId(new Long(1));
		return expensesDTO;
	}

	/**
	 * Should success save expenses.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveExpenses() throws Exception {

		// WHEN
		given(expensesService.save(any(ExpensesDTO.class))).willReturn(new ExpensesDTO());

		// WHEN
		this.mockMvc
				.perform(post("/expenses/create").content(CommonFunctions.toJson(new ExpensesDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesService, times(1)).save(any(ExpensesDTO.class));
		verifyNoMoreInteractions(expensesService);
	}

	/**
	 * Should success find documents pagination.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindPagination() throws Exception {

		// GIVEN
		given(expensesService.find(any(ExpensesPaginationDTO.class)))
				.willReturn(new ExpensesPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/expenses/find-pagination")
						.content(CommonFunctions.toJson(new ExpensesPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesService, times(1)).find(any(ExpensesPaginationDTO.class));
		verifyNoMoreInteractions(expensesService);
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
		ExpensesDTO expensesDTO = createExpensesDTO();
		given(expensesService.save(expensesDTO.getId(), expensesDTO)).willReturn(expensesDTO);

		// WHEN
		this.mockMvc
				.perform(put("/expenses/update").content(CommonFunctions.toJson(expensesDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesService, times(1)).save(any(Long.class), any(ExpensesDTO.class));
		verifyNoMoreInteractions(expensesService);
	}

	/**
	 * Should return count.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnCount() throws Exception {

		ExpensesCountDTO expensesCountDTO = new ExpensesCountDTO();
		given(expensesService.count()).willReturn(expensesCountDTO);

		// WHEN
		this.mockMvc.perform(get("/expenses/count").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(expensesService, times(1)).count();
		verifyNoMoreInteractions(expensesService);
	}

}
