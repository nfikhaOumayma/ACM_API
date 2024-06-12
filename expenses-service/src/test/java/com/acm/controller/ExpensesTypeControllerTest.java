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
import com.acm.service.ExpensesTypeService;
import com.acm.utils.dtos.ExpensesTypeDTO;

/**
 * The class {@link ExpensesTypeControllerTest}.
 *
 * @author MoezMhiri
 * @since 1.0.8
 */
public class ExpensesTypeControllerTest {

	/** The expensesType controller. */
	@InjectMocks
	private ExpensesTypeController expensesTypeController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The expensesType service. */
	@Mock
	private ExpensesTypeService expensesTypeService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(expensesTypeController).build();
	}

	/**
	 * Creates the expensesType DTO.
	 * 
	 * @author MoezMhiri
	 * @return the expensesType DTO
	 */
	private ExpensesTypeDTO createExpensesTypeDTO() {

		ExpensesTypeDTO expensesTypeDTO = new ExpensesTypeDTO();
		expensesTypeDTO.setId(new Long(1));
		return expensesTypeDTO;
	}

	/**
	 * Should return list expensesType DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListExpensesTypeDTO() throws Exception {

		// Given
		ExpensesTypeDTO expensesTypeDTO = createExpensesTypeDTO();

		// WHEN
		given(expensesTypeService.findAll()).willReturn(Collections.singletonList(expensesTypeDTO));

		// THEN
		this.mockMvc.perform(get("/expenses-type/").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		verify(expensesTypeService, times(1)).findAll();
		verifyNoMoreInteractions(expensesTypeService);
	}

	/**
	 * Should success save expensesType.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveExpensesTypeLoan() throws Exception {

		// WHEN
		given(expensesTypeService.save(any(ExpensesTypeDTO.class)))
				.willReturn(new ExpensesTypeDTO());

		// WHEN
		this.mockMvc
				.perform(post("/expenses-type/create")
						.content(CommonFunctions.toJson(new ExpensesTypeDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesTypeService, times(1)).save(any(ExpensesTypeDTO.class));
		verifyNoMoreInteractions(expensesTypeService);
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
		ExpensesTypeDTO acmExpensesTypeDTO = createExpensesTypeDTO();
		given(expensesTypeService.save(acmExpensesTypeDTO.getId(), acmExpensesTypeDTO))
				.willReturn(acmExpensesTypeDTO);

		// WHEN
		this.mockMvc
				.perform(post("/expenses-type/update")
						.content(CommonFunctions.toJson(acmExpensesTypeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesTypeService, times(1)).save(any(Long.class), any(ExpensesTypeDTO.class));
		verifyNoMoreInteractions(expensesTypeService);
	}

	/**
	 * Should success delete expensesType.
	 *
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteExpensesType() throws Exception {

		// GIVEN
		doNothing().when(expensesTypeService).delete(new Long(1));

		// WHEN
		this.mockMvc.perform(delete("/expenses-type/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(expensesTypeService).delete(new Long(1));
		verifyNoMoreInteractions(expensesTypeService);
	}

}
