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
import com.acm.service.CustomerDecisionService;
import com.acm.utils.dtos.CustomerDecisionDTO;

/**
 * The class {@link CustomerDecisionControllerTest}.
 *
 * @author MoezMhiri
 * @since 0.8.0
 */
class CustomerDecisionControllerTest {

	/** The loan controller. */
	@InjectMocks
	private CustomerDecisionController CustomerDecisionController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan service. */
	@Mock
	private CustomerDecisionService customerDecisionService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(CustomerDecisionController).build();
	}

	/**
	 * Creates the CustomerDecision DTO.
	 * 
	 * @author MoezMhiri
	 * @return the CustomerDecision DTO
	 */
	private CustomerDecisionDTO createCustomerDecisionDTO() {

		CustomerDecisionDTO customerDecisionDTO = new CustomerDecisionDTO();
		customerDecisionDTO.setIdLoan(new Long(1));
		customerDecisionDTO.setId(new Long(1));
		return customerDecisionDTO;
	}

	/**
	 * Should success find customer by id.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerDecisionById() throws Exception {

		// GIVEN
		CustomerDecisionDTO customerDesicionDTO = new CustomerDecisionDTO();
		given(customerDecisionService.find(any(Long.class))).willReturn(customerDesicionDTO);

		// WHEN
		this.mockMvc.perform(get("/customer-decision/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerDecisionService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(customerDecisionService);
	}

	/**
	 * Should return list customer DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCustomerDecisionDTO() throws Exception {

		// GIVEN
		CustomerDecisionDTO customerDesicionDTO = new CustomerDecisionDTO();
		given(customerDecisionService.find(any(CustomerDecisionDTO.class)))
				.willReturn(Collections.singletonList(customerDesicionDTO));

		// WHEN
		this.mockMvc
				.perform(post("/customer-decision/")
						.content(CommonFunctions.toJson(customerDesicionDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerDecisionService, times(1)).find(any(CustomerDecisionDTO.class));
		verifyNoMoreInteractions(customerDecisionService);
	}

	/**
	 * Should success update CustomerDecision.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateCustomerDecision() throws Exception {

		// GIVEN
		CustomerDecisionDTO customerDecisionDTO = createCustomerDecisionDTO();
		given(customerDecisionService.save(customerDecisionDTO.getId(), customerDecisionDTO))
				.willReturn(customerDecisionDTO);

		// WHEN
		this.mockMvc
				.perform(put("/customer-decision/update")
						.content(CommonFunctions.toJson(customerDecisionDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerDecisionService, times(1)).save(any(Long.class),
				any(CustomerDecisionDTO.class));
		verifyNoMoreInteractions(customerDecisionService);
	}

	/**
	 * Should success validate CustomerDecision.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessValidateCustomerDecision() throws Exception {

		given(customerDecisionService.validate(any(CustomerDecisionDTO.class)))
				.willReturn(new CustomerDecisionDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customer-decision/validate")
						.content(CommonFunctions.toJson(new CustomerDecisionDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerDecisionService, times(1)).validate(any(CustomerDecisionDTO.class));
		verifyNoMoreInteractions(customerDecisionService);
	}
}
