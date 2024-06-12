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
import com.acm.service.CustomerService;
import com.acm.utils.dtos.LoanDTO;

/**
 * The class {@link GuarantorControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class GuarantorControllerTest {

	/** The guarantor controller. */
	@InjectMocks
	private GuarantorController guarantorController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The customer service. */
	@Mock
	private CustomerService customerService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(guarantorController).build();
	}

	/**
	 * Should success add guarantor.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessAddGuarantor() throws Exception {

		// GIVEN
		LoanDTO loanDTO = new LoanDTO();
		given(customerService.addGuarantors(any(LoanDTO.class))).willReturn(loanDTO);

		// WHEN
		this.mockMvc
				.perform(post("/guarantors/add-guarantors").content(CommonFunctions.toJson(loanDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerService, times(1)).addGuarantors(any(LoanDTO.class));
		verifyNoMoreInteractions(customerService);
	}

}
