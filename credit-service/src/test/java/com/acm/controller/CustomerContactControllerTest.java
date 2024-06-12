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
import com.acm.service.CustomerContactService;
import com.acm.utils.dtos.CustomerContactDTO;

/**
 * The class {@link CustomerContactControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class CustomerContactControllerTest {

	/** The customer contact controller. */
	@InjectMocks
	private CustomerContactController customerContactController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The customer contact service. */
	@Mock
	private CustomerContactService customerContactService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(customerContactController).build();
	}

	/**
	 * Creates the customerContact DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the customerContact DTO
	 */
	private CustomerContactDTO createCustomerContactDTO() {

		CustomerContactDTO customerContactDTO = new CustomerContactDTO();
		customerContactDTO.setId(new Long(1));
		return customerContactDTO;
	}

	/**
	 * Should success find customerContact by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerContactById() throws Exception {

		// GIVEN
		CustomerContactDTO customerContactDTO = createCustomerContactDTO();
		given(customerContactService.find(any(Long.class))).willReturn(customerContactDTO);

		// WHEN
		this.mockMvc.perform(get("/customer-contact/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerContactService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(customerContactService);
	}

	/**
	 * Should return list customerContact DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCustomerContactDTO() throws Exception {

		// GIVEN
		CustomerContactDTO customerContactDTO = new CustomerContactDTO();
		given(customerContactService.find(any(CustomerContactDTO.class)))
				.willReturn(Collections.singletonList(customerContactDTO));

		// WHEN
		this.mockMvc
				.perform(post("/customer-contact/")
						.content(CommonFunctions.toJson(customerContactDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerContactService, times(1)).find(any(CustomerContactDTO.class));
		verifyNoMoreInteractions(customerContactService);
	}

	/**
	 * Should success save customerContact.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveCustomerContact() throws Exception {

		// WHEN
		given(customerContactService.save(any(CustomerContactDTO.class)))
				.willReturn(new CustomerContactDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customer-contact/create")
						.content(CommonFunctions.toJson(new CustomerContactDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerContactService, times(1)).save(any(CustomerContactDTO.class));
		verifyNoMoreInteractions(customerContactService);
	}

	/**
	 * Should success create mail.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCreateMail() throws Exception {

		// WHEN
		given(customerContactService.saveMail(any(CustomerContactDTO.class)))
				.willReturn(new CustomerContactDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customer-contact/create-mail")
						.content(CommonFunctions.toJson(new CustomerContactDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerContactService, times(1)).saveMail(any(CustomerContactDTO.class));
		verifyNoMoreInteractions(customerContactService);
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
		CustomerContactDTO customerContactDTO = createCustomerContactDTO();
		given(customerContactService.save(customerContactDTO.getId(), customerContactDTO))
				.willReturn(customerContactDTO);

		// WHEN
		this.mockMvc
				.perform(put("/customer-contact/update")
						.content(CommonFunctions.toJson(customerContactDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerContactService, times(1)).save(any(Long.class),
				any(CustomerContactDTO.class));
		verifyNoMoreInteractions(customerContactService);
	}

	/**
	 * Should success disable message.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDisableMessage() throws Exception {

		// GIVEN
		CustomerContactDTO customerContactDTO = createCustomerContactDTO();

		// WHEN
		this.mockMvc
				.perform(put("/customer-contact/disable-message")
						.content(CommonFunctions.toJson(customerContactDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
	}
}
