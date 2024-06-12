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
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;

/**
 * The class {@link CustomerLinkRelationshipControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class CustomerLinkRelationshipControllerTest {

	/** The customer links relationship controller. */
	@InjectMocks
	private CustomerLinksRelationshipController customerLinksRelationshipController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The customer links relationship service. */
	@Mock
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(customerLinksRelationshipController).build();
	}

	/**
	 * Creates the customer links relationship DTO.
	 *
	 * @author HaythemBenizid
	 * @return the customer links relationship DTO
	 */
	private CustomerLinksRelationshipDTO createCustomerLinksRelationshipDTO() {

		CustomerLinksRelationshipDTO customerLinksRelationship = new CustomerLinksRelationshipDTO();
		customerLinksRelationship.setId(new Long(1));
		return customerLinksRelationship;
	}

	/**
	 * Should return list customerLinksRelationship DTO.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCustomerLinksRelationshipDTO() throws Exception {

		// GIVEN
		CustomerLinksRelationshipDTO customerLinksRelationship =
				createCustomerLinksRelationshipDTO();
		given(customerLinksRelationshipService.find(any(CustomerLinksRelationshipDTO.class)))
				.willReturn(Collections.singletonList(customerLinksRelationship));

		// WHEN
		this.mockMvc
				.perform(post("/customer-link-relationship/")
						.content(CommonFunctions.toJson(customerLinksRelationship))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerLinksRelationshipService, times(1))
				.find(any(CustomerLinksRelationshipDTO.class));
		verifyNoMoreInteractions(customerLinksRelationshipService);
	}

	/**
	 * Should return find all members by customer members id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindAllMembersByCustomerMembersId() throws Exception {

		// GIVEN
		CustomerLinksRelationshipDTO customerLinksRelationship = new CustomerLinksRelationshipDTO();
		given(customerLinksRelationshipService
				.findAllMembersByCustomerMembersId(any(CustomerLinksRelationshipDTO.class)))
						.willReturn(Collections.singletonList(customerLinksRelationship));

		// WHEN
		this.mockMvc
				.perform(post("/customer-link-relationship/find-members")
						.content(CommonFunctions.toJson(customerLinksRelationship))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerLinksRelationshipService, times(1))
				.findAllMembersByCustomerMembersId(any(CustomerLinksRelationshipDTO.class));
		verifyNoMoreInteractions(customerLinksRelationshipService);
	}

	/**
	 * Should return find all active guarantors.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindAllActiveGuarantors() throws Exception {

		// GIVEN
		CustomerLinksRelationshipDTO customerLinksRelationship = new CustomerLinksRelationshipDTO();
		given(customerLinksRelationshipService
				.findAllActiveGuarantors(any(CustomerLinksRelationshipDTO.class)))
						.willReturn(Collections.singletonList(customerLinksRelationship));

		// WHEN
		this.mockMvc
				.perform(post("/customer-link-relationship/customer-active-guarantor")
						.content(CommonFunctions.toJson(customerLinksRelationship))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerLinksRelationshipService, times(1))
				.findAllActiveGuarantors(any(CustomerLinksRelationshipDTO.class));
		verifyNoMoreInteractions(customerLinksRelationshipService);
	}

	/**
	 * Should return find all loan guarantors.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindAllLoanGuarantors() throws Exception {

		// GIVEN
		CustomerLinksRelationshipDTO customerLinksRelationship = new CustomerLinksRelationshipDTO();
		given(customerLinksRelationshipService
				.findAllLoanGuarantors(any(CustomerLinksRelationshipDTO.class)))
						.willReturn(Collections.singletonList(customerLinksRelationship));

		// WHEN
		this.mockMvc
				.perform(post("/customer-link-relationship/find-all-loan-guarantors")
						.content(CommonFunctions.toJson(customerLinksRelationship))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerLinksRelationshipService, times(1))
				.findAllLoanGuarantors(any(CustomerLinksRelationshipDTO.class));
		verifyNoMoreInteractions(customerLinksRelationshipService);
	}

	/**
	 * Should return find guarantees.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindGuarantees() throws Exception {

		// GIVEN
		CustomerLinksRelationshipDTO customerLinksRelationship = new CustomerLinksRelationshipDTO();
		given(customerLinksRelationshipService
				.findGuarantees(any(CustomerLinksRelationshipDTO.class)))
						.willReturn(Collections.singletonList(customerLinksRelationship));

		// WHEN
		this.mockMvc
				.perform(post("/customer-link-relationship/find-guarantees")
						.content(CommonFunctions.toJson(customerLinksRelationship))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerLinksRelationshipService, times(1))
				.findGuarantees(any(CustomerLinksRelationshipDTO.class));
		verifyNoMoreInteractions(customerLinksRelationshipService);
	}

	/**
	 * Should success cancel guarantor loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCancelGuarantorLoan() throws Exception {

		// GIVEN

		// WHEN
		this.mockMvc
				.perform(post("/customer-link-relationship/delete-guarantor")
						.content(CommonFunctions.toJson(new CustomerLinksRelationshipDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerLinksRelationshipService, times(1))
				.cancelGuarantorLoan(any(CustomerLinksRelationshipDTO.class));
		verifyNoMoreInteractions(customerLinksRelationshipService);
	}

}
