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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.CustomerAbacusService;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.ScheduleDTO;

/**
 * {@link LoadDataCustomerControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public class LoadDataCustomerControllerTests {

	/** The load data customer controller. */
	@InjectMocks
	private LoadDataCustomerController loadDataCustomerController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The customer ABACUS service. */
	@Mock
	private CustomerAbacusService customerAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataCustomerController).build();
	}

	/**
	 * Should success find customer by id.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerById() throws Exception {

		// GIVEN
		given(customerAbacusService.find(any(Long.class))).willReturn(new CustomerDTO());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/customer/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success load customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadCustomer() throws Exception {

		// GIVEN
		List<CustomerDTO> customerDTOs = new ArrayList<>();
		given(customerAbacusService.find()).willReturn(customerDTOs);

		// WHEN
		this.mockMvc
				.perform(get("/load-data-abacus/customer/all").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).find();
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find customer by id loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerByIdLoan() throws Exception {

		// GIVEN
		given(customerAbacusService.findByLoan(any(Long.class))).willReturn(new CustomerDTO());

		// WHEN
		this.mockMvc
				.perform(
						get("/load-data-abacus/customer-loan/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findByLoan(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find account by loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAccountByLoan() throws Exception {

		// GIVEN
		List<CustomerAccountDTO> customerAccountDTOs = new ArrayList<>();
		given(customerAbacusService.findAccountByLoan(any(Long.class)))
				.willReturn(customerAccountDTOs);

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/customer-account/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findAccountByLoan(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find account by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAccountByCustomer() throws Exception {

		// GIVEN
		List<CustomerAccountDTO> customerAccountDTOs = new ArrayList<>();
		given(customerAbacusService.findAccountByCustomer(any(Long.class)))
				.willReturn(customerAccountDTOs);

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/account-by-customer/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findAccountByCustomer(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find customer account schedule by loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerAccountScheduleByLoan() throws Exception {

		// GIVEN
		List<ScheduleDTO> scheduleDTOs = new ArrayList<>();
		given(customerAbacusService.findCustomerAccountScheduleByLoan(any(Long.class)))
				.willReturn(scheduleDTOs);

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/customer-account-schedule/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findCustomerAccountScheduleByLoan(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find members group by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindMembersGroupByCustomer() throws Exception {

		// GIVEN
		List<CustomerMemberDTO> customerMemberDTOs = new ArrayList<>();
		given(customerAbacusService.findMembersGroupByCustomer(any(Long.class)))
				.willReturn(customerMemberDTOs);

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/find-members-group/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findMembersGroupByCustomer(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find members organisation by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindMembersOrganisationByCustomer() throws Exception {

		// GIVEN
		List<CustomerMemberDTO> customerMemberDTOs = new ArrayList<>();
		given(customerAbacusService.findMembersOrganisationByCustomer(any(Long.class)))
				.willReturn(customerMemberDTOs);

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/find-members-organisation/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findMembersOrganisationByCustomer(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find relationship by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindRelationshipByCustomer() throws Exception {

		// GIVEN
		List<CustomerMemberDTO> customerMemberDTOs = new ArrayList<>();
		given(customerAbacusService.findRelationshipByCustomer(any(Long.class)))
				.willReturn(customerMemberDTOs);

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/find-relationship/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findRelationshipByCustomer(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find customer active account.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerActiveAccount() throws Exception {

		// GIVEN
		Long resulat = 1L;
		given(customerAbacusService.findCustomerActiveAccount(any(Long.class), any(Long.class)))
				.willReturn(resulat);

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/find-customer-active-account/1/2")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findCustomerActiveAccount(any(Long.class),
				any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find arrears by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindArrearsByCustomer() throws Exception {

		// GIVEN
		given(customerAbacusService.findArrearsByCustomer(any(Long.class)))
				.willReturn(new ArrearsDTO());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/customer-arrears/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findArrearsByCustomer(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find all active accounts for customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAllActiveAccountsForCustomer() throws Exception {

		// GIVEN
		List<CustomerActiveAccountDTO> customerActiveAccountDTOs = new ArrayList<>();
		given(customerAbacusService.findAllActiveAccountsForCustomer(any(Long.class)))
				.willReturn(customerActiveAccountDTOs);

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/find-all-active-accounts-for-customer/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findAllActiveAccountsForCustomer(any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success find customer paid account.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerPaidAccount() throws Exception {

		// GIVEN
		Double resulat = 1D;
		given(customerAbacusService.findCustomerPaidAccount(any(Long.class), any(Long.class)))
				.willReturn(resulat);

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/find-customer-paid-account/1/2")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).findCustomerPaidAccount(any(Long.class),
				any(Long.class));
		verifyNoMoreInteractions(customerAbacusService);
	}

	/**
	 * Should success load check paiment.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadCheckPaiment() throws Exception {

		// GIVEN
		Map<Long, List<ScheduleDTO>> map = new HashMap<>();
		given(customerAbacusService.loadCheckPaiment(any(String.class))).willReturn(map);

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/customer-check-paiment/customerNumber")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerAbacusService, times(1)).loadCheckPaiment(any(String.class));
		verifyNoMoreInteractions(customerAbacusService);
	}
}
