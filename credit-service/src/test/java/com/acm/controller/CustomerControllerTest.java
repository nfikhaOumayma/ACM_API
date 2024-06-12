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
import com.acm.service.CustomerService;
import com.acm.utils.dtos.ArrearsDTO;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.CustomerActiveAccountDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerMezaCardStatutDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.CustomerPaginationDTO;

/**
 * The class {@link CustomerControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public class CustomerControllerTest {

	/** The customer controller. */
	@InjectMocks
	private CustomerController customerController;

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
		this.mockMvc = MockMvcBuilders.standaloneSetup(customerController).build();
	}

	/**
	 * Creates the customer DTO.
	 *
	 * @author HaythemBenizid
	 * @return the customer DTO
	 */
	private CustomerDTO createCustomerDTO() {

		CustomerDTO customerDTO = new CustomerDTO();
		customerDTO.setId(new Long(1));
		return customerDTO;
	}

	/**
	 * Should return list Customer DTO.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCustomers() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = new CustomerDTO();
		given(customerService.findCustomers()).willReturn(Collections.singletonList(customerDTO));

		// WHEN
		this.mockMvc.perform(get("/customers/find-customers").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findCustomers();
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success find customer by id full.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerByIdFull() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = createCustomerDTO();
		given(customerService.find(any(Long.class))).willReturn(customerDTO);

		// WHEN
		this.mockMvc.perform(get("/customers/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(customerService);
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
		CustomerDTO customerDTO = createCustomerDTO();
		given(customerService.findCustomer(any(Long.class))).willReturn(customerDTO);

		// WHEN
		this.mockMvc.perform(get("/customers/find-ustomer/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findCustomer(any(Long.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should return list customer DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCustomerDTO() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = new CustomerDTO();
		given(customerService.find(any(CustomerDTO.class)))
				.willReturn(Collections.singletonList(customerDTO));

		// WHEN
		this.mockMvc
				.perform(post("/customers/").content(CommonFunctions.toJson(customerDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerService, times(1)).find(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success save customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveCustomer() throws Exception {

		// WHEN
		given(customerService.save(any(CustomerDTO.class))).willReturn(new CustomerDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/create")
						.content(CommonFunctions.toJson(new CustomerDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).save(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
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
		CustomerDTO customerDTO = createCustomerDTO();
		given(customerService.save(customerDTO.getId(), customerDTO)).willReturn(customerDTO);

		// WHEN
		this.mockMvc
				.perform(put("/customers/update").content(CommonFunctions.toJson(customerDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).save(any(Long.class), any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success find customer pagination.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerPagination() throws Exception {

		// GIVEN
		given(customerService.find(any(CustomerPaginationDTO.class)))
				.willReturn(new CustomerPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/find-pagination")
						.content(CommonFunctions.toJson(new CustomerPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).find(any(CustomerPaginationDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success find customer pagination for link.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerPaginationForLink() throws Exception {

		// GIVEN
		given(customerService.findForLink(any(CustomerPaginationDTO.class)))
				.willReturn(new CustomerPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/find-pagination-for-link")
						.content(CommonFunctions.toJson(new CustomerPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findForLink(any(CustomerPaginationDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success save for application.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveForApplication() throws Exception {

		// WHEN
		given(customerService.saveForApplication(any(CustomerDTO.class)))
				.willReturn(new CustomerDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/save-for-application")
						.content(CommonFunctions.toJson(new CustomerDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).saveForApplication(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success find customer account.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerAccount() throws Exception {

		// GIVEN
		CustomerAccountDTO customerAccountDTO = new CustomerAccountDTO();
		given(customerService.findCustomerAccount(any(Long.class)))
				.willReturn(Collections.singletonList(customerAccountDTO));

		// WHEN
		this.mockMvc
				.perform(get("/customers/customer-account/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findCustomerAccount(any(Long.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success update for application.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateForApplication() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = createCustomerDTO();
		given(customerService.updateForApplication(customerDTO)).willReturn(customerDTO);

		// WHEN
		this.mockMvc
				.perform(put("/customers/update-for-application")
						.content(CommonFunctions.toJson(customerDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).updateForApplication(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should return list customer member.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListCustomerMember() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = new CustomerDTO();
		given(customerService.findCustomersRelationShip(any(CustomerDTO.class)))
				.willReturn(Collections.singletonList(customerDTO));

		// WHEN
		this.mockMvc
				.perform(post("/customers/find-customer-by-members")
						.content(CommonFunctions.toJson(customerDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerService, times(1)).findCustomersRelationShip(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
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
		Long value = 1L;
		given(customerService.findCustomerActiveAccount(any(Long.class), any(Long.class)))
				.willReturn(value);

		// WHEN
		this.mockMvc.perform(
				get("/customers/customer-active-account/1/2").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findCustomerActiveAccount(any(Long.class),
				any(Long.class));
		verifyNoMoreInteractions(customerService);
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
		CustomerActiveAccountDTO customerActiveAccountDTO = new CustomerActiveAccountDTO();
		given(customerService.findAllActiveAccountsForCustomer(any(Long.class)))
				.willReturn(Collections.singletonList(customerActiveAccountDTO));

		// WHEN
		this.mockMvc.perform(get("/customers/find-all-active-accounts-for-customer/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findAllActiveAccountsForCustomer(any(Long.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success find arrears customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindArrearsCustomer() throws Exception {

		// GIVEN
		ArrearsDTO arrearsDTO = new ArrearsDTO();
		given(customerService.findArrearsCustomer(any(Long.class))).willReturn(arrearsDTO);

		// WHEN
		this.mockMvc.perform(
				get("/customers/data-abacus/customer-arrears/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findArrearsCustomer(any(Long.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success resend login.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessResendLogin() throws Exception {

		// WHEN
		given(customerService.resendLogin(any(CustomerDTO.class))).willReturn(new UserDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/resend-login")
						.content(CommonFunctions.toJson(new CustomerDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).resendLogin(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success check customer loan issued.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCheckCustomerLoanIssued() throws Exception {

		// WHEN
		given(customerService.checkCustomerLoanStatuts(any(CustomerDTO.class)))
				.willReturn(Boolean.TRUE);

		// WHEN
		this.mockMvc
				.perform(post("/customers/check-loan-issued")
						.content(CommonFunctions.toJson(new CustomerDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).checkCustomerLoanStatuts(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
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
		Long value = 1L;
		given(customerService.findCustomerPaidAccount(any(Long.class), any(Long.class)))
				.willReturn(value.doubleValue());

		// WHEN
		this.mockMvc.perform(
				get("/customers/customer-paid-account/1/2").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findCustomerPaidAccount(any(Long.class), any(Long.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success update meza card status.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateMezaCardStatus() throws Exception {

		// WHEN
		given(customerService.updateMezaCardStatus(any(CustomerDTO.class)))
				.willReturn(new CustomerDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/update-meza-card-status")
						.content(CommonFunctions.toJson(new CustomerDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).updateMezaCardStatus(any(CustomerDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should return find by mez card status.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindByMezCardStatus() throws Exception {

		// GIVEN
		CustomerDTO customerDTO = new CustomerDTO();
		given(customerService.findByMezCardStatus(any(String.class)))
				.willReturn(Collections.singletonList(customerDTO));

		// WHEN
		this.mockMvc
				.perform(get("/customers/find-by-mezcard-status/meza")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerService, times(1)).findByMezCardStatus(any(String.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success find customer pagination for meza card.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCustomerPaginationForMezaCard() throws Exception {

		// GIVEN
		given(customerService.findForMezaCard(any(CustomerPaginationDTO.class)))
				.willReturn(new CustomerPaginationDTO());

		// WHEN
		this.mockMvc
				.perform(post("/customers/find-pagination-for-meza-card")
						.content(CommonFunctions.toJson(new CustomerPaginationDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).findForMezaCard(any(CustomerPaginationDTO.class));
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should return Count MEZA card.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnCountMezaCard() throws Exception {

		// GIVEN
		CustomerMezaCardStatutDTO customerMezaCardStatutDTO = new CustomerMezaCardStatutDTO();
		given(customerService.count()).willReturn(customerMezaCardStatutDTO);

		// WHEN
		this.mockMvc.perform(get("/customers/count").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(customerService, times(1)).count();
		verifyNoMoreInteractions(customerService);
	}

	/**
	 * Should success update all for meza.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateAllForMeza() throws Exception {

		// GIVEN
		List<CustomerDTO> customerDTOs = new ArrayList<>();
		// WHEN
		this.mockMvc
				.perform(put("/customers/update-all-customer-meza-card")
						.content(CommonFunctions.toJson(customerDTOs))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(customerService, times(1)).updateAll(customerDTOs);
		verifyNoMoreInteractions(customerService);
	}
}
