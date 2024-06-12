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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.AddressSettingAbacusService;

/**
 * {@link LoadDataAddressControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataAddressControllerTests {

	/** The load data address controller. */
	@InjectMocks
	private LoadDataAddressController loadDataAddressController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The address setting abacus service. */
	@Mock
	private AddressSettingAbacusService addressSettingAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataAddressController).build();
	}

	/**
	 * Should success find address list.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressList() throws Exception {

		// GIVEN
		given(addressSettingAbacusService.findAddressList()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/address/find-address-list")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(addressSettingAbacusService, times(1)).findAddressList();
		verifyNoMoreInteractions(addressSettingAbacusService);
	}

	/**
	 * Should success find address list value.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressListValue() throws Exception {

		// GIVEN
		given(addressSettingAbacusService.findAddressListValue()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/address/find-address-list-value")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(addressSettingAbacusService, times(1)).findAddressListValue();
		verifyNoMoreInteractions(addressSettingAbacusService);
	}

	/**
	 * Should success find address type.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressType() throws Exception {

		// GIVEN
		given(addressSettingAbacusService.findAddressType()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/address/find-address-type")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(addressSettingAbacusService, times(1)).findAddressType();
		verifyNoMoreInteractions(addressSettingAbacusService);
	}

	/**
	 * Should success find settings address.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindSettingsAddress() throws Exception {

		// GIVEN
		given(addressSettingAbacusService.findSettingsAddress()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/address/find-settings-address")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(addressSettingAbacusService, times(1)).findSettingsAddress();
		verifyNoMoreInteractions(addressSettingAbacusService);
	}

	/**
	 * Should success load address for customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadAddressForCustomer() throws Exception {

		// GIVEN
		given(addressSettingAbacusService.loadAddressForCustomer(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/address/load-for-customer/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(addressSettingAbacusService, times(1)).loadAddressForCustomer(any(Long.class));
		verifyNoMoreInteractions(addressSettingAbacusService);
	}

	/**
	 * Should success load address by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadAddressByCustomer() throws Exception {

		// GIVEN
		given(addressSettingAbacusService.loadAddressByCustomer(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/address/load-by-customer/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(addressSettingAbacusService, times(1)).loadAddressByCustomer(any(Long.class));
		verifyNoMoreInteractions(addressSettingAbacusService);
	}
}
