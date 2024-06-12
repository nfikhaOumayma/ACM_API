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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.AddressSettingService;
import com.acm.utils.dtos.AddressSettingDTO;

/**
 * The Class {@link AddressSettingControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class AddressSettingControllerTest {

	/** The address setting controller. */
	@InjectMocks
	private AddressSettingController addressSettingController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The address setting service. */
	@Mock
	private AddressSettingService addressSettingService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(addressSettingController).build();
	}

	/**
	 * Should success return listaddress setting DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReturnListaddressSettingDTO() throws Exception {

		// GIVEN
		AddressSettingDTO addressSettingDTO = new AddressSettingDTO();
		given(addressSettingService.find(any(AddressSettingDTO.class)))
				.willReturn(Collections.singletonList(addressSettingDTO));
		// WHEN
		this.mockMvc
				.perform(post("/address-settings/")
						.content(CommonFunctions.toJson(addressSettingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(addressSettingService, times(1)).find(any(AddressSettingDTO.class));
		verifyNoMoreInteractions(addressSettingService);
	}

	/**
	 * Should success find address type.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressType() throws Exception {

		// GIVEN
		AddressSettingDTO addressSettingDTO = new AddressSettingDTO();
		given(addressSettingService.find(any(AddressSettingDTO.class)))
				.willReturn(Collections.singletonList(addressSettingDTO));

		// WHEN
		this.mockMvc
				.perform(get("/address-settings/find-address-type")
						.content(CommonFunctions.toJson(addressSettingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(addressSettingService, times(1)).find(any(AddressSettingDTO.class));
		verifyNoMoreInteractions(addressSettingService);

	}

	/**
	 * Should success find setting address.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindSettingAddress() throws Exception {

		// GIVEN
		AddressSettingDTO addressSettingDTO = new AddressSettingDTO();
		given(addressSettingService.find(any(AddressSettingDTO.class)))
				.willReturn(Collections.singletonList(addressSettingDTO));

		// WHEN
		this.mockMvc
				.perform(get("/address-settings/find-settings-address")
						.content(CommonFunctions.toJson(addressSettingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(addressSettingService, times(1)).find(any(AddressSettingDTO.class));
		verifyNoMoreInteractions(addressSettingService);
	}

	/**
	 * Should success find address list.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressList() throws Exception {

		// GIVEN
		AddressSettingDTO addressSettingDTO = new AddressSettingDTO();
		List<String> idAddressListParam = new ArrayList<>();
		given(addressSettingService.find(any(AddressSettingDTO.class)))
				.willReturn(Collections.singletonList(addressSettingDTO));
		// WHEN
		this.mockMvc
				.perform(post("/address-settings/find-address-list")
						.content(CommonFunctions.toJson(idAddressListParam))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(addressSettingService, times(1)).find(any(AddressSettingDTO.class));
		verifyNoMoreInteractions(addressSettingService);
	}

	/**
	 * Should success find address list value.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressListValue() throws Exception {

		// GIVEN
		AddressSettingDTO addressSettingDTO = new AddressSettingDTO();
		given(addressSettingService.find(any(AddressSettingDTO.class)))
				.willReturn(Collections.singletonList(addressSettingDTO));
		// WHEN
		this.mockMvc
				.perform(post("/address-settings/find-address-list-value")
						.content(CommonFunctions.toJson(addressSettingDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(addressSettingService, times(1)).find(any(AddressSettingDTO.class));
		verifyNoMoreInteractions(addressSettingService);

	}

	/**
	 * Should success load setting.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadSetting() throws Exception {

		// GIVEN
		doNothing().when(addressSettingService).loadSettingFromAbacus();
		// WHEN
		this.mockMvc.perform(get("/address-settings/load-setting")).andExpect(status().isOk());
		// THEN
		verify(addressSettingService, times(1)).loadSettingFromAbacus();
		verifyNoMoreInteractions(addressSettingService);
	}

	/**
	 * Should success reset setting.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessResetSetting() throws Exception {

		// GIVEN
		doNothing().when(addressSettingService).loadSettingFromAbacus();
		// WHEN
		this.mockMvc.perform(get("/address-settings/reset-setting")).andExpect(status().isOk());
		// THEN
		verify(addressSettingService, times(1)).resetSettingFromAbacus();
		verifyNoMoreInteractions(addressSettingService);
	}

}
