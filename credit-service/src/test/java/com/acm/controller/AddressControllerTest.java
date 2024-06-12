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
import com.acm.service.AddressService;
import com.acm.utils.dtos.AddressDTO;

/**
 * The class {@link AddressControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class AddressControllerTest {

	/** The address controller. */
	@InjectMocks
	private AddressController addressController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The address service. */
	@Mock
	private AddressService addressService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(addressController).build();
	}

	/**
	 * Creates the address DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the address DTO
	 */
	private AddressDTO createAddressDTO() {

		AddressDTO addressDTO = new AddressDTO();
		addressDTO.setId(new Long(1));
		return addressDTO;
	}

	/**
	 * Should success find address by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAddressById() throws Exception {

		// GIVEN
		AddressDTO addressDTO = createAddressDTO();
		given(addressService.find(any(Long.class))).willReturn(addressDTO);

		// WHEN
		this.mockMvc.perform(get("/address/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(addressService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(addressService);
	}

	/**
	 * Should return list address DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListAddressDTO() throws Exception {

		// GIVEN
		AddressDTO addressDTO = new AddressDTO();
		given(addressService.find(any(AddressDTO.class)))
				.willReturn(Collections.singletonList(addressDTO));

		// WHEN
		this.mockMvc
				.perform(post("/address/").content(CommonFunctions.toJson(addressDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(addressService, times(1)).find(any(AddressDTO.class));
		verifyNoMoreInteractions(addressService);
	}

	/**
	 * Should success save address.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveAddress() throws Exception {

		// WHEN
		given(addressService.save(any(AddressDTO.class))).willReturn(new AddressDTO());

		// WHEN
		this.mockMvc
				.perform(post("/address/create").content(CommonFunctions.toJson(new AddressDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(addressService, times(1)).save(any(AddressDTO.class));
		verifyNoMoreInteractions(addressService);
	}

	/**
	 * Should success save address by batch.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveAddressByBatch() throws Exception {

		// WHEN
		given(addressService.saveByBatch(any(AddressDTO.class))).willReturn(new AddressDTO());

		// WHEN
		this.mockMvc
				.perform(post("/address/create-by-batch")
						.content(CommonFunctions.toJson(new AddressDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(addressService, times(1)).saveByBatch(any(AddressDTO.class));
		verifyNoMoreInteractions(addressService);
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
		AddressDTO addressDTO = createAddressDTO();
		given(addressService.save(addressDTO.getId(), addressDTO)).willReturn(addressDTO);

		// WHEN
		this.mockMvc
				.perform(put("/address/update").content(CommonFunctions.toJson(addressDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(addressService, times(1)).save(any(Long.class), any(AddressDTO.class));
		verifyNoMoreInteractions(addressService);
	}

	/**
	 * Should success update all.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateAll() throws Exception {

		// GIVEN
		AddressDTO addressDTO = createAddressDTO();
		given(addressService.saveAll(addressDTO)).willReturn(addressDTO);

		// WHEN
		this.mockMvc
				.perform(put("/address/update-all").content(CommonFunctions.toJson(addressDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(addressService, times(1)).saveAll(any(AddressDTO.class));
		verifyNoMoreInteractions(addressService);
	}
}
