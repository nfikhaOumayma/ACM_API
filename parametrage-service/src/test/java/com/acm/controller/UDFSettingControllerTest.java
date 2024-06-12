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

import java.util.Collections;

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

import com.acm.service.UserDefinedFieldGroupService;
import com.acm.service.UserDefinedFieldListValuesService;
import com.acm.service.UserDefinedFieldsService;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link UDFSettingControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class UDFSettingControllerTest {

	/** The udf setting controller. */
	@InjectMocks
	private UDFSettingController udfSettingController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The user defined field group service. */
	@Mock
	private UserDefinedFieldGroupService userDefinedFieldGroupService;

	/** The user defined field list values service. */
	@Mock
	private UserDefinedFieldListValuesService userDefinedFieldListValuesService;

	/** The user defined fields service. */
	@Mock
	private UserDefinedFieldsService userDefinedFieldsService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(udfSettingController).build();
	}

	/**
	 * To json.
	 *
	 * @param r the r
	 * @return the byte[]
	 * @throws Exception the exception
	 */
	private byte[] toJson(Object r) throws Exception {

		ObjectMapper map = new ObjectMapper();
		return map.writeValueAsBytes(r);
	}

	/**
	 * Should return list user defined field group DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListUserDefinedFieldGroupDTO() throws Exception {

		// GIVEN
		UserDefinedFieldGroupDTO userDefinedFieldGroupDTO = new UserDefinedFieldGroupDTO();
		given(userDefinedFieldGroupService.find(any(UserDefinedFieldGroupDTO.class)))
				.willReturn(Collections.singletonList(userDefinedFieldGroupDTO));
		// WHEN
		this.mockMvc
				.perform(post("/udf-settings/udf-group-setting")
						.content(toJson(userDefinedFieldGroupDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userDefinedFieldGroupService, times(1)).find(any(UserDefinedFieldGroupDTO.class));
	}

	/**
	 * Should return list user defined fields DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListUserDefinedFieldsDTO() throws Exception {

		// GIVEN
		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();
		given(userDefinedFieldsService.find(any(UserDefinedFieldsDTO.class)))
				.willReturn(Collections.singletonList(userDefinedFieldsDTO));
		// WHEN
		this.mockMvc
				.perform(post("/udf-settings/udf-fields-setting")
						.content(toJson(userDefinedFieldsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userDefinedFieldsService, times(1)).find(any(UserDefinedFieldsDTO.class));
	}

	/**
	 * Should return list user defined field list values DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListUserDefinedFieldListValuesDTO() throws Exception {

		// GIVEN
		UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO =
				new UserDefinedFieldListValuesDTO();
		given(userDefinedFieldListValuesService.find(any(UserDefinedFieldListValuesDTO.class)))
				.willReturn(Collections.singletonList(userDefinedFieldListValuesDTO));
		// WHEN
		this.mockMvc
				.perform(post("/udf-settings/udf-list-value")
						.content(toJson(userDefinedFieldListValuesDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userDefinedFieldListValuesService, times(1))
				.find(any(UserDefinedFieldListValuesDTO.class));
	}

	/**
	 * Should success load group setting from abacus.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadGroupSettingFromAbacus() throws Exception {

		// GIVEN
		doNothing().when(userDefinedFieldGroupService).loadSettingFromAbacus(new UserDTO());
		// WHEN
		this.mockMvc.perform(get("/udf-settings/load-group-setting")).andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldGroupService, times(1)).loadSettingFromAbacus(new UserDTO());
	}

	/**
	 * Should success load list value setting from abacus.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadListValueSettingFromAbacus() throws Exception {

		// GIVEN
		doNothing().when(userDefinedFieldListValuesService).loadSettingFromAbacus(new UserDTO());
		// WHEN
		this.mockMvc.perform(get("/udf-settings/load-list-value-setting"))
				.andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldListValuesService, times(1)).loadSettingFromAbacus(new UserDTO());
	}

	/**
	 * Should success load fields setting from abacus.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadFieldsSettingFromAbacus() throws Exception {

		// GIVEN
		doNothing().when(userDefinedFieldsService).loadSettingFromAbacus(new UserDTO());
		// WHEN
		this.mockMvc.perform(get("/udf-settings/load-fields-setting")).andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldsService, times(1)).loadSettingFromAbacus(new UserDTO());
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
		doNothing().when(userDefinedFieldListValuesService).resetSettingFromAbacus();
		// WHEN
		this.mockMvc.perform(get("/udf-settings/reset-setting")).andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldListValuesService, times(1)).resetSettingFromAbacus();
	}

	/**
	 * Should success find by id UDF list value.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindByIdUDFListValue() throws Exception {

		// GIVEN
		UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO =
				new UserDefinedFieldListValuesDTO();
		given(userDefinedFieldListValuesService.findByIdUDFListValue(any(Long.class)))
				.willReturn(Collections.singletonList(userDefinedFieldListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/udf-settings/find-by-idUDF-list-value/1")
						.content(toJson(userDefinedFieldListValuesDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldListValuesService, times(1)).findByIdUDFListValue(any(Long.class));
		verifyNoMoreInteractions(userDefinedFieldListValuesService);
	}
}
