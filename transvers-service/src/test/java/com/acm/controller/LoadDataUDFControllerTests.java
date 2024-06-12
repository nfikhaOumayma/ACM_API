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

import com.acm.service.UDFSettingAbacusService;

/**
 * {@link LoadDataUDFControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataUDFControllerTests {

	/** The load data UDF controller. */
	@InjectMocks
	private LoadDataUDFController loadDataUDFController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The udf setting abacus service. */
	@Mock
	private UDFSettingAbacusService udfSettingAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataUDFController).build();
	}

	/**
	 * Should success find user defined fields.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUserDefinedFields() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.findUserDefinedFields()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/udf/find-udf-fields").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).findUserDefinedFields();
		verifyNoMoreInteractions(udfSettingAbacusService);
	}

	/**
	 * Should success find user defined field group.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUserDefinedFieldGroup() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.findUserDefinedFieldGroup()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/udf/find-udf-group").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).findUserDefinedFieldGroup();
		verifyNoMoreInteractions(udfSettingAbacusService);
	}

	/**
	 * Should success find user defined field list values.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindUserDefinedFieldListValues() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.findUserDefinedFieldListValues())
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/udf/find-udf-list-values")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).findUserDefinedFieldListValues();
		verifyNoMoreInteractions(udfSettingAbacusService);
	}

	/**
	 * Should success load UDF for loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadUDFForLoan() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.loadUDFForLoan(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/udf/load-udf-loan/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).loadUDFForLoan(any(Long.class));
		verifyNoMoreInteractions(udfSettingAbacusService);
	}

	/**
	 * Should success load UDF by loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadUDFByLoan() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.loadUDFByLoan(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/udf/load-udf-by-loan/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).loadUDFByLoan(any(Long.class));
		verifyNoMoreInteractions(udfSettingAbacusService);
	}

	/**
	 * Should success load UDF for customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadUDFForCustomer() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.loadUDFForCustomer(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/udf/load-udf-customer/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).loadUDFForCustomer(any(Long.class));
		verifyNoMoreInteractions(udfSettingAbacusService);
	}

	/**
	 * Should success load UDF by customer.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLoadUDFByCustomer() throws Exception {

		// GIVEN
		given(udfSettingAbacusService.loadUDFByCustomer(1L)).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/udf/load-udf-by-customer/1")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(udfSettingAbacusService, times(1)).loadUDFByCustomer(any(Long.class));
		verifyNoMoreInteractions(udfSettingAbacusService);
	}
}
