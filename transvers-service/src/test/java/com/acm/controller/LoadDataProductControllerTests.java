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

import com.acm.service.ProductAbacusServices;
import com.acm.utils.dtos.ProductDTO;

/**
 * {@link LoadDataProductControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataProductControllerTests {

	/** The load data product controller. */
	@InjectMocks
	private LoadDataProductController loadDataProductController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The product abacus services. */
	@Mock
	private ProductAbacusServices productAbacusServices;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataProductController).build();
	}

	/**
	 * Should success find product by ID.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindProductByID() throws Exception {

		// GIVEN
		given(productAbacusServices.find(1L)).willReturn(new ProductDTO());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/product/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productAbacusServices, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(productAbacusServices);
	}

	/**
	 * Should success find all products.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAllProducts() throws Exception {

		// GIVEN
		given(productAbacusServices.find()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/products/").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productAbacusServices, times(1)).find();
		verifyNoMoreInteractions(productAbacusServices);
	}

}
