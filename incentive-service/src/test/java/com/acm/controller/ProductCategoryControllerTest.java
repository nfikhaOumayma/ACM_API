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
import com.acm.service.ProductCategoryService;
import com.acm.utils.dtos.ProductCategoryDTO;

/**
 * The class {@link ProductCategoryControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class ProductCategoryControllerTest {

	/** The product category controller. */
	@InjectMocks
	private ProductCategoryController productCategoryController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The product category service. */
	@Mock
	private ProductCategoryService productCategoryService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(productCategoryController).build();
	}

	/**
	 * Creates the product category DTO.
	 *
	 * @author HaythemBenizid
	 * @return the productCategory DTO
	 */
	private ProductCategoryDTO createProductCategoryDTO() {

		ProductCategoryDTO productCategoryDTO = new ProductCategoryDTO();
		productCategoryDTO.setId(new Long(1));
		return productCategoryDTO;
	}

	/**
	 * Should return list product category DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListDTO() throws Exception {

		// GIVEN
		ProductCategoryDTO productCategoryDTO = new ProductCategoryDTO();
		given(productCategoryService.find(any(ProductCategoryDTO.class)))
				.willReturn(Collections.singletonList(productCategoryDTO));

		// WHEN
		this.mockMvc
				.perform(post("/product-category/")
						.content(CommonFunctions.toJson(productCategoryDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(productCategoryService, times(1)).find(any(ProductCategoryDTO.class));
		verifyNoMoreInteractions(productCategoryService);
	}

	/**
	 * Should success save product category.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSave() throws Exception {

		// WHEN
		given(productCategoryService.save(any(ProductCategoryDTO.class)))
				.willReturn(new ProductCategoryDTO());

		// WHEN
		this.mockMvc
				.perform(post("/product-category/create")
						.content(CommonFunctions.toJson(new ProductCategoryDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productCategoryService, times(1)).save(any(ProductCategoryDTO.class));
		verifyNoMoreInteractions(productCategoryService);
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
		ProductCategoryDTO productCategoryDTO = createProductCategoryDTO();
		given(productCategoryService.save(productCategoryDTO.getId(), productCategoryDTO))
				.willReturn(productCategoryDTO);

		// WHEN
		this.mockMvc
				.perform(put("/product-category/update")
						.content(CommonFunctions.toJson(productCategoryDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productCategoryService, times(1)).save(any(Long.class),
				any(ProductCategoryDTO.class));
		verifyNoMoreInteractions(productCategoryService);
	}

}
