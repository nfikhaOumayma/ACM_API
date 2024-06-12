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
import com.acm.service.ProductService;
import com.acm.utils.dtos.ProductDTO;

/**
 * The Class {@link ProductControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class ProductControllerTest {

	/** The product controller. */
	@InjectMocks
	private ProductController productController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The product service. */
	@Mock
	private ProductService productService;

	/**
	 * Sets the up.
	 * 
	 * @author ManelLamloum
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(productController).build();
	}

	/**
	 * Creates the product DTO.
	 *
	 * @author ManelLamloum
	 * @return the product DTO
	 */
	private ProductDTO createProductDTO() {

		ProductDTO productDTO = new ProductDTO();
		productDTO.setId(new Long(1));
		return productDTO;
	}

	/**
	 * Should success find by id.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindById() throws Exception {

		// GIVEN
		ProductDTO productDTO = createProductDTO();
		given(productService.find(any(Long.class))).willReturn(productDTO);

		// WHEN
		this.mockMvc
				.perform(get("/products/" + productDTO.getId()).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(productService);
	}

	/**
	 * Should return list of product DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListOfProductDTO() throws Exception {

		// GIVEN
		ProductDTO productDTO = new ProductDTO();
		given(productService.find(any(ProductDTO.class)))
				.willReturn(Collections.singletonList(productDTO));
		// WHEN
		this.mockMvc
				.perform(post("/products/").content(CommonFunctions.toJson(productDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(productService, times(1)).find(any(ProductDTO.class));
		verifyNoMoreInteractions(productService);
	}

	/**
	 * Should success create product.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCreateProduct() throws Exception {

		// GIVEN
		given(productService.save(any(ProductDTO.class))).willReturn(new ProductDTO());
		// WHEN
		this.mockMvc
				.perform(post("/products/create").content(CommonFunctions.toJson(new ProductDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productService, times(1)).save(any(ProductDTO.class));
		verifyNoMoreInteractions(productService);
	}

	/**
	 * Should succes update product DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccesUpdateProductDTO() throws Exception {

		// GIVEN
		ProductDTO productDTO = createProductDTO();
		given(productService.save(productDTO.getId(), productDTO)).willReturn(productDTO);

		// WHEN
		this.mockMvc
				.perform(put("/products/update").content(CommonFunctions.toJson(productDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productService, times(1)).save(any(Long.class), any(ProductDTO.class));
		verifyNoMoreInteractions(productService);
	}

	/**
	 * Should success find all.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAll() throws Exception {

		// GIVEN
		ProductDTO productDTO = new ProductDTO();
		given(productService.find()).willReturn((Collections.singletonList(productDTO)));

		// WHEN
		this.mockMvc
				.perform(get("/products/find-all").content(CommonFunctions.toJson(productDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productService, times(1)).find();
		verifyNoMoreInteractions(productService);

	}

	/**
	 * Should success reload setting.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void ShouldSuccessReloadSetting() throws Exception {

		// GIVEN
		ProductDTO productDTO = new ProductDTO();
		given(productService.loadSettingFromAbacus()).willReturn(new String());

		// WHEN
		this.mockMvc
				.perform(get("/products/reload-setting").content(CommonFunctions.toJson(productDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productService, times(1)).loadSettingFromAbacus();
		verifyNoMoreInteractions(productService);
	}
}
