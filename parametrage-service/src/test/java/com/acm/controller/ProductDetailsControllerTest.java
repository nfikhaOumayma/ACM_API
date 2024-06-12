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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
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
import com.acm.service.ProductDetailsService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ProductDetailsDTO;

/**
 * The Class {@link ProductDetailsControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class ProductDetailsControllerTest {

	/** The product details controller. */
	@InjectMocks
	private ProductDetailsController productDetailsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The product details service. */
	@Mock
	private ProductDetailsService productDetailsService;

	/**
	 * Sets the up.
	 * 
	 * @author ManelLamloum
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(productDetailsController).build();
	}

	/**
	 * Creates the product details DTO.
	 * 
	 * @author ManelLamloum
	 * @return the product details DTO
	 */
	private ProductDetailsDTO createProductDetailsDTO() {

		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		productDetailsDTO.setId(new Long(1));
		return productDetailsDTO;
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
		ProductDetailsDTO productDetailsDTO = createProductDetailsDTO();
		given(productDetailsService.find(any(Long.class))).willReturn(productDetailsDTO);

		// WHEN
		this.mockMvc.perform(get("/product-details/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productDetailsService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(productDetailsService);
	}

	/**
	 * Should return list of product details DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListOfProductDetailsDTO() throws Exception {

		// GIVEN
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		given(productDetailsService.find(any(ProductDetailsDTO.class)))
				.willReturn(Collections.singletonList(productDetailsDTO));

		// WHEN
		this.mockMvc
				.perform(post("/product-details/")
						.content(CommonFunctions.toJson(productDetailsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(productDetailsService, times(1)).find(any(ProductDetailsDTO.class));
		verifyNoMoreInteractions(productDetailsService);
	}

	/**
	 * Should success create product details DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCreateProductDetailsDTO() throws Exception {

		// WHEN
		given(productDetailsService.save(any(ProductDetailsDTO.class)))
				.willReturn(new ProductDetailsDTO());

		// WHEN
		this.mockMvc
				.perform(post("/product-details/create")
						.content(CommonFunctions.toJson(new LoanDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productDetailsService, times(1)).save(any(ProductDetailsDTO.class));
		verifyNoMoreInteractions(productDetailsService);
	}

	/**
	 * Should success update product details DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateProductDetailsDTO() throws Exception {

		// GIVEN
		given(productDetailsService.save(any(Long.class), any(ProductDetailsDTO.class)))
				.willReturn(new ProductDetailsDTO());

		// WHEN
		this.mockMvc
				.perform(put("/product-details/update")
						.content(CommonFunctions.toJson(createProductDetailsDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(productDetailsService, times(1)).save(any(Long.class), any(ProductDetailsDTO.class));
		verifyNoMoreInteractions(productDetailsService);
	}

	/**
	 * Should success delete by id.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteById() throws Exception {

		// GIVEN
		doNothing().when(productDetailsService).deleteAll(any(ProductDetailsDTO.class));

		// WHEN
		this.mockMvc.perform(delete("/product-details/1")).andExpect(status().isOk());
		// THEN
		verify(productDetailsService).deleteAll(any(ProductDetailsDTO.class));
		verifyNoMoreInteractions(productDetailsService);
	}
}
