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
import com.acm.service.SettingDocumentProductService;
import com.acm.utils.dtos.SettingDocumentProductDTO;

/**
 * The Class {@link SettingDocumentProductControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingDocumentProductControllerTest {

	/** The setting document product controller. */
	@InjectMocks
	private SettingDocumentProductController settingDocumentProductController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting document product service. */
	@Mock
	private SettingDocumentProductService settingDocumentProductService;

	/**
	 * Sets the up.
	 * 
	 * @author ManelLamloum
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingDocumentProductController).build();
	}

	/**
	 * Creates the setting document product DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting document product DTO
	 */
	private SettingDocumentProductDTO createSettingDocumentProductDTO() {

		SettingDocumentProductDTO settingDocumentProductDTO = new SettingDocumentProductDTO();
		settingDocumentProductDTO.setId(new Long(1));
		settingDocumentProductDTO.setEnabled(Boolean.TRUE);
		return settingDocumentProductDTO;
	}

	/**
	 * Should success return list setting document product DTO with post.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReturnListSettingDocumentProductDTOWithPost() throws Exception {

		// GIVEN
		SettingDocumentProductDTO settingDocumentProductDTO = new SettingDocumentProductDTO();
		given(settingDocumentProductService.find(any(SettingDocumentProductDTO.class)))
				.willReturn(Collections.singletonList(settingDocumentProductDTO));

		// WHEN
		this.mockMvc
				.perform(post("/setting-document-products/")
						.content(CommonFunctions.toJson(settingDocumentProductDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingDocumentProductService, times(1)).find(any(SettingDocumentProductDTO.class));
		verifyNoMoreInteractions(settingDocumentProductService);
	}

	/**
	 * Should success create setting document product DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCreateSettingDocumentProductDTO() throws Exception {

		// WHEN
		given(settingDocumentProductService.save(any(SettingDocumentProductDTO.class)))
				.willReturn(new SettingDocumentProductDTO());

		// WHEN
		this.mockMvc
				.perform(post("/setting-document-products/create")
						.content(CommonFunctions.toJson(new SettingDocumentProductDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingDocumentProductService, times(1)).save(any(SettingDocumentProductDTO.class));
		verifyNoMoreInteractions(settingDocumentProductService);
	}

	/**
	 * Should success update setting document product DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateSettingDocumentProductDTO() throws Exception {

		// GIVEN
		given(settingDocumentProductService.save(any(Long.class),
				any(SettingDocumentProductDTO.class))).willReturn(new SettingDocumentProductDTO());

		// WHEN
		this.mockMvc
				.perform(put("/setting-document-products/update")
						.content(CommonFunctions.toJson(createSettingDocumentProductDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingDocumentProductService, times(1)).save(any(Long.class),
				any(SettingDocumentProductDTO.class));
		verifyNoMoreInteractions(settingDocumentProductService);
	}

	/**
	 * Should success return list setting document product DTO with get.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReturnListSettingDocumentProductDTOWithGet() throws Exception {

		// GIVEN
		SettingDocumentProductDTO settingDocumentProductDTO = new SettingDocumentProductDTO();
		given(settingDocumentProductService.find())
				.willReturn(Collections.singletonList(settingDocumentProductDTO));

		// WHEN
		this.mockMvc
				.perform(get("/setting-document-products/")
						.content(CommonFunctions.toJson(settingDocumentProductDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingDocumentProductService, times(1)).find();
		verifyNoMoreInteractions(settingDocumentProductService);
	}

	/**
	 * Should success disable document type.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDisableDocumentProduct() throws Exception {

		// GIVEN
		SettingDocumentProductDTO settingDocumentProductDTO = createSettingDocumentProductDTO();
		doNothing().when(settingDocumentProductService).updateStatus(settingDocumentProductDTO,
				Boolean.FALSE);

		// WHEN
		this.mockMvc
				.perform(put("/setting-document-products/disable-document-product")
						.content(CommonFunctions.toJson(settingDocumentProductDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingDocumentProductService, times(1))
				.updateStatus(any(SettingDocumentProductDTO.class), any(Boolean.class));
		verifyNoMoreInteractions(settingDocumentProductService);
	}

	/**
	 * Should success enable document product.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessEnableDocumentProduct() throws Exception {

		// GIVEN
		SettingDocumentProductDTO settingDocumentProductDTO = createSettingDocumentProductDTO();
		doNothing().when(settingDocumentProductService).updateStatus(settingDocumentProductDTO,
				Boolean.FALSE);

		// WHEN
		this.mockMvc
				.perform(put("/setting-document-products/enable-document-product")
						.content(CommonFunctions.toJson(settingDocumentProductDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingDocumentProductService, times(1))
				.updateStatus(any(SettingDocumentProductDTO.class), any(Boolean.class));
		verifyNoMoreInteractions(settingDocumentProductService);
	}
}
