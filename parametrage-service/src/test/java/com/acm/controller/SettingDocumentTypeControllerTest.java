/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.collection.IsIterableContainingInAnyOrder.containsInAnyOrder;
import static org.junit.Assert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
import com.acm.service.SettingDocumentTypeService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.enums.DocumentTypeCatgory;

/**
 * The Class {@link SettingDocumentTypeControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingDocumentTypeControllerTest {

	/** The setting document type controller. */
	@InjectMocks
	private SettingDocumentTypeController settingDocumentTypeController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting document type service. */
	@Mock
	private SettingDocumentTypeService settingDocumentTypeService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingDocumentTypeController).build();
	}

	/**
	 * Creates the setting document type DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting document type DTO
	 */
	private SettingDocumentTypeDTO createSettingDocumentTypeDTO() {

		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		settingDocumentTypeDTO.setId(new Long(1));
		return settingDocumentTypeDTO;
	}

	/**
	 * Should return list setting document type DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingDocumentTypeDTO() throws Exception {

		// GIVEN
		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		given(settingDocumentTypeService.find(any(SettingDocumentTypeDTO.class)))
				.willReturn(Collections.singletonList(settingDocumentTypeDTO));
		// WHEN
		this.mockMvc
				.perform(post("/setting-document-types/")
						.content(CommonFunctions.toJson(settingDocumentTypeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingDocumentTypeService, times(1)).find(any(SettingDocumentTypeDTO.class));
	}

	/**
	 * Should create setting document type DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldCreateSettingDocumentTypeDTO() throws Exception {

		// GIVEN
		given(settingDocumentTypeService.save(any(SettingDocumentTypeDTO.class)))
				.willReturn(new SettingDocumentTypeDTO());

		// WHEN
		this.mockMvc
				.perform(post("/setting-document-types/create")
						.content(CommonFunctions.toJson(new SettingDocumentTypeDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingDocumentTypeService, times(1)).save(any(SettingDocumentTypeDTO.class));
		verifyNoMoreInteractions(settingDocumentTypeService);
	}

	/**
	 * Should success update setting document type DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateSettingDocumentTypeDTO() throws Exception {

		// GIVEN
		SettingDocumentTypeDTO settingDocumentTypeDTO = createSettingDocumentTypeDTO();
		given(settingDocumentTypeService.save(settingDocumentTypeDTO.getId(),
				settingDocumentTypeDTO)).willReturn(settingDocumentTypeDTO);

		// WHEN
		this.mockMvc
				.perform(put("/setting-document-types/update")
						.content(CommonFunctions.toJson(settingDocumentTypeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingDocumentTypeService, times(1)).save(any(Long.class),
				any(SettingDocumentTypeDTO.class));
		verifyNoMoreInteractions(settingDocumentTypeService);
	}

	/**
	 * Should access load category type.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessLoadCategoryType() throws Exception {

		// GIVEN
		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.LOAN.categoryId(),
				DocumentTypeCatgory.LOAN.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.CLIENT.categoryId(),
				DocumentTypeCatgory.CLIENT.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.ASSIGN_DOCUMENT.categoryId(),
				DocumentTypeCatgory.ASSIGN_DOCUMENT.name()));

		// WHEN
		this.mockMvc
				.perform(get("/setting-document-types/document-type-category")
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		assertTrue(acmStatutsDTOs.size() == 3);
		// Test class property, and its value
		assertThat(acmStatutsDTOs,
				containsInAnyOrder(hasProperty("value", is(DocumentTypeCatgory.LOAN.name())),
						hasProperty("value", is(DocumentTypeCatgory.CLIENT.name())),
						hasProperty("value", is(DocumentTypeCatgory.ASSIGN_DOCUMENT.name()))));
	}

	/**
	 * Should success disable document type.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDisableDocumentType() throws Exception {

		// GIVEN
		SettingDocumentTypeDTO settingDocumentTypeDTO = createSettingDocumentTypeDTO();
		doNothing().when(settingDocumentTypeService).updateStatus(settingDocumentTypeDTO,
				Boolean.FALSE);

		// WHEN
		this.mockMvc
				.perform(put("/setting-document-types/disable-document-type")
						.content(CommonFunctions.toJson(settingDocumentTypeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingDocumentTypeService, times(1)).updateStatus(any(SettingDocumentTypeDTO.class),
				any(Boolean.class));
		verifyNoMoreInteractions(settingDocumentTypeService);
	}

	/**
	 * Should success enable document type.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessEnableDocumentType() throws Exception {

		// GIVEN
		SettingDocumentTypeDTO settingDocumentTypeDTO = createSettingDocumentTypeDTO();
		doNothing().when(settingDocumentTypeService).updateStatus(settingDocumentTypeDTO,
				Boolean.FALSE);

		// WHEN
		this.mockMvc
				.perform(put("/setting-document-types/enable-document-type")
						.content(CommonFunctions.toJson(settingDocumentTypeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(settingDocumentTypeService, times(1)).updateStatus(any(SettingDocumentTypeDTO.class),
				any(Boolean.class));
		verifyNoMoreInteractions(settingDocumentTypeService);
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
		SettingDocumentTypeDTO settingDocumentTypeDTO = createSettingDocumentTypeDTO();
		given(settingDocumentTypeService.find())
				.willReturn(Collections.singletonList(settingDocumentTypeDTO));

		// WHEN
		this.mockMvc
				.perform(get("/setting-document-types/find-all").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingDocumentTypeService, times(1)).find();
		verifyNoMoreInteractions(settingDocumentTypeService);
	}
}
