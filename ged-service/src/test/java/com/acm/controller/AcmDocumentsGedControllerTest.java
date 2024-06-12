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
import com.acm.service.AcmDocumentsGedService;
import com.acm.utils.dtos.AcmDocumentsGedDTO;

/**
 * The class {@link AcmDocumentsGedControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class AcmDocumentsGedControllerTest {

	/** The acm documents ged controller. */
	@InjectMocks
	private AcmDocumentsGedController acmDocumentsGedController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The acm documents ged service. */
	@Mock
	private AcmDocumentsGedService acmDocumentsGedService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(acmDocumentsGedController).build();
	}

	/**
	 * Should return list acmDocumentsGed DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListAcmDocumentsGedDTO() throws Exception {

		// GIVEN
		AcmDocumentsGedDTO acmDocumentsGedDTO = new AcmDocumentsGedDTO();
		given(acmDocumentsGedService.find(any(AcmDocumentsGedDTO.class)))
				.willReturn(Collections.singletonList(acmDocumentsGedDTO));

		// WHEN
		this.mockMvc
				.perform(post("/documents-ged/").content(CommonFunctions.toJson(acmDocumentsGedDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(acmDocumentsGedService, times(1)).find(any(AcmDocumentsGedDTO.class));
		verifyNoMoreInteractions(acmDocumentsGedService);
	}

	/**
	 * Should success disable all.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDisableAll() throws Exception {

		// GIVEN

		// WHEN
		this.mockMvc.perform(get("/documents-ged/disable-all").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmDocumentsGedService, times(1)).disableAll();
		verifyNoMoreInteractions(acmDocumentsGedService);
	}

	/**
	 * Should success count all.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCountAll() throws Exception {

		// GIVEN

		// WHEN
		this.mockMvc.perform(get("/documents-ged/count-all").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmDocumentsGedService, times(1)).countAll();
		verifyNoMoreInteractions(acmDocumentsGedService);
	}

	/**
	 * Should success save acm documents ged.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveAcmDocumentsGed() throws Exception {

		// WHEN
		given(acmDocumentsGedService.findPhotoClient(any(Long.class))).willReturn(new byte[1]);

		// WHEN
		this.mockMvc
				.perform(get("/documents-ged/photo-client/1").content(CommonFunctions.toJson(1L))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmDocumentsGedService, times(1)).findPhotoClient(any(Long.class));
		verifyNoMoreInteractions(acmDocumentsGedService);
	}

}
