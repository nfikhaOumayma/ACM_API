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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.multipart.MultipartFile;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.AMLDataService;
import com.acm.utils.dtos.AMLDataDTO;

/**
 * The Class AMLDataControllerTest.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class AMLDataControllerTest {

	/** The aml data controller. */
	@InjectMocks
	private AMLDataController amlDataController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The aml data service. */
	@Mock
	private AMLDataService amlDataService;

	/** The base path. */
	private String basePath = "/ged/temp";

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(amlDataController).build();
	}

	/**
	 * Should success return list AML data.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReturnListAMLData() throws Exception {

		// GIVEN
		AMLDataDTO amlDataDTO = new AMLDataDTO();
		given(amlDataService.find(any(AMLDataDTO.class)))
				.willReturn(Collections.singletonList(amlDataDTO));
		// WHEN
		this.mockMvc
				.perform(post("/aml-settings/").content(CommonFunctions.toJson(amlDataDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(amlDataService, times(1)).find(any(AMLDataDTO.class));
		verifyNoMoreInteractions(amlDataService);
	}

	/**
	 * Should success check AML.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void ShouldSuccessCheckAML() throws Exception {

		// GIVEN
		AMLDataDTO amlDataDTO = new AMLDataDTO();
		amlDataDTO.setName("test");
		given(amlDataService.checkAML(amlDataDTO))
				.willReturn(Collections.singletonList(amlDataDTO));
		// WHEN
		this.mockMvc
				.perform(post("/aml-settings/check-aml").content(CommonFunctions.toJson(amlDataDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(amlDataService, times(1)).checkAML(any(AMLDataDTO.class));
		verifyNoMoreInteractions(amlDataService);
	}

	/**
	 * Should success upload aml file.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	@Disabled
	void shouldSuccessUploadAmlFile() throws Exception {

		// GIVEN
		MockMultipartFile mockMultipartFile = new MockMultipartFile("files", "FileUploadTest.txt",
				"text/plain", "This is a Test".getBytes());
		File file = CommonFunctions.fileConverter(mockMultipartFile, basePath);
		doNothing().when(amlDataService).uploadAmlFile(any(MultipartFile[].class));

		// WHEN
		this.mockMvc.perform(post("/aml-settings/upload-aml-file").content(file.toString()))
				.andExpect(status().isOk());

		// THEN
		verify(amlDataService, times(1)).uploadAmlFile(any(MultipartFile[].class));
		verifyNoMoreInteractions(amlDataService);
	}
}
