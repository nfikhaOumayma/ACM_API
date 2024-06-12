/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
import com.acm.service.SettingClientService;

/**
 * The Class {@link SettingClientControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingClientControllerTest {

	/** The setting client controller. */
	@InjectMocks
	private SettingClientController settingClientController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting client service. */
	@Mock
	private SettingClientService settingClientService;

	/**
	 * Sets the up.
	 * 
	 * @author ManelLamloum
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingClientController).build();
	}

	/**
	 * Should success save image to ged.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	@Disabled
	void shouldSuccessSaveImageToGed() throws Exception {

		// GIVEN
		MockMultipartFile mockMultipartFile = new MockMultipartFile("files", "FileUploadTest.txt",
				"text/plain", "This is a Test".getBytes());
		MockMultipartFile mockMultipartFiles[] = new MockMultipartFile[1];
		mockMultipartFiles[0] = mockMultipartFile;
		doNothing().when(settingClientService).save((MultipartFile[]) any(Object.class));

		// WHEN
		this.mockMvc.perform(post("/setting-client/save-client-image-to-ged",
				CommonFunctions.toJson(mockMultipartFiles)).contentType(MediaType.APPLICATION_JSON)
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingClientService, times(1)).save((MultipartFile[]) any(Object.class));
		verifyNoMoreInteractions(settingClientService);
	}

}
