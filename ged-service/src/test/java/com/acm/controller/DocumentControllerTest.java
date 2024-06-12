/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.DocumentService;
import com.acm.utils.dtos.GedParameterDTO;

/**
 * {@link DocumentControllerTest} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
class DocumentControllerTest {

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The document controller. */
	@InjectMocks
	private DocumentController documentController;

	/** The document service. */
	@Mock
	private DocumentService documentService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The base path. */
	private String basePath = "/ged/temp";

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
		this.mockMvc = MockMvcBuilders.standaloneSetup(documentController).build();
	}

	/**
	 * Creates the ged parameter DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the ged parameter DTO
	 */
	private GedParameterDTO createGedParameterDTO() {

		List<File> filesToSend = new ArrayList<>();
		List<String> tags = new ArrayList<>();
		tags.add("1");
		MockMultipartFile mockMultipartFile = new MockMultipartFile("files", "FileUploadTest.txt",
				"text/plain", "This is a Test".getBytes());
		File file = CommonFunctions.fileConverter(mockMultipartFile, basePath);
		filesToSend.add(file);
		return new GedParameterDTO(filesToSend, "ACM", tags);
	}

	/**
	 * Shouldupload.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldupload() throws Exception {

		// GIVEN
		GedParameterDTO fileParameters = createGedParameterDTO();
		doReturn(new ArrayList<String>()).when(documentService)
				.uploadToGed(any(GedParameterDTO.class));
		// WHEN
		this.mockMvc
				.perform(post("/documents/upload-ged/")
						.content(CommonFunctions.toJson(fileParameters))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(documentService, times(1)).uploadToGed(any(GedParameterDTO.class));
		verifyNoMoreInteractions(documentService);

	}

	/**
	 * Display document.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void displayDocument() throws Exception {

		// GIVEN
		byte[] data = {1, 2, 3, 4, 5};
		String id = "1";
		given(documentService.displayDocument(any(String.class))).willReturn(data);
		// WHEN
		this.mockMvc.perform(get("/documents/display/" + id).accept(MediaType.ALL))
				.andExpect(status().isOk());
		// THEN
		verify(documentService, times(1)).displayDocument(any(String.class));
		verifyNoMoreInteractions(documentService);
	}

	/**
	 * Should get the document by id.
	 * 
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldGetDocumentById() throws Exception {

		// GIVEN

		// WHEN
		this.mockMvc.perform(get("/documents/document/1").accept(MediaType.ALL))
				.andExpect(status().isOk());
		// THEN

	}

}
