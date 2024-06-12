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
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;

/**
 * The Class {@link AcmEnvironnementControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class AcmEnvironnementControllerTest {

	/** The acm environnement controller. */
	@InjectMocks
	private AcmEnvironnementController acmEnvironnementController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The acm environnement service. */
	@Mock
	private AcmEnvironnementService acmEnvironnementService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(acmEnvironnementController).build();
	}

	/**
	 * Creates the acm environnement DTO.
	 *
	 * @author ManelLamloum
	 * @return the acm environnement DTO
	 */
	private AcmEnvironnementDTO createAcmEnvironnementDTO() {

		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		acmEnvironnementDTO.setId(new Long(1));
		acmEnvironnementDTO.setCategory("REMINDER");
		acmEnvironnementDTO.setKey("UnitTest KEY");
		acmEnvironnementDTO.setValue("UnitTest value");
		return acmEnvironnementDTO;
	}

	/**
	 * Should success update limite.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateLimite() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = createAcmEnvironnementDTO();
		given(acmEnvironnementService.updateLimite(any(String.class), any(String.class)))
				.willReturn(acmEnvironnementDTO);

		// WHEN
		this.mockMvc.perform(get("/acm-environnements/updateLimite/" + "key" + "/" + "limite")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// THEN
		verify(acmEnvironnementService, times(1)).updateLimite(any(String.class),
				any(String.class));
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should success find acm environment by key.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAcmEnvironmentByKey() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = createAcmEnvironnementDTO();
		given(acmEnvironnementService.find(any(String.class))).willReturn(acmEnvironnementDTO);

		// WHEN
		this.mockMvc.perform(get("/acm-environnements/'key'").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmEnvironnementService, times(1)).find(any(String.class));
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should success find like by key.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLikeByKey() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		given(acmEnvironnementService.findLikeKey(any(AcmEnvironnementDTO.class)))
				.willReturn(Collections.singletonList(acmEnvironnementDTO));
		// WHEN
		this.mockMvc
				.perform(post("/acm-environnements/find-like-key")
						.content(CommonFunctions.toJson(acmEnvironnementDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(acmEnvironnementService, times(1)).findLikeKey(any(AcmEnvironnementDTO.class));
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should return list acm environment DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListAcmEnvironmentDTO() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		given(acmEnvironnementService.find(any(AcmEnvironnementDTO.class)))
				.willReturn(Collections.singletonList(acmEnvironnementDTO));
		// WHEN
		this.mockMvc
				.perform(post("/acm-environnements/")
						.content(CommonFunctions.toJson(acmEnvironnementDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(acmEnvironnementService, times(1)).find(any(AcmEnvironnementDTO.class));
		verifyNoMoreInteractions(acmEnvironnementService);

	}

	/**
	 * Should return list motifs de rejet.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListMotifsDeRejet() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		given(acmEnvironnementService.find(any(AcmEnvironnementDTO.class)))
				.willReturn(Collections.singletonList(acmEnvironnementDTO));
		// WHERE
		this.mockMvc
				.perform(get("/acm-environnements/find-all")
						.content(CommonFunctions.toJson(acmEnvironnementDTO))
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(acmEnvironnementService, times(1)).find();
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should return list by category.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListByCategory() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		given(acmEnvironnementService.findByCategory(any(String.class)))
				.willReturn(Collections.singletonList(acmEnvironnementDTO));
		// WHERE
		this.mockMvc
				.perform(get("/acm-environnements/find-by-category/REMINDER")
						.content(CommonFunctions.toJson(acmEnvironnementDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(acmEnvironnementService, times(1)).findByCategory(any(String.class));
		verifyNoMoreInteractions(acmEnvironnementService);

	}

	/**
	 * Should success save acm Environnement.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveAcmEnvironnement() throws Exception {

		// GIVEN
		given(acmEnvironnementService.save(any(AcmEnvironnementDTO.class)))
				.willReturn(new AcmEnvironnementDTO());
		// WHEN
		this.mockMvc
				.perform(post("/acm-environnements/create")
						.content(CommonFunctions.toJson(new AcmEnvironnementDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmEnvironnementService, times(1)).save(any(AcmEnvironnementDTO.class));
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should success update.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = createAcmEnvironnementDTO();
		given(acmEnvironnementService.save(acmEnvironnementDTO.getId(), acmEnvironnementDTO))
				.willReturn(acmEnvironnementDTO);

		// WHEN
		this.mockMvc
				.perform(put("/acm-environnements/update")
						.content(CommonFunctions.toJson(acmEnvironnementDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmEnvironnementService, times(1)).save(any(Long.class),
				any(AcmEnvironnementDTO.class));
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should success return list acm environnement DTO.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessReturnListAcmEnvironnementDTO() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		given(acmEnvironnementService.find(any(AcmEnvironnementDTO.class)))
				.willReturn(Collections.singletonList(acmEnvironnementDTO));
		// WHEN
		this.mockMvc
				.perform(post("/acm-environnements/")
						.content(CommonFunctions.toJson(acmEnvironnementDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(acmEnvironnementService, times(1)).find(any(AcmEnvironnementDTO.class));
		verifyNoMoreInteractions(acmEnvironnementService);
	}

	/**
	 * Should success find by keys.
	 *
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindByKeys() throws Exception {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		List<String> keys = new ArrayList<>();
		given(acmEnvironnementService.findByKeys(keys))
				.willReturn(Collections.singletonList(acmEnvironnementDTO));
		// WHEN
		this.mockMvc
				.perform(post("/acm-environnements/find-by-keys")
						.content(CommonFunctions.toJson(keys))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(acmEnvironnementService, times(1)).findByKeys(keys);
		verifyNoMoreInteractions(acmEnvironnementService);
	}
}
