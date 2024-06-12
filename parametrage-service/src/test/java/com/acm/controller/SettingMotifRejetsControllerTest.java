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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.client.TransversClient;
import com.acm.service.SettingMotifRejetsService;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * {@link SettingMotifRejetsControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
public class SettingMotifRejetsControllerTest {

	/** The setting motif rejets controller. */
	@InjectMocks
	private SettingMotifRejetsController settingMotifRejetsController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The motif rejets service. */
	@Mock
	private SettingMotifRejetsService motifRejetsService;

	/** The transvers client. */
	@Mock
	private TransversClient transversClient;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingMotifRejetsController).build();
	}

	/**
	 * To json.
	 *
	 * @param r the r
	 * @return the byte[]
	 * @throws Exception the exception
	 */
	private byte[] toJson(Object r) throws Exception {

		ObjectMapper map = new ObjectMapper();
		return map.writeValueAsBytes(r);
	}

	/**
	 * Creates the setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting motif rejets DTO
	 */
	private SettingMotifRejetsDTO createSettingMotifRejetsDTO() {

		SettingMotifRejetsDTO settingMotifRejetsDTO = new SettingMotifRejetsDTO();
		settingMotifRejetsDTO.setId(new Long(1));
		return settingMotifRejetsDTO;
	}

	/**
	 * Should return list setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListSettingMotifRejetsDTO() throws Exception {

		// GIVEN
		SettingMotifRejetsDTO settingMotifRejets = new SettingMotifRejetsDTO();
		given(motifRejetsService.find(settingMotifRejets))
				.willReturn(Collections.singletonList(settingMotifRejets));

		// WHEN
		this.mockMvc
				.perform(post("/setting-motif-rejets/").content(toJson(settingMotifRejets))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(motifRejetsService, times(1)).find(any(SettingMotifRejetsDTO.class));
		verifyNoMoreInteractions(motifRejetsService);
	}

	/**
	 * Should success create setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessCreateSettingMotifRejetsDTO() throws Exception {

		// GIVEN
		given(motifRejetsService.save(any(SettingMotifRejetsDTO.class)))
				.willReturn(new SettingMotifRejetsDTO());

		// WHEN
		this.mockMvc
				.perform(post("/setting-motif-rejets/create")
						.content(toJson(new SettingMotifRejetsDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(motifRejetsService, times(1)).save(any(SettingMotifRejetsDTO.class));
		verifyNoMoreInteractions(motifRejetsService);
	}

	/**
	 * Should success update setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateSettingMotifRejetsDTO() throws Exception {

		// GIVEN
		SettingMotifRejetsDTO settingMotifRejetsDTO = createSettingMotifRejetsDTO();
		given(motifRejetsService.save(settingMotifRejetsDTO.getId(), settingMotifRejetsDTO))
				.willReturn(new SettingMotifRejetsDTO());

		// WHEN
		this.mockMvc
				.perform(put("/setting-motif-rejets/update").content(toJson(settingMotifRejetsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(motifRejetsService, times(1)).save(any(Long.class),
				any(SettingMotifRejetsDTO.class));
		verifyNoMoreInteractions(motifRejetsService);
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
		SettingMotifRejetsDTO settingMotifRejets = new SettingMotifRejetsDTO();
		given(motifRejetsService.find()).willReturn(Collections.singletonList(settingMotifRejets));

		// WHEN
		this.mockMvc
				.perform(get("/setting-motif-rejets/find-all").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(motifRejetsService, times(1)).find();
		verifyNoMoreInteractions(motifRejetsService);
	}

	/**
	 * Should success find all abacus.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAllAbacus() throws Exception {

		// GIVEN
		SettingMotifRejetsDTO settingMotifRejets = new SettingMotifRejetsDTO();
		given(transversClient.findAllMotifRejet())
				.willReturn(Collections.singletonList(settingMotifRejets));

		// WHEN
		this.mockMvc
				.perform(get("/setting-motif-rejets/find-all-abacus")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(transversClient, times(1)).findAllMotifRejet();
		verifyNoMoreInteractions(motifRejetsService);
	}

	/**
	 * Should success find enabled and disabled data.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindEnabledAndDisabledData() throws Exception {

		// GIVEN
		SettingMotifRejetsDTO settingMotifRejetsDTO = new SettingMotifRejetsDTO();
		given(motifRejetsService.find(settingMotifRejetsDTO))
				.willReturn(Collections.singletonList(settingMotifRejetsDTO));

		// WHEN
		this.mockMvc
				.perform(post("/setting-motif-rejets/find-enabled-and-disabled")
						.content(toJson(settingMotifRejetsDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(motifRejetsService, times(1)).find(any(SettingMotifRejetsDTO.class));
		verifyNoMoreInteractions(motifRejetsService);
	}

	/**
	 * Should success delete data.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteData() throws Exception {

		// GIVEN
		doNothing().when(motifRejetsService).delete(any(Long.class));

		// WHEN
		this.mockMvc
				.perform(
						delete("/setting-motif-rejets/delete/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(motifRejetsService).delete(any(Long.class));
		verifyNoMoreInteractions(motifRejetsService);
	}
}
