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
import com.acm.service.GroupeService;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.pagination.GroupePaginationDTO;

/**
 * The Class {@link GroupeControllerTest}.
 * 
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class GroupeControllerTest {

	/** The groupe controller. */
	@InjectMocks
	private GroupeController groupeController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The groupe service. */
	@Mock
	private GroupeService groupeService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(groupeController).build();
	}

	/**
	 * Creates the groupe DTO.
	 * 
	 * @author ManelLamloum
	 * @return the groupe DTO
	 */
	private GroupeDTO createGroupeDTO() {

		GroupeDTO groupeDTO = new GroupeDTO();
		groupeDTO.setId(new Long(1));
		return groupeDTO;
	}

	/**
	 * Should return list groupe DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListGroupeDTO() throws Exception {

		// GIVEN
		GroupeDTO groupeDTO = new GroupeDTO();
		given(groupeService.find(any(GroupeDTO.class)))
				.willReturn(Collections.singletonList(groupeDTO));
		// WHEN
		this.mockMvc
				.perform(post("/groupes/").content(CommonFunctions.toJson(groupeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(groupeService, times(1)).find(any(GroupeDTO.class));
	}

	/**
	 * Should success find all groupes.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAllGroupes() throws Exception {

		// GIVEN
		GroupeDTO groupeDTO = new GroupeDTO();
		given(groupeService.find()).willReturn(Collections.singletonList(groupeDTO));

		// WHEN
		this.mockMvc.perform(get("/groupes/find-all")).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(groupeService, times(1)).find();
		verifyNoMoreInteractions(groupeService);
	}

	/**
	 * Should succes create groupe.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccesCreateGroupe() throws Exception {

		// GIVEN
		given(groupeService.save(any(GroupeDTO.class))).willReturn(new GroupeDTO());

		// WHEN
		this.mockMvc
				.perform(post("/groupes/create").content(CommonFunctions.toJson(new GroupeDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(groupeService, times(1)).save(any(GroupeDTO.class));
		verifyNoMoreInteractions(groupeService);
	}

	/**
	 * Should success update groupe DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateGroupeDTO() throws Exception {

		// GIVEN
		GroupeDTO groupeDTO = createGroupeDTO();
		given(groupeService.save(groupeDTO.getId(), groupeDTO)).willReturn(groupeDTO);

		// WHEN
		this.mockMvc.perform(put("/groupes/update").content(CommonFunctions.toJson(groupeDTO))
				.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// THEN
		verify(groupeService, times(1)).save(any(Long.class), any(GroupeDTO.class));
		verifyNoMoreInteractions(groupeService);
	}

	/**
	 * Should success update enabled.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateEnabled() throws Exception {

		// GIVEN
		GroupeDTO groupeDTO = createGroupeDTO();
		given(groupeService.updateEnabled(groupeDTO)).willReturn(groupeDTO);

		// WHEN
		this.mockMvc.perform(put("/groupes/update-enabled")
				.content(CommonFunctions.toJson(groupeDTO)).contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());

		// THEN
		verify(groupeService, times(1)).updateEnabled(any(GroupeDTO.class));
		verifyNoMoreInteractions(groupeService);
	}

	/**
	 * Should success find groupe pagination.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindGroupePagination() throws Exception {

		// GIVEN
		GroupePaginationDTO groupePaginationDTO = new GroupePaginationDTO();
		given(groupeService.find(any(GroupePaginationDTO.class))).willReturn(groupePaginationDTO);
		// WHEN
		this.mockMvc
				.perform(post("/groupes/find-groupe-pagination")
						.content(CommonFunctions.toJson(groupePaginationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(groupeService, times(1)).find(any(GroupePaginationDTO.class));
	}

	/**
	 * Should success find groupe by code.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindGroupeByCode() throws Exception {

		// GIVEN
		GroupeDTO groupeDTO = createGroupeDTO();
		given(groupeService.findByCode(any(String.class))).willReturn(groupeDTO);

		// WHEN
		this.mockMvc
				.perform(post("/groupes/find-groupe-by-code")
						.content(CommonFunctions.toJson(groupeDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));

		// THEN
		verify(groupeService, times(1)).findByCode(any(String.class));
		verifyNoMoreInteractions(groupeService);
	}

}
