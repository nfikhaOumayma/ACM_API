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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.utils.dtos.LoansUdfDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * The class {@link UDFLinksControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class UDFLinksControllerTest {

	/** The user defined fields links controller. */
	@InjectMocks
	private UserDefinedFieldsLinksController userDefinedFieldsLinksController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The user defined fields link service. */
	@Mock
	private UserDefinedFieldsLinksService userDefinedFieldsLinkService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(userDefinedFieldsLinksController).build();
	}

	/**
	 * Creates the user defined fields links DTO.
	 *
	 * @return the user defined fields links DTO
	 */
	private UserDefinedFieldsLinksDTO createUserDefinedFieldsLinksDTO() {

		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		userDefinedFieldsLinksDTO.setId(new Long(1));
		return userDefinedFieldsLinksDTO;
	}

	/**
	 * Should success save user defined fields links.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveUserDefinedFieldsLinks() throws Exception {

		// GIVEN
		given(userDefinedFieldsLinkService.save(any(UserDefinedFieldsLinksDTO.class)))
				.willReturn(new UserDefinedFieldsLinksDTO());

		// WHEN
		this.mockMvc
				.perform(post("/udf-links/create")
						.content(CommonFunctions.toJson(new UserDefinedFieldsLinksDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldsLinkService, times(1)).save(any(UserDefinedFieldsLinksDTO.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}

	/**
	 * Should success save by batch user defined fields links.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveByBatchUserDefinedFieldsLinks() throws Exception {

		// GIVEN
		given(userDefinedFieldsLinkService.saveByBatch(any(UserDefinedFieldsLinksDTO.class)))
				.willReturn(new UserDefinedFieldsLinksDTO());

		// WHEN
		this.mockMvc
				.perform(post("/udf-links/create-by-batch")
						.content(CommonFunctions.toJson(new UserDefinedFieldsLinksDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldsLinkService, times(1))
				.saveByBatch(any(UserDefinedFieldsLinksDTO.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}

	/**
	 * Should success save all user defined fields links.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessSaveAllUserDefinedFieldsLinks() throws Exception {

		// GIVEN
		List<UserDefinedFieldsLinksDTO> udfLinks = new ArrayList<>();
		given(userDefinedFieldsLinkService.saveAll(udfLinks)).willReturn(udfLinks);

		// WHEN
		this.mockMvc
				.perform(post("/udf-links/create-all").content(CommonFunctions.toJson(udfLinks))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldsLinkService, times(1))
				.saveAll((List<UserDefinedFieldsLinksDTO>) any(Object.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}

	/**
	 * Should success update.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdate() throws Exception {

		// GIVEN
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = createUserDefinedFieldsLinksDTO();
		given(userDefinedFieldsLinkService.save(userDefinedFieldsLinksDTO.getId(),
				userDefinedFieldsLinksDTO)).willReturn(userDefinedFieldsLinksDTO);

		// WHEN
		this.mockMvc
				.perform(put("/udf-links/update")
						.content(CommonFunctions.toJson(userDefinedFieldsLinksDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(userDefinedFieldsLinkService, times(1)).save(any(Long.class),
				any(UserDefinedFieldsLinksDTO.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}

	/**
	 * Should return list user defined fields links DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListUserDefinedFieldsLinksDTO() throws Exception {

		// GIVEN
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		given(userDefinedFieldsLinkService.find(any(UserDefinedFieldsLinksDTO.class)))
				.willReturn(Collections.singletonList(userDefinedFieldsLinksDTO));

		// WHEN
		this.mockMvc
				.perform(post("/udf-links/")
						.content(CommonFunctions.toJson(userDefinedFieldsLinksDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userDefinedFieldsLinkService, times(1)).find(any(UserDefinedFieldsLinksDTO.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}

	/**
	 * Should find UDF group by.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldFindUDFGroupBy() throws Exception {

		// GIVEN
		List<UDFLinksGroupeFieldsDTO> udfLinksGroupeFieldsDTOs = new ArrayList<>();
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		given(userDefinedFieldsLinkService.findUDFGroupBy(any(UserDefinedFieldsLinksDTO.class)))
				.willReturn(udfLinksGroupeFieldsDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/udf-links/find-udf-groupby")
						.content(CommonFunctions.toJson(userDefinedFieldsLinksDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userDefinedFieldsLinkService, times(1))
				.findUDFGroupBy(any(UserDefinedFieldsLinksDTO.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}

	/**
	 * Should find UDF loans group by.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldFindUDFLoansGroupBy() throws Exception {

		// GIVEN
		List<LoansUdfDTO> loansUdfDTOs = new ArrayList<>();
		UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO = new UserDefinedFieldsLinksDTO();
		given(userDefinedFieldsLinkService
				.findUDFLoansGroupBy(any(UserDefinedFieldsLinksDTO.class)))
						.willReturn(loansUdfDTOs);

		// WHEN
		this.mockMvc
				.perform(post("/udf-links/find-udf-loans-bycustomer")
						.content(CommonFunctions.toJson(userDefinedFieldsLinksDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(userDefinedFieldsLinkService, times(1))
				.findUDFLoansGroupBy(any(UserDefinedFieldsLinksDTO.class));
		verifyNoMoreInteractions(userDefinedFieldsLinkService);
	}
}
