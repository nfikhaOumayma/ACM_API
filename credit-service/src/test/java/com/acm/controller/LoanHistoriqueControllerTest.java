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
import com.acm.service.LoanHistoriqueService;
import com.acm.utils.dtos.LoanHistoriqueDTO;
import com.acm.utils.dtos.LoanNoteHistoriqueDTO;

/**
 * The class {@link LoanHistoriqueControllerTest}.
 *
 * @author MoezMhiri
 * @since 0.8.0
 */
class LoanHistoriqueControllerTest {

	/** The loan historique controller. */
	@InjectMocks
	private LoanHistoriqueController loanHistoriqueController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan historique service. */
	@Mock
	private LoanHistoriqueService loanHistoriqueService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loanHistoriqueController).build();
	}

	/**
	 * Should return list LoanHistorique DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListLoanHistoriqueDTO() throws Exception {

		// GIVEN
		LoanHistoriqueDTO loanHistoriqueDTO = new LoanHistoriqueDTO();
		given(loanHistoriqueService.find(any(LoanHistoriqueDTO.class)))
				.willReturn(Collections.singletonList(loanHistoriqueDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loan-historiques/")
						.content(CommonFunctions.toJson(loanHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanHistoriqueService, times(1)).find(any(LoanHistoriqueDTO.class));
		verifyNoMoreInteractions(loanHistoriqueService);
	}

	/**
	 * Should return list loan note historique DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListLoanNoteHistoriqueDTO() throws Exception {

		// GIVEN
		LoanNoteHistoriqueDTO loanNoteHistoriqueDTO = new LoanNoteHistoriqueDTO();
		given(loanHistoriqueService.find(any(LoanNoteHistoriqueDTO.class)))
				.willReturn(Collections.singletonList(loanNoteHistoriqueDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loan-historiques/notes")
						.content(CommonFunctions.toJson(loanNoteHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanHistoriqueService, times(1)).find(any(LoanNoteHistoriqueDTO.class));
		verifyNoMoreInteractions(loanHistoriqueService);
	}
}
