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
import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;

/**
 * The class {@link LoanApprovalHistoriqueControllerTest}.
 *
 * @author MoezMhiri
 * @since 0.8.0
 */
class LoanApprovalHistoriqueControllerTest {

	/** The loan approval historique controller. */
	@InjectMocks
	private LoanApprovalHistoriqueController loanApprovalHistoriqueController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The loan approval historique service. */
	@Mock
	private LoanApprovalHistoriqueService loanApprovalHistoriqueService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loanApprovalHistoriqueController).build();
	}

	/**
	 * Should return list LoanApprovalHistorique DTO.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListLoanApprovalHistoriqueDTO() throws Exception {

		// GIVEN
		LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO = new LoanApprovalHistoriqueDTO();
		given(loanApprovalHistoriqueService.find(any(LoanApprovalHistoriqueDTO.class)))
				.willReturn(Collections.singletonList(loanApprovalHistoriqueDTO));

		// WHEN
		this.mockMvc
				.perform(post("/loan-approval-historiques/")
						.content(CommonFunctions.toJson(loanApprovalHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(loanApprovalHistoriqueService, times(1)).find(any(LoanApprovalHistoriqueDTO.class));
		verifyNoMoreInteractions(loanApprovalHistoriqueService);
	}

	/**
	 * Should success save loanApprovalHistorique.
	 * 
	 * @author MoezMhiri
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveLoanApprovalHistorique() throws Exception {

		// WHEN
		given(loanApprovalHistoriqueService.saveAndSetApprovalLabel(any(LoanApprovalHistoriqueDTO.class)))
				.willReturn(new LoanApprovalHistoriqueDTO());

		// WHEN
		this.mockMvc
				.perform(post("/loan-approval-historiques/create")
						.content(CommonFunctions.toJson(new LoanApprovalHistoriqueDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanApprovalHistoriqueService, times(1)).saveAndSetApprovalLabel(any(LoanApprovalHistoriqueDTO.class));
		verifyNoMoreInteractions(loanApprovalHistoriqueService);
	}
}
