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

import com.acm.service.CollateralAbacusService;
import com.acm.service.GuarantorAbacusService;
import com.acm.service.LoanAbacusService;
import com.acm.service.LoanProcessSettingAbacusService;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanProcessSettingDTO;
import com.acm.utils.dtos.ScheduleDTO;

/**
 * {@link LoadDataLoanControllerTests} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class LoadDataLoanControllerTests {

	/** The load data ABACUS controller. */
	@InjectMocks
	private LoadDataLoanController loadDataABACUSController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The collateral abacus service. */
	@Mock
	private CollateralAbacusService collateralAbacusService;

	/** The guarantor abacus service. */
	@Mock
	private GuarantorAbacusService guarantorAbacusService;

	/** The loan process abacus service. */
	@Mock
	private LoanProcessSettingAbacusService loanProcessAbacusService;

	/** The loan abacus service. */
	@Mock
	private LoanAbacusService loanAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataABACUSController).build();
	}

	/**
	 * Should success find collaterals by id loan.
	 * 
	 * @author YesserSomai
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindCollateralsByIdLoan() throws Exception {

		// GIVEN
		given(loadDataABACUSController.findCollaterols(any(Long.class)))
				.willReturn(Collections.singletonList(new CollaterolDTO()));

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/loans/collaterols/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(collateralAbacusService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(collateralAbacusService);
	}

	/**
	 * Should success find guarantors by id loan.
	 * 
	 * @author YesserSomai
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindGuarantorsByIdLoan() throws Exception {

		// GIVEN
		given(loadDataABACUSController.findGuarantors(any(Long.class)))
				.willReturn(Collections.singletonList(new GuarantorDTO()));

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/loans/guarantors/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(guarantorAbacusService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(guarantorAbacusService);
	}

	/**
	 * Should success find loan by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanById() throws Exception {

		// GIVEN
		given(loadDataABACUSController.findLoan(any(Long.class))).willReturn(new LoanDTO());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/loan/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanAbacusService, times(1)).findDetails(any(Long.class));
		verifyNoMoreInteractions(loanAbacusService);
	}

	/**
	 * Should success find schedules by id loan.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindSchedulesByIdLoan() throws Exception {

		// GIVEN
		given(loadDataABACUSController.findSchedules(any(Long.class)))
				.willReturn(Collections.singletonList(new ScheduleDTO()));

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/loans/schedule/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanAbacusService, times(1)).findSchedule(any(Long.class));
		verifyNoMoreInteractions(loanAbacusService);
	}

	/**
	 * Should success find loan processs by id product.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanProcesssByIdProduct() throws Exception {

		// GIVEN
		given(loadDataABACUSController.findLoanProcessSetting(any(Long.class)))
				.willReturn(Collections.singletonList(new LoanProcessSettingDTO()));

		// WHEN
		this.mockMvc
				.perform(
						get("/load-data-abacus/loans/process/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(loanProcessAbacusService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(loanProcessAbacusService);
	}
}
