/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.SettingListValuesAbacusService;

/**
 * {@link LoadDataSettingListValuesControllerTests} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class LoadDataSettingListValuesControllerTests {

	/** The load data setting list values controller. */
	@InjectMocks
	private LoadDataSettingListValuesController loadDataSettingListValuesController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting list values abacus service. */
	@Mock
	private SettingListValuesAbacusService settingListValuesAbacusService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(loadDataSettingListValuesController).build();
	}

	/**
	 * Should success find branches.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindBranches() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findBranches()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/setting/branche/").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findBranches();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find product loan reasons.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindProductLoanReasons() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findProductLoanReasons())
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-product-loan-reasons/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findProductLoanReasons();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find loan guarantor source.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanGuarantorSource() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findLoanGuarantorSource())
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-loan-guarantor-source/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findLoanGuarantorSource();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find loan source of fundse.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanSourceOfFundse() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findLoanSourceOfFunds()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-loan-source-of-funds/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findLoanSourceOfFunds();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find loan refinance reason.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanRefinanceReason() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findLoanRefinanceReason())
				.willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-loan-refinance-reason/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findLoanRefinanceReason();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find relationship.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindRelationship() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findRelationship()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-relationship/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findRelationship();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find industry.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIndustry() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findIndustry()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(
				get("/load-data-abacus/setting/find-industry/").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findIndustry();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find role abacus.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindRoleAbacus() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findRoleAbacus()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-role-abacus/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findRoleAbacus();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}

	/**
	 * Should success find disctrict code.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindDisctrictCode() throws Exception {

		// GIVEN
		given(settingListValuesAbacusService.findDistricCode()).willReturn(new ArrayList<>());

		// WHEN
		this.mockMvc.perform(get("/load-data-abacus/setting/find-district-code/")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(settingListValuesAbacusService, times(1)).findDistricCode();
		verifyNoMoreInteractions(settingListValuesAbacusService);
	}
}
