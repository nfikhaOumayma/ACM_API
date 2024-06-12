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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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

import com.acm.service.SettingListValuesService;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.SettingListValuesDTO;
import com.acm.utils.enums.SettingListValuesTable;

/**
 * {@link SettingListValuesControllerTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@SpringBootTest
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
public class SettingListValuesControllerTest {

	/** The setting list values controller. */
	@InjectMocks
	private SettingListValuesController settingListValuesController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The setting list values service. */
	@Mock
	private SettingListValuesService settingListValuesService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(settingListValuesController).build();
	}

	/**
	 * Creates the setting list values DTO.
	 *
	 * @return the setting list values DTO
	 */
	private SettingListValuesDTO createSettingListValuesDTO() {

		SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO();
		settingListValuesDTO
				.setTableAbacusName(SettingListValuesTable.CU_LOAN_GUARANTOR_SOURCE.tableName());
		return settingListValuesDTO;
	}

	/**
	 * Should success find loan guarantor source.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanGuarantorSource() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-loan-guarantor-source")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find product loan reasons DTOS.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindProductLoanReasonsDTOS() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-product-loan-leasons")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find loan source funds.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindLoanSourceFunds() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-loan-source-funds")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success fin refinance reason.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFinRefinanceReason() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-loan-refinance-reason")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find branches.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindBranches() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-branches")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find relationship DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindRelationshipDTO() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-relationships")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find industry.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindIndustry() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-industrys")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find roles abacus.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindRolesAbacus() throws Exception {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = createSettingListValuesDTO();
		given(settingListValuesService.find(settingListValuesDTO))
				.willReturn(Collections.singletonList(settingListValuesDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/find-roles-abacus")
						.accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).find(any(SettingListValuesDTO.class));
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success find all portfolio DT os.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAllPortfolioDTOs() throws Exception {

		// GIVEN
		PortfolioDTO portfolioDTO = new PortfolioDTO();
		given(settingListValuesService.findAllPortfolio())
				.willReturn(Collections.singletonList(portfolioDTO));

		// WHEN
		this.mockMvc
				.perform(get("/settings-list-values/portfolio").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(settingListValuesService, times(1)).findAllPortfolio();
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success lod setting.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessLodSetting() throws Exception {

		// GIVEN
		doNothing().when(settingListValuesService).loadSettingFromAbacus();

		// WHEN
		this.mockMvc.perform(get("/settings-list-values/load-setting")).andExpect(status().isOk());
		// THEN
		verify(settingListValuesService, times(1)).loadSettingFromAbacus();
		verifyNoMoreInteractions(settingListValuesService);
	}

	/**
	 * Should success reset setting.
	 * 
	 * @author ManelLamloum
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessResetSetting() throws Exception {

		// GIVEN
		doNothing().when(settingListValuesService).resetSettingFromAbacus();

		// WHEN
		this.mockMvc.perform(get("/settings-list-values/reset-setting")).andExpect(status().isOk());

		// THEN
		verify(settingListValuesService, times(1)).resetSettingFromAbacus();
		verifyNoMoreInteractions(settingListValuesService);
	}
}
