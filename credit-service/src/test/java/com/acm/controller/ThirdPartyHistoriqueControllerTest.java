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
import com.acm.service.ThirdPartyHistoriqueService;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.enums.ThirdPartyCategory;

/**
 * The class {@link ThirdPartyHistoriqueControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class ThirdPartyHistoriqueControllerTest {

	/** The third party historique controller. */
	@InjectMocks
	private ThirdPartyHistoriqueController thirdPartyHistoriqueController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The third party historique service. */
	@Mock
	private ThirdPartyHistoriqueService thirdPartyHistoriqueService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(thirdPartyHistoriqueController).build();
	}

	/**
	 * Creates the third party historique DTO.
	 *
	 * @return the third party historique DTO
	 */
	private ThirdPartyHistoriqueDTO createThirdPartyHistoriqueDTO() {

		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		thirdPartyHistoriqueDTO.setId(new Long(1));
		return thirdPartyHistoriqueDTO;
	}

	/**
	 * Should success find third party historique by id.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindThirdPartyHistoriqueById() throws Exception {

		// GIVEN
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = createThirdPartyHistoriqueDTO();
		given(thirdPartyHistoriqueService.find(any(Long.class)))
				.willReturn(thirdPartyHistoriqueDTO);

		// WHEN
		this.mockMvc.perform(get("/third-party-historiques/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(thirdPartyHistoriqueService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}

	/**
	 * Should return list third party historique DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListThirdPartyHistoriqueDTO() throws Exception {

		// GIVEN
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		given(thirdPartyHistoriqueService.find(any(ThirdPartyHistoriqueDTO.class)))
				.willReturn(Collections.singletonList(thirdPartyHistoriqueDTO));

		// WHEN
		this.mockMvc
				.perform(post("/third-party-historiques/")
						.content(CommonFunctions.toJson(thirdPartyHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(thirdPartyHistoriqueService, times(1)).find(any(ThirdPartyHistoriqueDTO.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}

	/**
	 * Should return find for screening.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnFindForScreening() throws Exception {

		// GIVEN
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		given(thirdPartyHistoriqueService.findForScreening(any(ThirdPartyHistoriqueDTO.class)))
				.willReturn(Collections.singletonList(thirdPartyHistoriqueDTO));

		// WHEN
		this.mockMvc
				.perform(post("/third-party-historiques/find-for-screening")
						.content(CommonFunctions.toJson(thirdPartyHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(thirdPartyHistoriqueService, times(1))
				.findForScreening(any(ThirdPartyHistoriqueDTO.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}

	/**
	 * Should success find third party historique by given {@link ThirdPartyCategory} and ID Loan
	 * and ID customer for given category =(CUSTOMER / GUARANTOR).
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindThirdPartyHistoriqueByGivenParams() throws Exception {

		// GIVEN
		List<ThirdPartyHistoriqueDTO> thirdPartyHistoriqueDTOs = new ArrayList<>();
		given(thirdPartyHistoriqueService.find(any(String.class), any(Long.class), any(Long.class),
				any(String.class))).willReturn(thirdPartyHistoriqueDTOs);

		// WHEN
		this.mockMvc.perform(get("/third-party-historiques/code/1/2/categoryCustomer")
				.accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		// THEN
		verify(thirdPartyHistoriqueService, times(1)).find(any(String.class), any(Long.class),
				any(Long.class), any(String.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}

	/**
	 * Should return generate I score report.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnGenerateIScoreReport() throws Exception {

		// GIVEN
		byte[] data = new byte[1];
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		given(thirdPartyHistoriqueService.generateIscoreReport(any(ThirdPartyHistoriqueDTO.class)))
				.willReturn(data);

		// WHEN
		this.mockMvc
				.perform(post("/third-party-historiques/report-iscore")
						.content(CommonFunctions.toJson(thirdPartyHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
		// THEN
		verify(thirdPartyHistoriqueService, times(1))
				.generateIscoreReport(any(ThirdPartyHistoriqueDTO.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}

	/**
	 * Should return generate iscore stored report.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnGenerateIscoreStoredReport() throws Exception {

		// GIVEN
		byte[] data = new byte[1];
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		given(thirdPartyHistoriqueService
				.generateIscoreStoredReport(any(ThirdPartyHistoriqueDTO.class))).willReturn(data);

		// WHEN
		this.mockMvc
				.perform(post("/third-party-historiques/report-iscore-stored")
						.content(CommonFunctions.toJson(thirdPartyHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
		// THEN
		verify(thirdPartyHistoriqueService, times(1))
				.generateIscoreStoredReport(any(ThirdPartyHistoriqueDTO.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}

	/**
	 * Should return validate screening.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnValidateScreening() throws Exception {

		// GIVEN
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = new ThirdPartyHistoriqueDTO();
		given(thirdPartyHistoriqueService.validate(any(ThirdPartyHistoriqueDTO.class)))
				.willReturn(thirdPartyHistoriqueDTO);

		// WHEN
		this.mockMvc
				.perform(post("/third-party-historiques/validate")
						.content(CommonFunctions.toJson(thirdPartyHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(thirdPartyHistoriqueService, times(1)).validate(any(ThirdPartyHistoriqueDTO.class));
		verifyNoMoreInteractions(thirdPartyHistoriqueService);
	}
}
