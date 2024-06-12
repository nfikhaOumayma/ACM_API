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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.ReportVisitService;
import com.acm.utils.dtos.ReportVisitDTO;

/**
 * The class {@link ReportVisitControllerTest}.
 *
 * @author YesserSomai
 * @since 0.1.0
 */
public class ReportVisitControllerTest {

	/** The ReportVisit controller. */
	@InjectMocks
	private ReportVisitController AcmReportVisitController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The ReportVisit service. */
	@Mock
	private ReportVisitService acmReportVisitService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(AcmReportVisitController).build();
	}

	/**
	 * Creates the acm report visit DTO.
	 * 
	 * @author YesserSomai
	 * @return the acm report visit DTO
	 */
	private ReportVisitDTO createAcmReportVisitDTO() {

		ReportVisitDTO acmReportVisitDTO = new ReportVisitDTO();
		acmReportVisitDTO.setIdReportVisit(new Long(1));
		acmReportVisitDTO.setDescription("Test Desc 1");
		acmReportVisitDTO.setIdLoan(new Long(2));
		return acmReportVisitDTO;
	}

	/**
	 * Should success find acm report visit by id.
	 * 
	 * @author YesserSomai
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessFindAcmReportVisitById() throws Exception {

		// GIVEN
		ReportVisitDTO AcmReportVisitDTO = createAcmReportVisitDTO();
		given(acmReportVisitService.find(any(Long.class))).willReturn(AcmReportVisitDTO);

		// WHEN
		this.mockMvc.perform(get("/report-visits/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmReportVisitService, times(1)).find(any(Long.class));
		verifyNoMoreInteractions(acmReportVisitService);
	}

	/**
	 * Should return list acm report visit DTO.
	 * 
	 * @author YesserSomai
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListAcmReportVisitDTO() throws Exception {

		// GIVEN
		ReportVisitDTO AcmReportVisitDTO = new ReportVisitDTO();
		given(acmReportVisitService.find(any(ReportVisitDTO.class)))
				.willReturn(Collections.singletonList(AcmReportVisitDTO));

		// WHEN
		this.mockMvc
				.perform(post("/report-visits/").content(CommonFunctions.toJson(AcmReportVisitDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(acmReportVisitService, times(1)).find(any(ReportVisitDTO.class));
		verifyNoMoreInteractions(acmReportVisitService);
	}

	/**
	 * Should success update acm report visit.
	 * 
	 * @author YesserSomai
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessUpdateAcmReportVisit() throws Exception {

		// GIVEN
		given(acmReportVisitService.save(any(Long.class), any(ReportVisitDTO.class)))
				.willReturn(new ReportVisitDTO());

		// WHEN
		this.mockMvc
				.perform(put("/report-visits/update")
						.content(CommonFunctions.toJson(createAcmReportVisitDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmReportVisitService, times(1)).save(any(Long.class), any(ReportVisitDTO.class));
		verifyNoMoreInteractions(acmReportVisitService);
	}

	/**
	 * Should success save acm report visit.
	 * 
	 * @author YesserSomai
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveAcmReportVisit() throws Exception {

		// WHEN
		given(acmReportVisitService.save(any(ReportVisitDTO.class)))
				.willReturn(new ReportVisitDTO());

		// WHEN
		this.mockMvc
				.perform(post("/report-visits/create")
						.content(CommonFunctions.toJson(new ReportVisitDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(acmReportVisitService, times(1)).save(any(ReportVisitDTO.class));
		verifyNoMoreInteractions(acmReportVisitService);
	}
}
