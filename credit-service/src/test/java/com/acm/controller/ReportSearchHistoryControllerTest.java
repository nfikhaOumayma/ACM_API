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
import com.acm.service.ReportSearchHistoryServices;
import com.acm.utils.dtos.ReportSearchHistoryDTO;

/**
 * The class {@link ReportSearchHistoryControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
class ReportSearchHistoryControllerTest {

	/** The report search history controller. */
	@InjectMocks
	private ReportSearchHistoryController reportSearchHistoryController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The report search history services. */
	@Mock
	private ReportSearchHistoryServices reportSearchHistoryServices;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(reportSearchHistoryController).build();
	}

	/**
	 * Creates the reportSearchHistory DTO.
	 *
	 * @return the reportSearchHistory DTO
	 */
	private ReportSearchHistoryDTO createReportSearchHistoryDTO() {

		ReportSearchHistoryDTO reportSearchHistoryDTO = new ReportSearchHistoryDTO();
		reportSearchHistoryDTO.setId(new Long(1));
		return reportSearchHistoryDTO;
	}

	/**
	 * Should return list reportSearchHistory .
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListReportSearchHistory() throws Exception {

		// GIVEN
		ReportSearchHistoryDTO reportSearchHistoryPaginationDTO = new ReportSearchHistoryDTO();
		given(reportSearchHistoryServices.find(any(ReportSearchHistoryDTO.class)))
				.willReturn(Collections.singletonList(new ReportSearchHistoryDTO()));

		// WHEN
		this.mockMvc
				.perform(post("/report-search-history/")
						.content(CommonFunctions.toJson(reportSearchHistoryPaginationDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(reportSearchHistoryServices, times(1)).find(any(ReportSearchHistoryDTO.class));
		verifyNoMoreInteractions(reportSearchHistoryServices);
	}

	/**
	 * Should success save reportSearchHistory.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSaveReportSearchHistory() throws Exception {

		// GIVEN
		given(reportSearchHistoryServices.save(any(ReportSearchHistoryDTO.class)))
				.willReturn(new ReportSearchHistoryDTO());

		// WHEN
		this.mockMvc
				.perform(post("/report-search-history/create")
						.content(CommonFunctions.toJson(new ReportSearchHistoryDTO()))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(reportSearchHistoryServices, times(1)).save(any(ReportSearchHistoryDTO.class));
		verifyNoMoreInteractions(reportSearchHistoryServices);
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
		ReportSearchHistoryDTO reportSearchHistoryDTO = createReportSearchHistoryDTO();
		given(reportSearchHistoryServices.save(reportSearchHistoryDTO.getId(),
				reportSearchHistoryDTO)).willReturn(reportSearchHistoryDTO);

		// WHEN
		this.mockMvc
				.perform(put("/report-search-history/update")
						.content(CommonFunctions.toJson(reportSearchHistoryDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(reportSearchHistoryServices, times(1)).save(any(Long.class),
				any(ReportSearchHistoryDTO.class));
		verifyNoMoreInteractions(reportSearchHistoryServices);
	}

	/**
	 * Should success delete documents.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessDeleteDocuments() throws Exception {

		// GIVEN
		doNothing().when(reportSearchHistoryServices).delete(any(ReportSearchHistoryDTO.class));

		// WHEN
		this.mockMvc.perform(delete("/report-search-history/1").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
		// THEN
		verify(reportSearchHistoryServices).delete(any(ReportSearchHistoryDTO.class));
		verifyNoMoreInteractions(reportSearchHistoryServices);
	}

}
