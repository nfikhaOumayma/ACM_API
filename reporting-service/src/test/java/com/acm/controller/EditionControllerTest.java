/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.controller;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.acm.service.impl.ReportsServiceImpl;

import net.sf.jasperreports.engine.JREmptyDataSource;

/**
 * {@link EditionControllerTest} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
class EditionControllerTest {

	/** The report service. */
	@Mock
	private ReportsServiceImpl reportService;

	/** The banner controller. */
	@InjectMocks
	private EditionController editionController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(editionController).build();
	}

	/**
	 * Should generate PDF report.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldGeneratePDFReport() throws Exception {

		Map<String, Object> params = null;
		byte[] bytes = null;
		// GIVEN
		given(reportService.generatePDFReport("simple_report", params, new JREmptyDataSource()))
				.willReturn(bytes);
		// THEN

		this.mockMvc.perform(
				get("/edition/generateSimpleReport/test").contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
	}
}
