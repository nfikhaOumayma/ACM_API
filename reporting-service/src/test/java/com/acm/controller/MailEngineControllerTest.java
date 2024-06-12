/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.hamcrest.CoreMatchers;
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

import com.acm.constants.common.CommonFunctions;
import com.acm.service.impl.MailServiceEngineImpl;
import com.acm.utils.dtos.MailDTO;

/**
 * Unit tests for the controller MailEngine.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
class MailEngineControllerTest {

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The mailEngine controller. */
	@InjectMocks
	private MailEngineController mailEngineController;

	/** The mail service engine. */
	@Mock
	private MailServiceEngineImpl mailServiceEngine;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(mailEngineController).build();
	}

	/**
	 * Should send mail.
	 *
	 * @throws Exception the exception
	 */
	@Test
	void shouldSuccessSendMail() throws Exception {

		// GIVEN
		MailDTO mailDTO = new MailDTO("no-reply.acm.dev@acm.com", "haythem.benizid@dqlick.com",
				"Authentification Done", "Hello! sending mail to User.");
		doCallRealMethod().when(mailServiceEngine).prepareAndSend(mailDTO);

		// THEN
		this.mockMvc
				.perform(post("/mailsender/send").content(CommonFunctions.toJson(mailDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE))
				.andExpect(jsonPath("$.content", CoreMatchers.is("Hello! sending mail to User.")));

		verify(mailServiceEngine, times(1)).prepareAndSend(any(MailDTO.class));
		verifyNoMoreInteractions(mailServiceEngine);

	}

}
