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
import com.acm.service.AddressHistoriqueService;
import com.acm.utils.dtos.AddressHistoriqueDTO;

/**
 * The class {@link AddressHistoriqueControllerTest}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class AddressHistoriqueControllerTest {

	/** The address historique controller. */
	@InjectMocks
	private AddressHistoriqueController addressHistoriqueController;

	/** The mock mvc. */
	@Autowired
	private MockMvc mockMvc;

	/** The address historique service. */
	@Mock
	private AddressHistoriqueService addressHistoriqueService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		this.mockMvc = MockMvcBuilders.standaloneSetup(addressHistoriqueController).build();
	}

	/**
	 * Should return list addressHistorique DTO.
	 *
	 * @author HaythemBenizid
	 * @throws Exception the exception
	 */
	@Test
	void shouldReturnListAddressHistoriqueDTO() throws Exception {

		// GIVEN
		AddressHistoriqueDTO addressHistoriqueDTO = new AddressHistoriqueDTO();
		given(addressHistoriqueService.find(any(AddressHistoriqueDTO.class)))
				.willReturn(Collections.singletonList(addressHistoriqueDTO));

		// WHEN
		this.mockMvc
				.perform(post("/address-historique/")
						.content(CommonFunctions.toJson(addressHistoriqueDTO))
						.contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON_UTF8_VALUE));
		// THEN
		verify(addressHistoriqueService, times(1)).find(any(AddressHistoriqueDTO.class));
		verifyNoMoreInteractions(addressHistoriqueService);
	}

}
