/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import com.acm.service.impl.GuarantorAbacusServiceImpl;
import com.acm.utils.dtos.GuarantorDTO;

/**
 * {@link GuarantorAbacusServiceTests} class.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public class GuarantorAbacusServiceTests {

	/** The guarantor abacus service. */
	@InjectMocks
	private GuarantorAbacusServiceImpl guarantorAbacusService;

	/** The named parameter jdbc template. */
	@Mock
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The environment. */
	@Mock
	private Environment environment;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
	}

	/**
	 * Should success find list guarantor DTO.
	 */
	@Test
	void shouldSuccessFindListGuarantorDTO() {

		// GIVEN
		given(environment.getProperty(any(String.class))).willReturn("select * from Guarantor");

		// WHEN
		List<GuarantorDTO> guarantorDTOs = guarantorAbacusService.find(new Long(1));

		// THEN
		assertThat(guarantorDTOs).isNotNull();
	}
}
