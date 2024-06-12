/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import com.acm.service.impl.LoanAbacusServiceImpl;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.models.Loan;

/**
 * {@link LoanAbacusServiceTests} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public class LoanAbacusServiceTests {

	/** The loan abacus service. */
	@InjectMocks
	private LoanAbacusServiceImpl loanAbacusService;

	/** The named parameter jdbc template. */
	@Mock
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Should success find list loan DTO.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindListLoanDTO() {

		// GIVEN
		LoanDTO loanDTO = initLoanDTO();
		Loan loan = initLoan();
		given(environment.getProperty(any(String.class))).willReturn("select * from Loan");
		given(mapper.map(loan, LoanDTO.class)).willReturn(loanDTO);

		// WHEN
		List<LoanDTO> loanDTOs = loanAbacusService.find(new Long(1));

		// THEN
		assertThat(loanDTOs).isNotNull();
	}

	/**
	 * Initialize an loan.
	 * 
	 * @author HaythemBenizid
	 * @return a new loan
	 */
	private Loan initLoan() {

		Loan loan = new Loan();
		loan.setIdLoan(new Long(1));
		loan.setAcmVersion(0);
		return loan;
	}

	/**
	 * Inits the loan.
	 * 
	 * @author HaythemBenizid
	 * @return the loan DTO
	 */
	private LoanDTO initLoanDTO() {

		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setApplyDate(new Date());
		return loanDTO;
	}
}
