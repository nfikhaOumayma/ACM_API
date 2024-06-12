/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.repository.AcmIhmValidatorRepository;
import com.acm.service.impl.AcmIhmValidatorServiceImpl;
import com.acm.utils.dtos.AcmIhmValidatorDTO;
import com.acm.utils.models.AcmIhmValidator;

/**
 * {@link AcmIhmValidatorServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class AcmIhmValidatorServiceTest {

	/** The acm ihm validator service. */
	@InjectMocks
	private AcmIhmValidatorServiceImpl acmIhmValidatorService;

	/** The acm ihm validator repository. */
	@Mock
	private AcmIhmValidatorRepository acmIhmValidatorRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the acm ihm validator.
	 * 
	 * @author ManelLamloum
	 * @return the acm ihm validator
	 */
	private AcmIhmValidator initAcmIhmValidator() {

		AcmIhmValidator acmIhmValidator = new AcmIhmValidator();
		acmIhmValidator.setId(new Long(1));
		acmIhmValidator.setCodeValidator("code-test");
		return acmIhmValidator;
	}

	/**
	 * Inits the acm ihm validator DTO.
	 * 
	 * @author ManelLamloum
	 * @return the acm ihm validator DTO
	 */
	private AcmIhmValidatorDTO initAcmIhmValidatorDTO() {

		AcmIhmValidatorDTO acmIhmValidatorDTO = new AcmIhmValidatorDTO();
		acmIhmValidatorDTO.setId(new Long(1));
		acmIhmValidatorDTO.setCodeValidator("code-test");
		return acmIhmValidatorDTO;
	}

	/**
	 * Should return acm ihm validator DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnAcmIhmValidatorDTO() {

		// GIVEN
		AcmIhmValidator acmIhmValidator = initAcmIhmValidator();
		AcmIhmValidatorDTO acmIhmValidatorDTO = initAcmIhmValidatorDTO();
		given(acmIhmValidatorRepository.findAll())
				.willReturn(Collections.singletonList(acmIhmValidator));
		doReturn(acmIhmValidatorDTO).when(mapper).map(acmIhmValidator, AcmIhmValidatorDTO.class);

		// WHEN
		List<AcmIhmValidatorDTO> result = acmIhmValidatorService.find(acmIhmValidatorDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success find by id.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindById() {

		// GIVEN
		AcmIhmValidator acmIhmValidator = initAcmIhmValidator();
		AcmIhmValidatorDTO acmIhmValidatorDTO = initAcmIhmValidatorDTO();
		given(acmIhmValidatorRepository.findById(any(Long.class)))
				.willReturn(Optional.of(acmIhmValidator));
		doReturn(acmIhmValidatorDTO).when(mapper).map(acmIhmValidator, AcmIhmValidatorDTO.class);

		// WHEN
		AcmIhmValidatorDTO result = acmIhmValidatorService.findById(new Long(1));

		// THEN
		assertThat(result).isNotNull();
	}
}
