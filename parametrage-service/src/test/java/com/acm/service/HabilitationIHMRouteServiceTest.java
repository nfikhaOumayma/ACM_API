/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.util.Collections;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.repository.HabilitationIHMRouteRepository;
import com.acm.service.impl.HabilitationIHMRouteServiceImpl;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.models.HabilitationIHMRoute;

/**
 * {@link HabilitationIHMRouteServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class HabilitationIHMRouteServiceTest {

	/** The habilitation IHM route service. */
	@InjectMocks
	private HabilitationIHMRouteServiceImpl habilitationIHMRouteService;

	/** The habilitation IHM route repository. */
	@Mock
	private HabilitationIHMRouteRepository habilitationIHMRouteRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the habilitation IHM route.
	 *
	 * @return the habilitation IHM route
	 */
	private HabilitationIHMRoute initHabilitationIHMRoute() {

		HabilitationIHMRoute habilitationIHMRoute = new HabilitationIHMRoute();
		habilitationIHMRoute.setId(new Long(1));
		habilitationIHMRoute.setIhmRoute("test");
		return habilitationIHMRoute;
	}

	/**
	 * Inithabilitation IHM route DTO.
	 *
	 * @author ManelLamloum
	 * @return the habilitation IHM route DTO
	 */
	private HabilitationIHMRouteDTO inithabilitationIHMRouteDTO() {

		HabilitationIHMRouteDTO habilitationIHMRouteDTO = new HabilitationIHMRouteDTO();
		habilitationIHMRouteDTO.setId(new Long(1));
		habilitationIHMRouteDTO.setIhmRoute("test");
		return habilitationIHMRouteDTO;
	}

	/**
	 * Should return list of habilitation ihm route.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListOfHabilitationIhmRoute() {

		// GIVEN
		HabilitationIHMRoute habilitationIHMRoute = initHabilitationIHMRoute();
		given(habilitationIHMRouteRepository.findAll())
				.willReturn(Collections.singletonList(habilitationIHMRoute));
		doReturn(habilitationIHMRoute).when(mapper).map(habilitationIHMRoute,
				HabilitationIHMRouteDTO.class);
		// WHEN
		List<HabilitationIHMRouteDTO> result =
				habilitationIHMRouteService.find(inithabilitationIHMRouteDTO());
		// THEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should success find all.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindAll() {

		// GIVEN
		HabilitationIHMRoute habilitationIHMRoute = initHabilitationIHMRoute();
		HabilitationIHMRouteDTO habilitationIHMRouteDTO = inithabilitationIHMRouteDTO();
		given(habilitationIHMRouteRepository.findAll())
				.willReturn(Collections.singletonList(habilitationIHMRoute));
		doReturn(habilitationIHMRouteDTO).when(mapper).map(habilitationIHMRoute,
				HabilitationIHMRouteDTO.class);
		// WHEN
		List<HabilitationIHMRouteDTO> result = habilitationIHMRouteService.findAll();
		// THEN
		Assertions.assertThat(result).isNotNull();
	}
}
