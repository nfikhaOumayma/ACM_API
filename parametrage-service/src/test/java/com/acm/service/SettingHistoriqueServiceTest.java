/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.dozer.DozerBeanMapper;
import org.junit.Assert;
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
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingHistoriqueRepository;
import com.acm.service.impl.SettingHistoriqueServiceImpl;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.models.SettingHistorique;

/**
 * {@link } class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingHistoriqueServiceTest {

	/** The setting historique service. */
	@InjectMocks
	private SettingHistoriqueServiceImpl settingHistoriqueService;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The setting historique repository. */
	@Mock
	private SettingHistoriqueRepository settingHistoriqueRepository;

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
	 * Inits the setting historique DTO.
	 *
	 * @return the setting historique DTO
	 */
	SettingHistoriqueDTO initSettingHistoriqueDTO() {

		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO();
		settingHistoriqueDTO.setTableName("test");
		return settingHistoriqueDTO;
	}

	/**
	 * Should success find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindById() throws ResourcesNotFoundException {

		// GIVEN
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO();
		SettingHistorique settingHistorique = new SettingHistorique();
		given(settingHistoriqueRepository.findById(new Long(1)))
				.willReturn(Optional.of(settingHistorique));
		given(mapper.map(settingHistorique, SettingHistoriqueDTO.class))
				.willReturn(settingHistoriqueDTO);

		// WHEN
		SettingHistoriqueDTO result = settingHistoriqueService.find(new Long(1));
		// THEN

		assertThat(result).isNotNull();
	}

	/**
	 * Should fail find by id.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailFindById() {

		// GIVEN
		given(settingHistoriqueRepository.findById(new Long(1))).willReturn(Optional.empty());

		// WHEN
		try {
			settingHistoriqueService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingHistorique with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success create setting historique DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateSettingHistoriqueDTO() {

		// GIVEN
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO();
		SettingHistorique settingHistorique = new SettingHistorique();

		doReturn(settingHistorique).when(mapper).map(settingHistoriqueDTO, SettingHistorique.class);
		given(settingHistoriqueRepository.save(any(SettingHistorique.class)))
				.willReturn(settingHistorique);
		// WHEN
		SettingHistoriqueDTO result = settingHistoriqueService.save(settingHistoriqueDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should return list setting historique DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingHistoriqueDTO() {

		// GIVEN
		SettingHistoriqueDTO settingHistoriqueDTO = initSettingHistoriqueDTO();
		SettingHistorique settingHistorique = new SettingHistorique();

		given(settingHistoriqueRepository.findAll())
				.willReturn(Collections.singletonList(settingHistorique));
		doReturn(settingHistorique).when(mapper).map(settingHistoriqueDTO, SettingHistorique.class);

		// WHEN
		List<SettingHistoriqueDTO> result = settingHistoriqueService.find(settingHistoriqueDTO);

		// THEN
		assertThat(result).isNotNull();
	}
}
