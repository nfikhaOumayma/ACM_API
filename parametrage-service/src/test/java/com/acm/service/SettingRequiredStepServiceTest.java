/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingRequiredStepRepository;
import com.acm.service.impl.SettingRequiredStepServiceImpl;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingRequiredStep;

/**
 * {@link SettingRequiredStepServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingRequiredStepServiceTest {

	/** The setting required step service. */
	@InjectMocks
	SettingRequiredStepServiceImpl settingRequiredStepService;

	/** The setting required step repository. */
	@Mock
	private SettingRequiredStepRepository settingRequiredStepRepository;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

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
	 * Inits the user DTO.
	 *
	 * @return the user DTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin(CommonConstants.DEFAULT_USER);
		userDTO.setPrenom("benTest");
		userDTO.setFullName("Test Ben Test Test");
		return userDTO;
	}

	/**
	 * Inits the setting required step DTO.
	 *
	 * @return the setting required step DTO
	 */
	private SettingRequiredStepDTO initSettingRequiredStepDTO() {

		SettingRequiredStepDTO settingRequiredStepDTO = new SettingRequiredStepDTO();
		settingRequiredStepDTO.setId(new Long(1));
		settingRequiredStepDTO.setCode("test");
		settingRequiredStepDTO.setProductId(1);
		settingRequiredStepDTO.setMandatory(Boolean.TRUE);
		return settingRequiredStepDTO;
	}

	/**
	 * Should success find.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFind() {

		// GIVEN
		SettingRequiredStepDTO settingRequiredStepDTO = initSettingRequiredStepDTO();
		SettingRequiredStep settingRequiredStep = new SettingRequiredStep();

		given(settingRequiredStepRepository.findAll())
				.willReturn(Collections.singletonList(new SettingRequiredStep()));
		doReturn(settingRequiredStepDTO).when(mapper).map(settingRequiredStep,
				SettingRequiredStepDTO.class);
		doReturn(settingRequiredStep).when(mapper).map(settingRequiredStepDTO,
				SettingRequiredStep.class);

		// WHEN
		List<SettingRequiredStepDTO> result =
				settingRequiredStepService.find(settingRequiredStepDTO);
		// THEN

		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success create.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreate() {

		// GIVEN
		SettingRequiredStepDTO settingRequiredStepDTO = new SettingRequiredStepDTO();
		SettingRequiredStep settingRequiredStep = new SettingRequiredStep();

		doReturn(settingRequiredStep).when(mapper).map(settingRequiredStepDTO,
				SettingRequiredStep.class);
		given(settingRequiredStepRepository.save(any(SettingRequiredStep.class)))
				.willReturn(new SettingRequiredStep());
		doReturn(settingRequiredStepDTO).when(mapper).map(settingRequiredStep,
				SettingRequiredStepDTO.class);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		SettingRequiredStepDTO result = settingRequiredStepService.save(settingRequiredStepDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail update.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailUpdate() {

		// GIVEN
		given(settingRequiredStepRepository.findById(any(Long.class))).willReturn(Optional.empty());
		// WHEN
		try {
			settingRequiredStepService.save(new Long(1), new SettingRequiredStepDTO());
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingRequiredStep with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success update.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		given(settingRequiredStepRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new SettingRequiredStep()));
		given(settingRequiredStepRepository.save(any(SettingRequiredStep.class)))
				.willReturn(new SettingRequiredStep());

		doReturn(initUserDTO()).when(userClient).find();
		doReturn(new SettingRequiredStep()).when(mapper).map(new SettingRequiredStepDTO(),
				SettingRequiredStep.class);
		given(settingHistoriqueService.save(new SettingHistoriqueDTO()))
				.willReturn(new SettingHistoriqueDTO());
		// WHEN
		SettingRequiredStepDTO result =
				settingRequiredStepService.save(new Long(1), new SettingRequiredStepDTO());
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should return list setting required step DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingRequiredStepDTO() {

		// GIVEN
		given(settingRequiredStepRepository.findAll())
				.willReturn(Collections.singletonList(new SettingRequiredStep()));
		doReturn(new SettingRequiredStepDTO()).when(mapper).map(new SettingRequiredStep(),
				SettingRequiredStepDTO.class);

		// WHEN
		List<SettingRequiredStepDTO> result = settingRequiredStepService.find();
		// THEN
		Assertions.assertThat(result).isNotNull();

	}
}
