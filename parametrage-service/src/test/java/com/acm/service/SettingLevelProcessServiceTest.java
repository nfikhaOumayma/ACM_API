/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
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
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.CheckLevelProcessException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingLevelProcessRepository;
import com.acm.service.impl.SettingLevelProcessServiceImpl;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingLevel;
import com.acm.utils.models.SettingLevelProcess;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingLevelProcessProcessServiceTest} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
@RunWith(SpringRunner.class)

public class SettingLevelProcessServiceTest {

	/** The setting level service impl. */
	@InjectMocks
	private SettingLevelProcessServiceImpl settingLevelProcessServiceImpl;

	/** The setting level repository. */
	@Mock
	private SettingLevelProcessRepository settingLevelProcessRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

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
	 * Inits the setting level.
	 *
	 * @return the setting level
	 */
	private SettingLevelProcess initSettingLevelProcess() {

		SettingLevelProcess settingLevelProcess = new SettingLevelProcess();
		settingLevelProcess.setId(new Long(1));
		settingLevelProcess.setIdProduct(new Long(1));
		settingLevelProcess.setAmount(new BigDecimal(1000));
		settingLevelProcess.setSettingLevel(new SettingLevel());
		return settingLevelProcess;
	}

	/**
	 * Inits the setting level process DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting level process DTO
	 */
	private SettingLevelProcessDTO initSettingLevelProcessDTO() {

		SettingLevelProcessDTO settingLevelProcessDTO = new SettingLevelProcessDTO();
		settingLevelProcessDTO.setId(new Long(1));
		settingLevelProcessDTO.setIdProduct(new Long(1));
		settingLevelProcessDTO.setAmount(new BigDecimal(1000));
		SettingLevelDTO settingLevelDTO = new SettingLevelDTO();
		settingLevelDTO.setId(new Long(1));
		settingLevelProcessDTO.setSettingLevelDTO(settingLevelDTO);
		return settingLevelProcessDTO;
	}

	/**
	 * Should success find setting level by ID.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindSettingLevelProcess() throws ResourcesNotFoundException {

		// GIVEN
		given(settingLevelProcessRepository.findAll(any(BooleanBuilder.class), any(Sort.class)))
				.willReturn(Collections.singletonList(initSettingLevelProcess()));

		// WHEN
		List<SettingLevelProcessDTO> settingLevelProcessDTOs =
				settingLevelProcessServiceImpl.find(initSettingLevelProcessDTO());

		// THEN
		assertThat(settingLevelProcessDTOs).isNotNull();
	}

	/**
	 * Should success create setting level process DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateSettingLevelProcessDTO() {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = initSettingLevelProcessDTO();
		SettingLevelProcess settingLevelProcess = initSettingLevelProcess();

		doReturn(settingLevelProcess).when(mapper).map(settingLevelProcessDTO,
				SettingLevelProcess.class);
		doReturn(settingLevelProcessDTO).when(mapper).map(settingLevelProcess,
				SettingLevelProcessDTO.class);

		doReturn(initUserDTO()).when(userClient).find();
		given(settingLevelProcessRepository.save(any(SettingLevelProcess.class)))
				.willReturn(settingLevelProcess);

		// WHEN
		SettingLevelProcessDTO result = settingLevelProcessServiceImpl.save(settingLevelProcessDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update setting level process DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateSettingLevelProcessDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = initSettingLevelProcessDTO();
		SettingLevelProcess settingLevelProcess = initSettingLevelProcess();

		given(settingLevelProcessRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingLevelProcess));
		doReturn(settingLevelProcessDTO).when(mapper).map(settingLevelProcess,
				SettingLevelProcessDTO.class);
		given(settingLevelProcessRepository.save(any(SettingLevelProcess.class)))
				.willReturn(settingLevelProcess);
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		SettingLevelProcessDTO result = settingLevelProcessServiceImpl
				.save(settingLevelProcessDTO.getId(), settingLevelProcessDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success find setting.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindSetting() {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = initSettingLevelProcessDTO();
		SettingLevelProcess settingLevelProcess = initSettingLevelProcess();
		given(settingLevelProcessRepository.findAll())
				.willReturn(Collections.singletonList(settingLevelProcess));
		doReturn(settingLevelProcessDTO).when(mapper).map(settingLevelProcess,
				SettingLevelProcessDTO.class);

		// WHEN
		List<SettingLevelProcessDTO> result =
				settingLevelProcessServiceImpl.findSetting(settingLevelProcessDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update amount.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckLevelProcessException the check level process exception
	 */
	@Test
	void shouldSuccessUpdateAmount() throws ResourcesNotFoundException, CheckLevelProcessException {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = initSettingLevelProcessDTO();
		SettingLevelProcess settingLevelProcess = initSettingLevelProcess();

		// given of update method

		given(settingLevelProcessRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingLevelProcess));
		doReturn(settingLevelProcessDTO).when(mapper).map(settingLevelProcess,
				SettingLevelProcessDTO.class);
		given(settingLevelProcessRepository.save(any(SettingLevelProcess.class)))
				.willReturn(settingLevelProcess);
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		Boolean result = settingLevelProcessServiceImpl
				.updateAmount(Collections.singletonList(settingLevelProcessDTO));
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update setting level process.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessUpdateSettingLevelProcess() {

		// GIVEN
		SettingLevelProcessDTO settingLevelProcessDTO = initSettingLevelProcessDTO();
		SettingLevelProcess settingLevelProcess = initSettingLevelProcess();
		given(settingLevelProcessRepository.findBySettingLevel(any(SettingLevel.class)))
				.willReturn(Collections.singletonList(settingLevelProcess));
		given(settingLevelProcessRepository.save(any(SettingLevelProcess.class)))
				.willReturn(settingLevelProcess);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		settingLevelProcessServiceImpl.updateSettingLevelProcess(settingLevelProcessDTO);
		// THEN
		verify(settingLevelProcessRepository, times(1)).findBySettingLevel(any(SettingLevel.class));

	}
}
