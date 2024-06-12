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
import static org.mockito.Mockito.doNothing;
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
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingLevelRepository;
import com.acm.service.impl.SettingLevelServiceImpl;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingLevel;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingLevelServiceTest} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
@RunWith(SpringRunner.class)

public class SettingLevelServiceTest {

	/** The setting level service impl. */
	@InjectMocks
	private SettingLevelServiceImpl settingLevelServiceImpl;

	/** The setting level repository. */
	@Mock
	private SettingLevelRepository settingLevelRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The setting level process service. */
	@Mock
	private SettingLevelProcessService settingLevelProcessService;

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
	 * Inits the setting level.
	 *
	 * @return the setting level
	 */
	private SettingLevel initSettingLevel() {

		SettingLevel settingLevel = new SettingLevel();
		settingLevel.setId(new Long(1));
		settingLevel.setCode("Lvl 1");
		settingLevel.setTitle("Level 1");
		return settingLevel;
	}

	/**
	 * Inits the setting level DTO.
	 *
	 * @return the setting level DTO
	 */
	private SettingLevelDTO initSettingLevelDTO() {

		SettingLevelDTO settingLevelDTO = new SettingLevelDTO();
		settingLevelDTO.setId(new Long(1));
		settingLevelDTO.setCode("Lvl 1");
		settingLevelDTO.setTitle("Level 1");
		return settingLevelDTO;
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
	 * Should success find setting level by ID.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindSettingLevelByID() throws ResourcesNotFoundException {

		// GIVEN
		given(settingLevelRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initSettingLevel()));

		// WHEN
		SettingLevelDTO settingLevelDTO = settingLevelServiceImpl.find(new Long(1));

		// THEN
		assertThat(settingLevelDTO).isNotNull();
	}

	/**
	 * Should return list setting level DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingLevelDTO() {

		// GIVEN
		SettingLevelDTO settingLevelDTO = initSettingLevelDTO();
		given(settingLevelRepository.findAll(any(BooleanBuilder.class), any(Sort.class)))
				.willReturn(Collections.singletonList(initSettingLevel()));
		doReturn(new SettingDocumentTypeDTO()).when(mapper).map(new SettingLevel(),
				SettingLevelDTO.class);

		// WHEN
		List<SettingLevelDTO> result = settingLevelServiceImpl.find(settingLevelDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success create setting level DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateSettingLevelDTO() {

		// GIVEN
		SettingLevelDTO settingLevelDTO = initSettingLevelDTO();
		SettingLevel settingLevel = initSettingLevel();

		doReturn(settingLevel).when(mapper).map(settingLevelDTO, SettingLevel.class);
		doReturn(settingLevelDTO).when(mapper).map(settingLevel, SettingLevelDTO.class);

		doReturn(initUserDTO()).when(userClient).find();
		given(settingLevelRepository.save(any(SettingLevel.class))).willReturn(settingLevel);

		// WHEN
		SettingLevelDTO result = settingLevelServiceImpl.save(settingLevelDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update setting level DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateSettingLevelDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingLevelDTO settingLevelDTO = initSettingLevelDTO();
		SettingLevel settingLevel = initSettingLevel();

		given(settingLevelRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingLevel));
		doReturn(initUserDTO()).when(userClient).find();
		given(settingLevelRepository.save(any(SettingLevel.class))).willReturn(settingLevel);
		doReturn(settingLevelDTO).when(mapper).map(settingLevel, SettingLevelDTO.class);
		doNothing().when(settingLevelProcessService)
				.updateSettingLevelProcess(any(SettingLevelProcessDTO.class));
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		// WHEN
		SettingLevelDTO result =
				settingLevelServiceImpl.save(settingLevelDTO.getId(), settingLevelDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should fail update setting level DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldFailUpdateSettingLevelDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingLevelDTO settingLevelDTO = initSettingLevelDTO();
		given(settingLevelRepository.findById(any(Long.class))).willReturn(Optional.empty());

		// WHEN
		try {
			settingLevelServiceImpl.save(settingLevelDTO.getId(), settingLevelDTO);
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingLevel with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success find.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFind() {

		// GIVEN
		SettingLevelDTO settingLevelDTO = initSettingLevelDTO();
		SettingLevel settingLevel = initSettingLevel();
		given(settingLevelRepository.findAll()).willReturn(Collections.singletonList(settingLevel));
		doReturn(settingLevelDTO).when(mapper).map(settingLevel, SettingLevelDTO.class);

		// WHEN
		List<SettingLevelDTO> result = settingLevelServiceImpl.find();
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update order.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateOrder() throws ResourcesNotFoundException {

		// GIVEN
		SettingLevelDTO settingLevelDTO = initSettingLevelDTO();
		SettingLevel settingLevel = initSettingLevel();

		given(settingLevelRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingLevel));
		doReturn(initUserDTO()).when(userClient).find();
		given(settingLevelRepository.save(any(SettingLevel.class))).willReturn(settingLevel);
		doReturn(settingLevelDTO).when(mapper).map(settingLevel, SettingLevelDTO.class);
		doNothing().when(settingLevelProcessService)
				.updateSettingLevelProcess(any(SettingLevelProcessDTO.class));
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());

		// WHEN
		List<SettingLevelDTO> result =
				settingLevelServiceImpl.updateOrder(Collections.singletonList(settingLevelDTO));
		// THEN
		assertThat(result).isNotNull();
	}
}
