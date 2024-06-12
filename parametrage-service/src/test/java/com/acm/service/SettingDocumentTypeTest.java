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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

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
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingDocumentTypeRepository;
import com.acm.service.impl.SettingDocumentTypeServiceImpl;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingDocumentType;

/**
 * {@link SettingDocumentTypeTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingDocumentTypeTest {

	/** The setting document type service. */
	@InjectMocks
	private SettingDocumentTypeServiceImpl settingDocumentTypeService;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The setting document type repository. */
	@Mock
	private SettingDocumentTypeRepository settingDocumentTypeRepository;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

	/** The setting document product service. */
	@Mock
	private SettingDocumentProductService settingDocumentProductService;

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
	 * Inits the setting document type DTO.
	 *
	 * @return the setting document type DTO
	 */
	private SettingDocumentTypeDTO initSettingDocumentTypeDTO() {

		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		settingDocumentTypeDTO.setCategorie(1);
		settingDocumentTypeDTO.setCode("test");
		settingDocumentTypeDTO.setId(new Long(1));
		return settingDocumentTypeDTO;
	}

	/**
	 * Inits the setting document type.
	 *
	 * @return the setting document type
	 */
	private SettingDocumentType initSettingDocumentType() {

		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setCategorie(1);
		settingDocumentType.setCode("test");
		settingDocumentType.setId(new Long(1));
		return settingDocumentType;

	}

	/**
	 * Inits the user DTO.
	 * 
	 * @author ManelLamloum
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
	 * Should return list setting document type DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingDocumentTypeDTO() {

		// GIVEN
		SettingDocumentType settingDocumentType = new SettingDocumentType();
		given(settingDocumentTypeRepository.findAll())
				.willReturn(Collections.singletonList(new SettingDocumentType()));
		doReturn(new SettingDocumentTypeDTO()).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);

		// WHEN
		List<SettingDocumentTypeDTO> result =
				settingDocumentTypeService.find(initSettingDocumentTypeDTO());
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update setting document type DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateSettingDocumentTypeDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingDocumentType settingDocumentType = new SettingDocumentType();
		SettingDocumentTypeDTO settingDocumentTypeDTO = initSettingDocumentTypeDTO();
		given(settingDocumentTypeRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new SettingDocumentType()));
		doReturn(new SettingDocumentTypeDTO()).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingDocumentTypeRepository.save(any(SettingDocumentType.class)))
				.willReturn(new SettingDocumentType());
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		// WHEN
		SettingDocumentTypeDTO result = settingDocumentTypeService
				.save(settingDocumentTypeDTO.getId(), settingDocumentTypeDTO);

		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success create setting document type DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateSettingDocumentTypeDTO() {

		// GIVEN
		SettingDocumentType settingDocumentType = new SettingDocumentType();
		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		doReturn(settingDocumentType).when(mapper).map(settingDocumentTypeDTO,
				SettingDocumentType.class);
		doReturn(new SettingDocumentTypeDTO()).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingDocumentTypeRepository.save(any(SettingDocumentType.class)))
				.willReturn(settingDocumentType);
		given(settingDocumentProductService.save(new SettingDocumentProductDTO()))
				.willReturn(new SettingDocumentProductDTO());
		// WHEN
		SettingDocumentTypeDTO result = settingDocumentTypeService.save(settingDocumentTypeDTO);
		// THEN
		assertThat(result).isNotNull();
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
		SettingDocumentType settingDocumentType = new SettingDocumentType();

		given(settingDocumentTypeRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingDocumentType));
		doReturn(new SettingDocumentTypeDTO()).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);

		// WHEN
		SettingDocumentTypeDTO result = settingDocumentTypeService.find(new Long(1));

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
		given(settingDocumentTypeRepository.findById(any(Long.class))).willReturn(Optional.empty());

		// WHEN
		try {
			settingDocumentTypeService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingDocumentType with ID"));
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
		SettingDocumentType settingDocumentType = new SettingDocumentType();
		given(settingDocumentTypeRepository.findAll())
				.willReturn(Collections.singletonList(settingDocumentType));
		doReturn(new SettingDocumentTypeDTO()).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);

		// WHEN
		List<SettingDocumentTypeDTO> result = settingDocumentTypeService.find();

		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update status disabled.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateStatusDisabled() throws ResourcesNotFoundException {

		// GIVEN
		SettingDocumentType settingDocumentType = initSettingDocumentType();
		SettingDocumentTypeDTO settingDocumentTypeDTO = initSettingDocumentTypeDTO();

		given(settingDocumentProductService.find(any(SettingDocumentProductDTO.class)))
				.willReturn(Collections.singletonList(new SettingDocumentProductDTO()));
		doNothing().when(settingDocumentProductService)
				.updateStatus(any(SettingDocumentProductDTO.class), any(Boolean.class));
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(settingDocumentType).when(mapper).map(settingDocumentTypeDTO,
				SettingDocumentType.class);
		given(settingDocumentTypeRepository.save(any(SettingDocumentType.class)))
				.willReturn(settingDocumentType);

		// given of findById method :

		given(settingDocumentTypeRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingDocumentType));
		doReturn(settingDocumentTypeDTO).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);

		// WHEN
		settingDocumentTypeService.updateStatus(settingDocumentTypeDTO, Boolean.FALSE);
		// THEN
		verify(settingDocumentTypeRepository, times(1)).save(any(SettingDocumentType.class));
	}

	/**
	 * Should success update status enabled.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateStatusEnabled() throws ResourcesNotFoundException {

		// GIVEN
		SettingDocumentType settingDocumentType = initSettingDocumentType();
		SettingDocumentTypeDTO settingDocumentTypeDTO = initSettingDocumentTypeDTO();

		doReturn(initUserDTO()).when(userClient).find();
		doReturn(settingDocumentType).when(mapper).map(settingDocumentTypeDTO,
				SettingDocumentType.class);
		given(settingDocumentTypeRepository.save(any(SettingDocumentType.class)))
				.willReturn(settingDocumentType);

		// given of findById method :

		given(settingDocumentTypeRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingDocumentType));
		doReturn(settingDocumentTypeDTO).when(mapper).map(settingDocumentType,
				SettingDocumentTypeDTO.class);

		// WHEN
		settingDocumentTypeService.updateStatus(settingDocumentTypeDTO, Boolean.TRUE);
		// THEN
		verify(settingDocumentTypeRepository, times(1)).save(any(SettingDocumentType.class));

	}
}
