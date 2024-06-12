/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.assertj.core.api.Assertions;
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
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.impl.AcmEnvironnementServiceImpl;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmEnvironnement;
import com.acm.utils.repository.AcmEnvironnementRepository;

/**
 * {@link AcmEnvironnementServiceTest} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@RunWith(SpringRunner.class)
class AcmEnvironnementServiceTest {

	/** The acmEnvironnement service. */
	@InjectMocks
	private AcmEnvironnementServiceImpl acmEnvironnementService;

	/** The acmEnvironnement repository. */
	@Mock
	private AcmEnvironnementRepository acmEnvironnementRepository;

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
	 * Initialize a new UserDTO.
	 * 
	 * @return a new UserDTO
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
	 * Initialize an acmEnvironnement.
	 * 
	 * @author HaythemBenizid
	 * @return a new acmEnvironnement
	 */
	private AcmEnvironnement initAcmEnvironnement() {

		AcmEnvironnement acmEnvironnement = new AcmEnvironnement();
		acmEnvironnement.setId(new Long(0));
		acmEnvironnement.setAcmVersion(0);
		return acmEnvironnement;
	}

	/**
	 * Inits the acmEnvironnement.
	 * 
	 * @author HaythemBenizid
	 * @return the acmEnvironnement DTO
	 */
	private AcmEnvironnementDTO initAcmEnvironnementDTO() {

		AcmEnvironnementDTO acmEnvironnementDTO = new AcmEnvironnementDTO();
		acmEnvironnementDTO.setId(Long.valueOf(1));
		acmEnvironnementDTO.setKey("TEST");
		return acmEnvironnementDTO;
	}

	/**
	 * Should success save.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessSave() {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = initAcmEnvironnementDTO();
		AcmEnvironnement acmEnvironnement = initAcmEnvironnement();
		given(acmEnvironnementRepository.save(any(AcmEnvironnement.class)))
				.willReturn(acmEnvironnement);
		given(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class))
				.willReturn(acmEnvironnementDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		AcmEnvironnementDTO acmEnvironnementDTOReponse =
				acmEnvironnementService.save(acmEnvironnementDTO);

		// THEN
		assertThat(acmEnvironnementDTOReponse).isNotNull();
	}

	/**
	 * Should success update.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = initAcmEnvironnementDTO();
		acmEnvironnementDTO.setId(new Long(1));
		AcmEnvironnement acmEnvironnement = mapper.map(acmEnvironnementDTO, AcmEnvironnement.class);
		acmEnvironnement.setAcmVersion(0);
		given(acmEnvironnementRepository.findById(any(Long.class)))
				.willReturn(Optional.of(acmEnvironnement));
		given(acmEnvironnementRepository.save(any(AcmEnvironnement.class)))
				.willReturn(acmEnvironnement);
		given(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class))
				.willReturn(acmEnvironnementDTO);
		given(userClient.find()).willReturn(initUserDTO());
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());

		// WHEN
		AcmEnvironnementDTO acmEnvironnementDTOReponse =
				acmEnvironnementService.save(acmEnvironnement.getId(), acmEnvironnementDTO);

		// THEN
		assertThat(acmEnvironnementDTOReponse).isNotNull();
	}

	/**
	 * Should success find acmEnvironnement by KEY.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindAcmEnvironnementByKey() throws ResourcesNotFoundException {

		// GIVEN
		given(acmEnvironnementRepository.findByKey(any(String.class)))
				.willReturn(Collections.singletonList(initAcmEnvironnement()));

		// WHEN
		AcmEnvironnementDTO acmEnvironnementDTO = acmEnvironnementService.find("TEST");

		// THEN
		assertThat(acmEnvironnementDTO).isNotNull();
	}

	/**
	 * Should return list acm environnement DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListAcmEnvironnementDTO() {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = initAcmEnvironnementDTO();
		AcmEnvironnement acmEnvironnement = initAcmEnvironnement();

		given(acmEnvironnementRepository.findAll())
				.willReturn(Collections.singletonList(acmEnvironnement));
		given(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class))
				.willReturn(acmEnvironnementDTO);

		// WHEN
		List<AcmEnvironnementDTO> acmEnvironnementDTOsResponse =
				acmEnvironnementService.find(acmEnvironnementDTO);

		// THEN
		Assertions.assertThat(acmEnvironnementDTOsResponse).isNotNull();

	}

	/**
	 * Should success find acm environnement.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindAcmEnvironnement() {

		// GIVEN
		AcmEnvironnement acmEnvironnement = initAcmEnvironnement();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmEnvironnementRepository.findAll())
				.willReturn(Collections.singletonList(acmEnvironnement));
		given(acmEnvironnementRepository.findByCategory(any(String.class)))
				.willReturn(Collections.singletonList(acmEnvironnement));
		// WHEN
		List<AcmEnvironnementDTO> acmEnvironnementDTOsResponse = acmEnvironnementService.find();
		// THEN
		Assertions.assertThat(acmEnvironnementDTOsResponse).isNotNull();
	}

	/**
	 * Should success update limite.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateLimite() throws ResourcesNotFoundException {

		// GIVEN
		AcmEnvironnementDTO acmEnvironnementDTO = initAcmEnvironnementDTO();
		AcmEnvironnement acmEnvironnement = new AcmEnvironnement();

		given(settingHistoriqueService.save(new SettingHistoriqueDTO()))
				.willReturn(new SettingHistoriqueDTO());

		given(acmEnvironnementRepository.save(acmEnvironnement)).willReturn(acmEnvironnement);

		doReturn(initUserDTO()).when(userClient).find();

		given(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class))
				.willReturn(acmEnvironnementDTO);

		given(acmEnvironnementRepository.findById(any(Long.class)))
				.willReturn(Optional.of(acmEnvironnement));

		given(acmEnvironnementRepository.findByKey(any(String.class)))
				.willReturn(Collections.singletonList(acmEnvironnement));

		given(acmEnvironnementService.find("TEST")).willReturn(acmEnvironnementDTO);

		// WHEN
		AcmEnvironnementDTO acmEnvironnementDTOResponse =
				acmEnvironnementService.updateLimite("TEST", "value");

		// THEN
		Assertions.assertThat(acmEnvironnementDTOResponse).isNotNull();
		// check that the execution does not throw exception.
		assertAll(() -> acmEnvironnementService.updateLimite("key", "value"));
	}

	/**
	 * Should success find like key.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindLikeKey() {

		// GIVEN
		AcmEnvironnement acmEnvironnement = initAcmEnvironnement();
		AcmEnvironnementDTO acmEnvironnementDTO = initAcmEnvironnementDTO();

		given(acmEnvironnementRepository.findAll())
				.willReturn(Collections.singletonList(acmEnvironnement));
		given(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class))
				.willReturn(acmEnvironnementDTO);
		// WHEN
		List<AcmEnvironnementDTO> acmEnvironnementDTOResponse =
				acmEnvironnementService.findLikeKey(acmEnvironnementDTO);
		// THEN
		Assertions.assertThat(acmEnvironnementDTOResponse).isNotNull();
	}

	/**
	 * Should success find by keys.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindByKeys() {

		// GIVEN
		AcmEnvironnement acmEnvironnement = initAcmEnvironnement();
		List<String> keysTest = new ArrayList<String>();
		given(acmEnvironnementRepository.findByKeyIn(Collections.singletonList(any(String.class))))
				.willReturn(Collections.singletonList(acmEnvironnement));
		// WHEN
		List<AcmEnvironnementDTO> acmEnvironnementDTOResponse =
				acmEnvironnementService.findByKeys(keysTest);
		// THEN
		Assertions.assertThat(acmEnvironnementDTOResponse).isNotNull();
	}

	/**
	 * Should success find by category.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindByCategory() {

		// GIVEN
		AcmEnvironnement acmEnvironnement = initAcmEnvironnement();
		String categoryTest = "TEST CATEGPRY";
		given(acmEnvironnementRepository.findByCategory(categoryTest))
				.willReturn(Collections.singletonList(acmEnvironnement));
		// WHEN
		List<AcmEnvironnementDTO> acmEnvironnementDTOResponse =
				acmEnvironnementService.findByCategory(categoryTest);
		// THEN
		Assertions.assertThat(acmEnvironnementDTOResponse).isNotNull();
	}
}
