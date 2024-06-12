/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

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
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.HabilitationRepository;
import com.acm.service.impl.HabilitationServiceImpl;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.Habilitation;

/**
 * {@link HabilitationServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class HabilitationServiceTest {

	/** The habilitation service. */
	@InjectMocks
	private HabilitationServiceImpl habilitationService;

	/** The habilitation repository. */
	@Mock
	private HabilitationRepository habilitationRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

	/** The groupe service. */
	@Mock
	private GroupeService groupeService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the habilitation.
	 *
	 * @return the habilitation
	 */
	private Habilitation initHabilitation() {

		Habilitation habilitation = new Habilitation();
		habilitation.setId(new Long(1));
		habilitation.setValue("test");
		return habilitation;
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
		Set<GroupeDTO> groupesDTO = new HashSet<GroupeDTO>();
		GroupeDTO groupeDTO = new GroupeDTO();
		groupeDTO.setId(new Long(1));
		groupeDTO.setCode("TEST");
		groupesDTO.add(groupeDTO);
		userDTO.setGroupes(groupesDTO);
		return userDTO;
	}

	/**
	 * Inits the groupe DTO.
	 * 
	 * @author ManelLamloul
	 * @return the groupe DTO
	 */
	private GroupeDTO initGroupeDTO() {

		GroupeDTO groupeDTO = new GroupeDTO();
		groupeDTO.setId(new Long(1));
		groupeDTO.setCode("TEST");
		return groupeDTO;
	}

	/**
	 * Should return list habilitation DTO.
	 * 
	 * @author ManelLamloum
	 * @throws Exception
	 */
	@Test
	void shouldReturnListHabilitationDTO() throws Exception {

		// GIVEN
		Habilitation habilitation = new Habilitation();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		given(userClient.find()).willReturn(initUserDTO());
		given(habilitationRepository.findByIdGroupeAndClientAndEnabled(new Long(1), "TEST NAME",
				Boolean.TRUE)).willReturn(Collections.singletonList(habilitation));
		given(mapper.map(habilitation, HabilitationDTO.class)).willReturn(habilitationDTO);

		// WHEN
		List<HabilitationDTO> habilitationDTOs = habilitationService.find();
		// THEN
		Assertions.assertThat(habilitationDTOs).isNotNull();
	}

	/**
	 * Should success find habilitation DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindHabilitationDTO() {

		// GIVEN
		Habilitation habilitation = new Habilitation();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		given(habilitationRepository.findAll()).willReturn(Collections.singletonList(habilitation));
		given(mapper.map(habilitation, HabilitationDTO.class)).willReturn(habilitationDTO);

		// WHEN
		List<HabilitationDTO> habilitationDTOs = habilitationService.find(habilitationDTO);

		// THEN
		Assertions.assertThat(habilitationDTOs).isNotNull();

	}

	/**
	 * Should success find all habilitation DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindAllHabilitationDTO() {

		// GIVEN
		Habilitation habilitation = new Habilitation();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		given(habilitationRepository.findAll()).willReturn(Collections.singletonList(habilitation));
		given(mapper.map(habilitation, HabilitationDTO.class)).willReturn(habilitationDTO);

		// WHEN
		List<HabilitationDTO> habilitationDTOs = habilitationService.findAll(habilitationDTO);
		// THEN
		Assertions.assertThat(habilitationDTOs).isNotNull();

	}

	/**
	 * Should success find by group ID.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindByGroupID() throws ResourcesNotFoundException {

		// GIVEN
		GroupeDTO groupeDTO = initGroupeDTO();
		Habilitation habilitation = new Habilitation();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		given(groupeService.findByCode(any(String.class))).willReturn(groupeDTO);
		given(habilitationRepository.findByIdGroupe(groupeDTO.getId()))
				.willReturn(Collections.singletonList(habilitation));
		given(mapper.map(habilitation, HabilitationDTO.class)).willReturn(habilitationDTO);

		// WHEN
		List<HabilitationDTO> habilitationDTOs = habilitationService.findByGroupeID();
		// THEN
		Assertions.assertThat(habilitationDTOs).isNotNull();
	}

	/**
	 * Should success save all.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessSaveAll() {

		// GIVEN
		Habilitation habilitationWithoutId = new Habilitation();
		Habilitation habilitation = initHabilitation();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		given(mapper.map(habilitationDTO, Habilitation.class)).willReturn(habilitationWithoutId);
		doReturn(habilitation).when(habilitationRepository).save(habilitationWithoutId);

		// WHEN
		habilitationService.saveAll(Collections.singletonList(habilitationDTO));

		// THEN
		verify(habilitationRepository, times(1)).save(habilitationWithoutId);
	}

	/**
	 * Should success uapdate all.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUapdateAll() throws ResourcesNotFoundException {

		// GIVEN
		Habilitation habilitation = initHabilitation();
		HabilitationDTO habilitationDTO = new HabilitationDTO();
		habilitationDTO.setId(new Long(1));
		given(habilitationRepository.findById(any(Long.class)))
				.willReturn(Optional.of(habilitation));
		doReturn(habilitationDTO).when(mapper).map(habilitation, HabilitationDTO.class);

		given(userClient.find()).willReturn(initUserDTO());
		doReturn(habilitation).when(habilitationRepository).save(habilitation);

		// WHEN
		List<HabilitationDTO> habilitationDTOs =
				habilitationService.updateAll(Collections.singletonList(habilitationDTO));
		// THEN
		Assertions.assertThat(habilitationDTOs).isNotNull();
	}
}
