/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.mockito.ArgumentMatchers.any;
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
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmIhmFieldGroupeRepository;
import com.acm.service.impl.AcmIhmFieldGroupeServiceImpl;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmFieldGroupeDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmIhmFieldGroupe;

/**
 * {@link AcmIhmFieldGroupeServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class AcmIhmFieldGroupeServiceTest {

	/** The acm ihm field groupe service. */
	@InjectMocks
	private AcmIhmFieldGroupeServiceImpl acmIhmFieldGroupeService;

	/** The acm ihm field groupe repository. */
	@Mock
	private AcmIhmFieldGroupeRepository acmIhmFieldGroupeRepository;

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
	 * Inits the acm ihm field groupe DTO.
	 *
	 * @author ManelLamloum
	 * @return the acm ihm field groupe DTO
	 */
	private AcmIhmFieldGroupeDTO initAcmIhmFieldGroupeDTO() {

		AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO = new AcmIhmFieldGroupeDTO();
		// set group
		GroupeDTO groupeDTO = new GroupeDTO();
		groupeDTO.setId(new Long(1));
		groupeDTO.setCode("TEST CODE");
		acmIhmFieldGroupeDTO.setGroup(groupeDTO);
		return acmIhmFieldGroupeDTO;
	}

	/**
	 * Inits the acm ihm field DTO.
	 *
	 * @author ManelLamloum
	 * @return the acm ihm field DTO
	 */
	private AcmIhmFieldDTO initAcmIhmFieldDTO() {

		AcmIhmFieldDTO acmIhmFieldDTO = new AcmIhmFieldDTO();
		// set group
		acmIhmFieldDTO.setId(new Long(1));
		return acmIhmFieldDTO;
	}

	/**
	 * Should return list acm ihm field groupe.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListAcmIhmFieldGroupe() {

		// GIVEN
		AcmIhmFieldGroupe acmIhmFieldGroupe = new AcmIhmFieldGroupe();
		AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO = new AcmIhmFieldGroupeDTO();
		given(acmIhmFieldGroupeRepository.findAll())
				.willReturn(Collections.singletonList(acmIhmFieldGroupe));
		given(mapper.map(acmIhmFieldGroupe, AcmIhmFieldGroupeDTO.class))
				.willReturn(acmIhmFieldGroupeDTO);
		// WHEN
		List<AcmIhmFieldGroupeDTO> acmIhmFieldGroupeDTOs =
				acmIhmFieldGroupeService.find(initAcmIhmFieldGroupeDTO());
		// THEN
		Assertions.assertThat(acmIhmFieldGroupeDTOs).isNotNull();
	}

	/**
	 * Should success update habilitation.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateHabilitation() throws ResourcesNotFoundException {

		// GIVEN
		AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO = new AcmIhmFieldGroupeDTO();
		acmIhmFieldGroupeDTO.setAcmIhmField(initAcmIhmFieldDTO());
		AcmIhmFieldGroupe acmIhmFieldGroupe = new AcmIhmFieldGroupe();

		/*
		 * given(mapper.map(acmIhmFieldGroupeDTO, AcmIhmFieldGroupe.class))
		 * .willReturn(acmIhmFieldGroupe);
		 */
		given(acmIhmFieldGroupeRepository.save(any(AcmIhmFieldGroupe.class)))
				.willReturn(acmIhmFieldGroupe);
		given(acmIhmFieldGroupeRepository.findAll())
				.willReturn(Collections.singletonList(acmIhmFieldGroupe));

		doReturn(initUserDTO()).when(userClient).find();

		doReturn(acmIhmFieldGroupe).when(mapper).map(acmIhmFieldGroupeDTO, AcmIhmFieldGroupe.class);
		doReturn(acmIhmFieldGroupeDTO).when(mapper).map(acmIhmFieldGroupe,
				AcmIhmFieldGroupeDTO.class);
		// WHEN
		AcmIhmFieldGroupeDTO result =
				acmIhmFieldGroupeService.updateHabilitation(acmIhmFieldGroupeDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success save acm ihm field groupe DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessSaveAcmIhmFieldGroupeDTO() throws ResourcesNotFoundException {

		// GIVEN
		AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO = new AcmIhmFieldGroupeDTO();
		AcmIhmFieldDTO acmIhmFieldDTO = initAcmIhmFieldDTO();
		acmIhmFieldGroupeDTO.setAcmIhmField(acmIhmFieldDTO);
		AcmIhmFieldGroupe acmIhmFieldGroupe = new AcmIhmFieldGroupe();

		given(mapper.map(acmIhmFieldGroupeDTO, AcmIhmFieldGroupe.class))
				.willReturn(acmIhmFieldGroupe);
		given(acmIhmFieldGroupeRepository.save(acmIhmFieldGroupe)).willReturn(acmIhmFieldGroupe);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		AcmIhmFieldGroupeDTO result = acmIhmFieldGroupeService.save(acmIhmFieldGroupeDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should return empty list.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnEmptyList() {

		// GIVEN
		AcmIhmFieldGroupe acmIhmFieldGroupe = new AcmIhmFieldGroupe();
		AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO = initAcmIhmFieldGroupeDTO();
		given(acmIhmFieldGroupeRepository.findAll())
				.willReturn(Collections.singletonList(acmIhmFieldGroupe));
		given(mapper.map(acmIhmFieldGroupe, AcmIhmFieldGroupeDTO.class))
				.willReturn(acmIhmFieldGroupeDTO);

		// WHEN
		List<AcmIhmFieldGroupeDTO> acmIhmFieldGroupeDTOs =
				acmIhmFieldGroupeService.find(acmIhmFieldGroupeDTO);

		// THEN
		Assertions.assertThat(acmIhmFieldGroupeDTOs).isNotNull();
	}
}
