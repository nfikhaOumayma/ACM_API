/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

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
import com.acm.repository.AcmIhmFormRepository;
import com.acm.service.impl.AcmIhmFormServiceImpl;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmFormDTO;
import com.acm.utils.dtos.AcmIhmValidatorDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmIhmField;
import com.acm.utils.models.AcmIhmForm;
import com.acm.utils.models.HabilitationIHMRoute;

/**
 * {@link AcmIhmFormServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class AcmIhmFormServiceTest {

	/** The acm ihm form service. */
	@InjectMocks
	private AcmIhmFormServiceImpl acmIhmFormService;

	/** The acm ihm form repository. */
	@Mock
	private AcmIhmFormRepository acmIhmFormRepository;

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
	 * Inits the acm ihm form.
	 *
	 * @return the acm ihm form
	 */
	private AcmIhmForm initAcmIhmForm() {

		AcmIhmForm acmIhmForm = new AcmIhmForm();
		acmIhmForm.setId(new Long(1));
		acmIhmForm.setCodePage("PAGE-TEST");
		return acmIhmForm;

	}

	/**
	 * Inits the acm ihm form DTO.
	 * 
	 * @author ManelLamloum
	 * @return the acm ihm form DTO
	 */
	private AcmIhmFormDTO initAcmIhmFormDTO() {

		AcmIhmFormDTO acmIhmFormDTO = new AcmIhmFormDTO();
		HabilitationIHMRouteDTO habilitationIHMRouteDTO = new HabilitationIHMRouteDTO();
		habilitationIHMRouteDTO.setId(new Long(1));
		habilitationIHMRouteDTO.setCodeIHMRoute("code-test");
		acmIhmFormDTO.setHabilitationIHMRouteDTO(habilitationIHMRouteDTO);
		acmIhmFormDTO.setId(new Long(1));
		acmIhmFormDTO.setCodePage("PAGE-TEST");
		return acmIhmFormDTO;
	}

	/**
	 * Inits the acm ihm field DTO.
	 * 
	 * @author ManelLamloum
	 * @return the acm ihm field DTO
	 */
	private AcmIhmFieldDTO initAcmIhmFieldDTO() {

		AcmIhmFieldDTO acmIhmFieldDTO = new AcmIhmFieldDTO();
		AcmIhmFormDTO acmIhmFormDTO = new AcmIhmFormDTO();
		acmIhmFormDTO.setId(new Long(1));
		acmIhmFormDTO.setCodePage("PAGE-TEST");
		acmIhmFieldDTO.setAcmIhmFormDTO(acmIhmFormDTO);
		acmIhmFieldDTO.setValidators(new ArrayList<AcmIhmValidatorDTO>());
		acmIhmFieldDTO.setCodeField("test-code");
		acmIhmFieldDTO.setCodeForm("Code-form-test");
		acmIhmFieldDTO.setId(new Long(1));
		return acmIhmFieldDTO;
	}

	/**
	 * Inits the acm ihm field.
	 * 
	 * @author ManelLamloum
	 * @return the acm ihm field
	 */
	private AcmIhmField initAcmIhmField() {

		AcmIhmField acmIhmField = new AcmIhmField();
		AcmIhmForm acmIhmForm = new AcmIhmForm();
		acmIhmForm.setId(new Long(1));
		acmIhmForm.setCodePage("PAGE-TEST");
		acmIhmField.setCodeField("test-code");
		acmIhmField.setAcmIhmForm(acmIhmForm);
		acmIhmField.setId(new Long(1));
		return acmIhmField;
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
	 * Should return list of acm ihm form DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListOfAcmIhmFormDTO() {

		// GIVEN
		AcmIhmForm acmIhmForm = initAcmIhmForm();
		AcmIhmFormDTO acmIhmFormDTO = initAcmIhmFormDTO();
		AcmIhmField acmIhmField = initAcmIhmField();
		AcmIhmFieldDTO acmIhmFieldDTO = initAcmIhmFieldDTO();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmIhmFormRepository.findAll()).willReturn(Collections.singletonList(acmIhmForm));
		doReturn(acmIhmFormDTO).when(mapper).map(acmIhmForm, AcmIhmFormDTO.class);
		doReturn(acmIhmFieldDTO).when(mapper).map(acmIhmField, AcmIhmFieldDTO.class);

		// WHEN
		List<AcmIhmFormDTO> result = acmIhmFormService.find(acmIhmFormDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success create acm ihm frorm DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateAcmIhmFrormDTO() {

		// GIVEN
		AcmIhmForm acmIhmForm = initAcmIhmForm();
		AcmIhmFormDTO acmIhmFormDTO = initAcmIhmFormDTO();
		HabilitationIHMRouteDTO habilitationIHMRouteDTO = new HabilitationIHMRouteDTO();
		HabilitationIHMRoute habilitationIHMRoute = new HabilitationIHMRoute();

		given(userClient.find()).willReturn(initUserDTO());
		doReturn(acmIhmForm).when(mapper).map(acmIhmFormDTO, AcmIhmForm.class);
		doReturn(acmIhmFormDTO).when(mapper).map(acmIhmForm, AcmIhmFormDTO.class);
		doReturn(habilitationIHMRoute).when(mapper).map(habilitationIHMRouteDTO,
				HabilitationIHMRoute.class);
		given(acmIhmFormRepository.save(acmIhmForm)).willReturn(acmIhmForm);

		// WHEN
		AcmIhmFormDTO result = acmIhmFormService.save(acmIhmFormDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success update acm ihm form DTO.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateAcmIhmFormDTO() throws ResourcesNotFoundException {

		// GIVEN
		AcmIhmForm acmIhmForm = initAcmIhmForm();
		AcmIhmFormDTO acmIhmFormDTO = initAcmIhmFormDTO();
		given(acmIhmFormRepository.findById(new Long(1))).willReturn(Optional.of(acmIhmForm));
		given(acmIhmFormRepository.save(acmIhmForm)).willReturn(acmIhmForm);

		given(userClient.find()).willReturn(initUserDTO());
		doReturn(acmIhmFormDTO).when(mapper).map(acmIhmForm, AcmIhmFormDTO.class);

		// WHEN
		AcmIhmFormDTO result = acmIhmFormService.save(acmIhmFormDTO, acmIhmFormDTO.getId());

		// THEN
		assertThat(result).isNotNull();
	}
}
