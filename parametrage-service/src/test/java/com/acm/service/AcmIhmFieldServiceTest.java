/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
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
import com.acm.repository.AcmIhmFieldRepository;
import com.acm.service.impl.AcmIhmFieldServiceImpl;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmFormDTO;
import com.acm.utils.dtos.AcmIhmValidatorDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmIhmField;
import com.acm.utils.models.AcmIhmForm;
import com.acm.utils.models.AcmIhmValidator;

/**
 * {@link AcmIhmFieldServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class AcmIhmFieldServiceTest {

	/** The acm ihm field service. */
	@InjectMocks
	private AcmIhmFieldServiceImpl acmIhmFieldService;

	/** The acm ihm field repository. */
	@Mock
	private AcmIhmFieldRepository acmIhmFieldRepository;

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
	 * Inits the acm ihm field DTO.
	 *
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
	 * Should return list acm ihm field.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListAcmIhmField() {

		// GIVEN
		AcmIhmField acmIhmField = initAcmIhmField();
		AcmIhmFieldDTO acmIhmFieldDTO = initAcmIhmFieldDTO();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmIhmFieldRepository.findAll()).willReturn(Collections.singletonList(acmIhmField));
		given(mapper.map(acmIhmField, AcmIhmFieldDTO.class)).willReturn(acmIhmFieldDTO);
		// WHEN
		List<AcmIhmFieldDTO> result = acmIhmFieldService.find(acmIhmFieldDTO);
		// THEN
		assertThat(result).isNotNull();
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
		AcmIhmField acmIhmField = initAcmIhmField();
		AcmIhmFieldDTO acmIhmFieldDTO = initAcmIhmFieldDTO();
		AcmIhmValidatorDTO acmIhmValidatorDTO = new AcmIhmValidatorDTO();
		AcmIhmValidator acmIhmValidator = new AcmIhmValidator();
		given(userClient.find()).willReturn(initUserDTO());
		given(acmIhmFieldRepository.findById(any(Long.class))).willReturn(Optional.of(acmIhmField));

		given(acmIhmFieldRepository.save(acmIhmField)).willReturn(acmIhmField);
		doReturn(acmIhmValidator).when(mapper).map(acmIhmValidatorDTO, AcmIhmValidator.class);
		doReturn(acmIhmField).when(mapper).map(acmIhmFieldDTO, AcmIhmValidator.class);

		// WHEN
		AcmIhmFieldDTO result = acmIhmFieldService.save(acmIhmFieldDTO.getId(), acmIhmFieldDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Sould success save all.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void souldSuccessSaveAll() {

		// GIVEN
		AcmIhmFieldDTO acmIhmFieldDTO = initAcmIhmFieldDTO();
		AcmIhmField acmIhmField = initAcmIhmField();
		AcmIhmForm acmIhmForm = initAcmIhmForm();
		AcmIhmFormDTO acmIhmFormDTO = new AcmIhmFormDTO();
		AcmIhmValidatorDTO acmIhmValidatorDTO = new AcmIhmValidatorDTO();
		AcmIhmValidator acmIhmValidator = new AcmIhmValidator();

		given(userClient.find()).willReturn(initUserDTO());
		doReturn(acmIhmField).when(mapper).map(acmIhmFieldDTO, AcmIhmField.class);
		doReturn(acmIhmForm).when(mapper).map(acmIhmFormDTO, AcmIhmForm.class);
		doReturn(acmIhmValidator).when(mapper).map(acmIhmValidatorDTO, AcmIhmValidator.class);
		doReturn(acmIhmFieldDTO).when(mapper).map(acmIhmField, AcmIhmFieldDTO.class);
		given(acmIhmFieldRepository.save(any(AcmIhmField.class))).willReturn(acmIhmField);

		// WHEN
		List<AcmIhmFieldDTO> result =
				acmIhmFieldService.saveAll(Collections.singletonList(acmIhmFieldDTO));

		// THEN
		assertThat(result).isNotNull();
	}
}
