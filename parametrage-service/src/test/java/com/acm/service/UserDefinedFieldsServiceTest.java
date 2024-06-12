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

import java.util.ArrayList;
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
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldsRepository;
import com.acm.service.impl.UserDefinedFieldsServiceImpl;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.enums.SettingUDFFieldsType;
import com.acm.utils.models.UserDefinedFields;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link UserDefinedFieldsServiceTest } class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class UserDefinedFieldsServiceTest {

	/** The user defined fields service. */
	@InjectMocks
	private UserDefinedFieldsServiceImpl userDefinedFieldsService;
	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;
	/** The environment. */
	@Mock
	private Environment environment;

	/** The user defined fields repository. */
	@Mock
	private UserDefinedFieldsRepository userDefinedFieldsRepository;

	/** The user defined field list values service. */
	@Mock
	private UserDefinedFieldListValuesService userDefinedFieldListValuesService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the user defined fields.
	 *
	 * @return the user defined fields
	 */
	private UserDefinedFields initUserDefinedFields() {

		UserDefinedFields userDefinedFields = new UserDefinedFields();

		userDefinedFields.setFieldType(SettingUDFFieldsType.LIST.typeId());
		userDefinedFields.setIdUDFListValue(new Long(1));
		return userDefinedFields;
	}

	/**
	 * Inits the user defined fields DTO.
	 *
	 * @return the user defined fields DTO
	 */
	private UserDefinedFieldsDTO initUserDefinedFieldsDTO() {

		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();
		userDefinedFieldsDTO.setId(new Long(1));
		userDefinedFieldsDTO.setIdUDFField(new Long(1));
		UserDefinedFieldGroupDTO userDefinedFieldGroupDTO = new UserDefinedFieldGroupDTO();
		userDefinedFieldGroupDTO.setId(new Long(1));
		userDefinedFieldsDTO.setUserDefinedFieldGroupDTO(userDefinedFieldGroupDTO);
		userDefinedFieldsDTO.setFieldType(SettingUDFFieldsType.LIST.typeId());
		userDefinedFieldsDTO.setIdUDFListValue(new Long(1));
		List<String> names = new ArrayList<String>();
		names.add("test");
		userDefinedFieldsDTO.setNames(names);
		return userDefinedFieldsDTO;
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
	 * Should success find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindById() throws ResourcesNotFoundException {

		// GIVEN
		UserDefinedFields userDefinedFields = new UserDefinedFields();
		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();
		given(userDefinedFieldsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(userDefinedFields));
		doReturn(userDefinedFieldsDTO).when(mapper).map(userDefinedFields,
				UserDefinedFieldsDTO.class);

		// WHEN
		UserDefinedFieldsDTO result = userDefinedFieldsService.find(new Long(1));

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail find by id.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailFindById() {

		// GIVEN
		given(userDefinedFieldsRepository.findById(any(Long.class))).willReturn(Optional.empty());

		// WHEN
		try {
			userDefinedFieldsService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("UserDefinedFields with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should return list user defined fields DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListUserDefinedFieldsDTO() {

		// GIVEN
		UserDefinedFields userDefinedFields = initUserDefinedFields();
		UserDefinedFieldsDTO userDefinedFieldsDTO = initUserDefinedFieldsDTO();

		given(userDefinedFieldsRepository.findAll(any(BooleanBuilder.class)))
				.willReturn(Collections.singletonList(userDefinedFields));
		doReturn(userDefinedFieldsDTO).when(mapper).map(userDefinedFields,
				UserDefinedFieldsDTO.class);
		given(userDefinedFieldListValuesService.find(any(UserDefinedFieldListValuesDTO.class)))
				.willReturn(Collections.singletonList(new UserDefinedFieldListValuesDTO()));

		// WHEN
		List<UserDefinedFieldsDTO> result = userDefinedFieldsService.find(userDefinedFieldsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should success create user defined fields DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateUserDefinedFieldsDTO() {

		// GIVEN
		UserDefinedFields userDefinedFields = new UserDefinedFields();
		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();

		doReturn(initUserDTO()).when(userClient).find();
		doReturn(userDefinedFieldsDTO).when(mapper).map(userDefinedFields,
				UserDefinedFieldsDTO.class);
		doReturn(userDefinedFields).when(mapper).map(userDefinedFieldsDTO, UserDefinedFields.class);
		given(userDefinedFieldsRepository.save(any(UserDefinedFields.class)))
				.willReturn(userDefinedFields);
		// WHEN
		UserDefinedFieldsDTO result =
				userDefinedFieldsService.save(userDefinedFieldsDTO, new UserDTO());
		// THEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should success update user defined fields DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException
	 */
	@Test
	void shouldSuccessUpdateUserDefinedFieldsDTO() throws ResourcesNotFoundException {

		// GIVEN
		UserDefinedFields userDefinedFields = new UserDefinedFields();
		UserDefinedFieldsDTO userDefinedFieldsDTO = new UserDefinedFieldsDTO();
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(userDefinedFieldsDTO).when(mapper).map(userDefinedFields,
				UserDefinedFieldsDTO.class);
		doReturn(userDefinedFields).when(mapper).map(userDefinedFieldsDTO, UserDefinedFields.class);
		given(userDefinedFieldsRepository.save(any(UserDefinedFields.class)))
				.willReturn(userDefinedFields);
		given(userDefinedFieldsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(userDefinedFields));

		// WHEN
		UserDefinedFieldsDTO result =
				userDefinedFieldsService.save(new Long(1), userDefinedFieldsDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}
}
