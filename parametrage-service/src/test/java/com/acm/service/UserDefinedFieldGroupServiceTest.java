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

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldGroupRepository;
import com.acm.service.impl.UserDefinedFieldGroupServiceImpl;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.models.UserDefinedFieldGroup;

/**
 * {@link UserDefinedFieldGroupServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class UserDefinedFieldGroupServiceTest {

	/** The user defined field group service. */
	@InjectMocks
	private UserDefinedFieldGroupServiceImpl userDefinedFieldGroupService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user defined field group repository. */
	@Mock
	private UserDefinedFieldGroupRepository userDefinedFieldGroupRepository;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The setting transvers client. */
	@Mock
	private TransversClient settingTransversClient;

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
	 * Inits the user defined field group DTO.
	 *
	 * @return the user defined field group DTO
	 */
	private UserDefinedFieldGroupDTO initUserDefinedFieldGroupDTO() {

		UserDefinedFieldGroupDTO userDefinedFieldGroupDTO = new UserDefinedFieldGroupDTO();
		userDefinedFieldGroupDTO.setId(new Long(1));
		userDefinedFieldGroupDTO.setIdUDGroupAbacus(new Long(1));
		userDefinedFieldGroupDTO.setLoanId(new Long(1));
		userDefinedFieldGroupDTO.setCustomerId(new Long(1));
		userDefinedFieldGroupDTO.setCustomerType(1);
		userDefinedFieldGroupDTO.setCustomerTypeLabel("test");
		return userDefinedFieldGroupDTO;
	}

	/**
	 * Should fail find by id.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailFindById() {

		// GIVEN
		given(userDefinedFieldGroupRepository.findById(any(Long.class)))
				.willReturn(Optional.empty());
		// WHEN
		try {
			userDefinedFieldGroupService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("UserDefinedFieldGroup with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
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
		UserDefinedFieldGroup userDefinedFieldGroup = new UserDefinedFieldGroup();
		given(userDefinedFieldGroupRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new UserDefinedFieldGroup()));
		doReturn(new UserDefinedFieldGroupDTO()).when(mapper).map(userDefinedFieldGroup,
				UserDefinedFieldGroupDTO.class);

		// WHEN
		UserDefinedFieldGroupDTO result = userDefinedFieldGroupService.find(new Long(1));
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should return list user defined field group DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListUserDefinedFieldGroupDTO() {

		// GIVEN
		UserDefinedFieldGroupDTO userDefinedFieldGroupDTO = initUserDefinedFieldGroupDTO();

		UserDefinedFieldGroup userDefinedFieldGroup = new UserDefinedFieldGroup();

		given(userDefinedFieldGroupRepository.findAll())
				.willReturn(Collections.singletonList(new UserDefinedFieldGroup()));
		doReturn(new UserDefinedFieldGroupDTO()).when(mapper).map(userDefinedFieldGroup,
				UserDefinedFieldGroupDTO.class);

		// WHEN

		List<UserDefinedFieldGroupDTO> result =
				userDefinedFieldGroupService.find(userDefinedFieldGroupDTO);
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success create user defined field group DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateUserDefinedFieldGroupDTO() {

		// GIVEN
		UserDefinedFieldGroup userDefinedFieldGroup = new UserDefinedFieldGroup();

		given(userDefinedFieldGroupRepository.save(any(UserDefinedFieldGroup.class)))
				.willReturn(userDefinedFieldGroup);
		doReturn(new UserDefinedFieldGroupDTO()).when(mapper).map(userDefinedFieldGroup,
				UserDefinedFieldGroupDTO.class);
		doReturn(userDefinedFieldGroup).when(mapper).map(new UserDefinedFieldGroupDTO(),
				UserDefinedFieldGroup.class);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		UserDefinedFieldGroupDTO result =
				userDefinedFieldGroupService.save(new UserDefinedFieldGroupDTO(), new UserDTO());
		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should fail update user defined field group DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailUpdateUserDefinedFieldGroupDTO() {

		// GIVEN
		given(userDefinedFieldGroupRepository.findById(any(Long.class)))
				.willReturn(Optional.empty());

		// WHEN
		try {
			userDefinedFieldGroupService.save(new Long(1), new UserDefinedFieldGroupDTO());
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("UserDefinedFieldGroup with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success update user defined field group DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateUserDefinedFieldGroupDTO() throws ResourcesNotFoundException {

		// GIVEN
		UserDefinedFieldGroup userDefinedFieldGroup = new UserDefinedFieldGroup();

		given(userDefinedFieldGroupRepository.findById(any(Long.class)))
				.willReturn(Optional.of(userDefinedFieldGroup));
		doReturn(initUserDTO()).when(userClient).find();
		given(userDefinedFieldGroupRepository.save(any(UserDefinedFieldGroup.class)))
				.willReturn(userDefinedFieldGroup);
		doReturn(new UserDefinedFieldGroupDTO()).when(mapper).map(userDefinedFieldGroup,
				UserDefinedFieldGroupDTO.class);

		// WHEN
		UserDefinedFieldGroupDTO result =
				userDefinedFieldGroupService.save(new Long(1), new UserDefinedFieldGroupDTO());

		// THEN
		assertThat(result).isNotNull();
	}

	/**
	 * Should success load setting from abacus case one.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessLoadSettingFromAbacusCaseOne() throws ResourcesNotFoundException {

		// GIVEN

		UserDefinedFieldGroup userDefinedFieldGroup = new UserDefinedFieldGroup();
		userDefinedFieldGroup.setId(new Long(1));
		UserDefinedFieldGroupDTO userDefinedFieldGroupDTO = new UserDefinedFieldGroupDTO();
		userDefinedFieldGroupDTO.setIdUDGroupAbacus(new Long(1));
		userDefinedFieldGroup.setId(new Long(1));
		given(settingTransversClient.findUserDefinedFieldGroup())
				.willReturn(Collections.singletonList(userDefinedFieldGroupDTO));

		doReturn(Collections.singletonList(userDefinedFieldGroup))
				.when(userDefinedFieldGroupRepository)
				.findByIdUDGroupAbacusAndEnabled(any(Long.class), any(Boolean.class));

		given(userDefinedFieldGroupRepository.save(any(UserDefinedFieldGroup.class)))
				.willReturn(userDefinedFieldGroup);
		doReturn(new UserDefinedFieldGroupDTO()).when(mapper).map(userDefinedFieldGroup,
				UserDefinedFieldGroupDTO.class);
		doReturn(userDefinedFieldGroup).when(mapper).map(new UserDefinedFieldGroupDTO(),
				UserDefinedFieldGroup.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(userDefinedFieldGroupRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new UserDefinedFieldGroup()));

		// WHEN
		userDefinedFieldGroupService.loadSettingFromAbacus(new UserDTO());

		// THEN
		verify(userDefinedFieldGroupRepository, times(1)).save(any(UserDefinedFieldGroup.class));
	}

	/**
	 * Should success load setting from abacus case two.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessLoadSettingFromAbacusCaseTwo() throws ResourcesNotFoundException {

		// GIVEN

		UserDefinedFieldGroup userDefinedFieldGroup = new UserDefinedFieldGroup();
		given(settingTransversClient.findUserDefinedFieldGroup())
				.willReturn(Collections.singletonList(new UserDefinedFieldGroupDTO()));
		given(userDefinedFieldGroupRepository.findByIdUDGroupAbacusAndEnabled(any(Long.class),
				any(Boolean.class))).willReturn(null);

		given(userDefinedFieldGroupRepository.save(any(UserDefinedFieldGroup.class)))
				.willReturn(userDefinedFieldGroup);
		doReturn(new UserDefinedFieldGroupDTO()).when(mapper).map(userDefinedFieldGroup,
				UserDefinedFieldGroupDTO.class);
		doReturn(userDefinedFieldGroup).when(mapper).map(new UserDefinedFieldGroupDTO(),
				UserDefinedFieldGroup.class);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		userDefinedFieldGroupService.loadSettingFromAbacus(new UserDTO());

		// THEN
		verify(userDefinedFieldGroupRepository, times(1)).save(any(UserDefinedFieldGroup.class));

	}
}
