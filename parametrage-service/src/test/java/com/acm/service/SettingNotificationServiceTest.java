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

import com.acm.client.CreditClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingNotificationsRepository;
import com.acm.service.impl.SettingNotificationsServiceImpl;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingNotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UsersNotificationsDTO;
import com.acm.utils.models.SettingNotifications;

/**
 * {@link } class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingNotificationServiceTest {

	/** The setting notifications service. */
	@InjectMocks
	private SettingNotificationsServiceImpl settingNotificationsService;

	/** The setting notifications repository. */
	@Mock
	private SettingNotificationsRepository settingNotificationsRepository;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

	/** The credit client. */
	@Mock
	private CreditClient creditClient;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the setting notifications DTO.
	 *
	 * @return the setting notifications DTO
	 */
	private SettingNotificationsDTO initSettingNotificationsDTO() {

		SettingNotificationsDTO settingNotificationsDTO = new SettingNotificationsDTO();
		settingNotificationsDTO.setIdSettingNotification(new Long(1));
		settingNotificationsDTO.setCategory("Test");
		settingNotificationsDTO.setTypeNotif("test");
		return settingNotificationsDTO;
	}

	/**
	 * Inits the setting notifications.
	 *
	 * @return the setting notifications
	 */
	private SettingNotifications initSettingNotifications() {

		SettingNotifications settingNotifications = new SettingNotifications();
		settingNotifications.setIdSettingNotification(new Long(1));
		settingNotifications.setCategory("Test");
		settingNotifications.setTypeNotif("test");
		return settingNotifications;
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
		given(settingNotificationsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new SettingNotifications()));
		doReturn(new SettingNotifications()).when(mapper).map(new SettingNotificationsDTO(),
				SettingNotifications.class);

		// WHEN
		SettingNotificationsDTO result = settingNotificationsService.find(new Long(1));
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
		given(settingNotificationsRepository.findById(any(Long.class)))
				.willReturn(Optional.empty());

		// WHEN
		try {
			settingNotificationsService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingNotifications with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should return list setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingNotificationsDTO() {

		// GIVEN
		SettingNotificationsDTO settingNotificationsDTO = initSettingNotificationsDTO();
		SettingNotifications settingNotifications = initSettingNotifications();

		given(settingNotificationsRepository.findAll())
				.willReturn(Collections.singletonList(settingNotifications));
		doReturn(settingNotificationsDTO).when(mapper).map(settingNotifications,
				SettingNotificationsDTO.class);

		// WHEN
		List<SettingNotificationsDTO> result =
				settingNotificationsService.find(settingNotificationsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success create setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateSettingNotificationsDTO() {

		// GIVEN
		given(settingNotificationsRepository.save(any(SettingNotifications.class)))
				.willReturn(new SettingNotifications());
		doReturn(new SettingNotificationsDTO()).when(mapper).map(new SettingNotifications(),
				SettingNotificationsDTO.class);
		doReturn(new SettingNotifications()).when(mapper).map(new SettingNotificationsDTO(),
				SettingNotifications.class);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		SettingNotificationsDTO result =
				settingNotificationsService.save(new SettingNotificationsDTO());
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail update setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailUpdateSettingNotificationsDTO() {

		// GIVEN
		given(settingNotificationsRepository.findById(any(Long.class)))
				.willReturn(Optional.empty());

		// WHEN
		try {
			settingNotificationsService.save(new Long(1), new SettingNotificationsDTO());
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingNotifications with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success update setting notifications DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateSettingNotificationsDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingNotificationsDTO settingNotificationsDTO = new SettingNotificationsDTO();
		settingNotificationsDTO.setEnabled(Boolean.TRUE);
		settingNotificationsDTO.setUpdateUserNotification(Boolean.TRUE);
		SettingNotifications settingNotifications = new SettingNotifications();
		given(settingNotificationsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingNotifications));
		doReturn(new SettingNotifications()).when(mapper).map(new SettingNotificationsDTO(),
				SettingNotifications.class);
		doReturn(settingNotificationsDTO).when(mapper).map(settingNotifications,
				SettingNotificationsDTO.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingNotificationsRepository.save(any(SettingNotifications.class)))
				.willReturn(settingNotifications);
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		given(creditClient.updateAll(any(Long.class), any(Boolean.class)))
				.willReturn(Collections.singletonList(new UsersNotificationsDTO()));
		// WHEN
		SettingNotificationsDTO result =
				settingNotificationsService.save(new Long(1), settingNotificationsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFind() {

		// GIVEN
		given(settingNotificationsRepository.findAll())
				.willReturn(Collections.singletonList(new SettingNotifications()));
		doReturn(new SettingNotificationsDTO()).when(mapper).map(new SettingNotifications(),
				SettingNotificationsDTO.class);

		// WHEN
		List<SettingNotificationsDTO> result = settingNotificationsService.find("GLOBAL");
		// THEN
		Assertions.assertThat(result).isNotNull();
	}
}
