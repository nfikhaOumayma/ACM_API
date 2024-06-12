/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

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
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingMotifRejetsRepository;
import com.acm.service.impl.SettingMotifRejetsServiceImpl;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingMotifRejets;

/**
 * {@link SettingMotifRejetsServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingMotifRejetsServiceTest {

	/** The setting motif rejets service. */
	@InjectMocks
	private SettingMotifRejetsServiceImpl settingMotifRejetsService;

	/** The setting motif rejets repository. */
	@Mock
	private SettingMotifRejetsRepository settingMotifRejetsRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

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
	 * Inits the setting motif rejets DTO.
	 *
	 * @return the setting motif rejets DTO
	 */
	private SettingMotifRejetsDTO initSettingMotifRejetsDTO() {

		SettingMotifRejetsDTO settingMotifRejetsDTO = new SettingMotifRejetsDTO();
		settingMotifRejetsDTO.setEnabled(Boolean.TRUE);
		settingMotifRejetsDTO.setCategorie("test-categorie");
		settingMotifRejetsDTO.setId(new Long(1));
		return settingMotifRejetsDTO;
	}

	/**
	 * Inits the setting motif rejets.
	 *
	 * @return the setting motif rejets
	 */
	private SettingMotifRejets initSettingMotifRejets() {

		SettingMotifRejets settingMotifRejets = new SettingMotifRejets();
		settingMotifRejets.setEnabled(Boolean.TRUE);
		settingMotifRejets.setCategorie("test-categorie");
		settingMotifRejets.setId(new Long(1));
		return settingMotifRejets;
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
	 * Should return list setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingMotifRejetsDTO() {

		// GIVEN
		SettingMotifRejets settingMotifRejets = initSettingMotifRejets();
		SettingMotifRejetsDTO settingMotifRejetsDTO = initSettingMotifRejetsDTO();
		given(settingMotifRejetsRepository.findAll())
				.willReturn(Collections.singletonList(settingMotifRejets));
		doReturn(settingMotifRejetsDTO).when(mapper).map(settingMotifRejets,
				SettingMotifRejetsDTO.class);

		// WHEN
		List<SettingMotifRejetsDTO> result = settingMotifRejetsService.find(settingMotifRejetsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success create setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@Test
	void shouldSuccessCreateSettingMotifRejetsDTO() throws CodeSettingExistException {

		// GIVEN
		SettingMotifRejets settingMotifRejets = initSettingMotifRejets();
		SettingMotifRejetsDTO settingMotifRejetsDTO = initSettingMotifRejetsDTO();
		given(settingMotifRejetsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingMotifRejets));
		given(settingMotifRejetsRepository.findAll())
				.willReturn(Collections.singletonList(settingMotifRejets));
		doReturn(settingMotifRejetsDTO).when(mapper).map(settingMotifRejets,
				SettingMotifRejetsDTO.class);
		given(settingMotifRejetsRepository.save(any(SettingMotifRejets.class)))
				.willReturn(settingMotifRejets);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		SettingMotifRejetsDTO result = settingMotifRejetsService.save(settingMotifRejetsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success update setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	@Test
	void shouldSuccessUpdateSettingMotifRejetsDTO()
			throws ResourcesNotFoundException, CodeSettingExistException {

		// GIVEN
		SettingMotifRejets settingMotifRejets = initSettingMotifRejets();
		SettingMotifRejetsDTO settingMotifRejetsDTO = initSettingMotifRejetsDTO();
		given(settingMotifRejetsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingMotifRejets));
		given(settingMotifRejetsRepository.findAll())
				.willReturn(Collections.singletonList(settingMotifRejets));
		doReturn(settingMotifRejetsDTO).when(mapper).map(settingMotifRejets,
				SettingMotifRejetsDTO.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingMotifRejetsRepository.save(any(SettingMotifRejets.class)))
				.willReturn(settingMotifRejets);
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		// WHEN
		SettingMotifRejetsDTO result = settingMotifRejetsService.save(settingMotifRejetsDTO.getId(),
				settingMotifRejetsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail update setting motif rejets DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailUpdateSettingMotifRejetsDTO() {

		// GIVEN
		given(settingMotifRejetsRepository.findById(any(Long.class))).willReturn(Optional.empty());

		// WHEN
		try {
			settingMotifRejetsService.save(new Long(1), new SettingMotifRejetsDTO());
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingMotifRejets with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should fail delete.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailDelete() {

		// GIVEN
		given(settingMotifRejetsRepository.findById(any(Long.class))).willReturn(Optional.empty());
		// WHEN
		try {
			settingMotifRejetsService.delete(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingMotifRejets with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success delete.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessDelete() throws ResourcesNotFoundException {

		// GIVEN
		given(settingMotifRejetsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new SettingMotifRejets()));
		doNothing().when(settingMotifRejetsRepository).delete(any(SettingMotifRejets.class));
		// WHEN
		settingMotifRejetsService.delete(new Long(1));

		// THEN
		verify(settingMotifRejetsRepository, times(1)).delete(any(SettingMotifRejets.class));

	}
}
