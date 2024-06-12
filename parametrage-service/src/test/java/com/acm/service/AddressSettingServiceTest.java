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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Query;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionTemplate;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AddressSettingRepository;
import com.acm.service.impl.AddressSettingServiceImpl;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressSettingDTO;
import com.acm.utils.dtos.AddressTypeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AddressSetting;
import com.querydsl.core.types.Predicate;

/**
 * {@link AddressSettingServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
@SpringBootTest(classes = {EntityManager.class, EntityManagerFactory.class})
public class AddressSettingServiceTest {

	/** The address setting service. */
	@InjectMocks
	private AddressSettingServiceImpl addressSettingService;

	/** The address setting service mock. */
	@Mock
	private AddressSettingServiceImpl addressSettingServiceMock;

	/** The address setting repository. */
	@Mock
	private AddressSettingRepository addressSettingRepository;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The transaction manager. */
	@Spy
	private PlatformTransactionManager transactionManager;

	/** The transaction template. */
	@Mock
	private TransactionTemplate transactionTemplate;

	/** The entity manager. */
	private EntityManager entityManager;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The setting transvers client. */
	@Mock
	private TransversClient settingTransversClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The transaction status. */
	@Mock
	private TransactionStatus transactionStatus;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
		entityManager = Mockito.mock(EntityManager.class);
		ReflectionTestUtils.setField(addressSettingService, "entityManager", entityManager);
		// transactionTemplate = new TransactionTemplate(transactionManager);
		// transactionTemplate.setTransactionManager(transactionManager);
		// transactionStatus = Mockito.mock(TransactionStatus.class);
	}

	/**
	 * Inits the address setting.
	 *
	 * @return the address setting
	 */
	private AddressSetting initAddressSetting() {

		AddressSetting addressSetting = new AddressSetting();
		addressSetting.setId(new Long(1));
		addressSetting.setAcmVersion(0);
		return addressSetting;
	}

	/**
	 * Inits the address setting without id.
	 *
	 * @return the address setting
	 */
	private AddressSetting initAddressSettingWithoutId() {

		AddressSetting addressSetting = new AddressSetting();
		addressSetting.setAcmVersion(0);
		return addressSetting;
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
	 * Inits the address setting DTO.
	 *
	 * @return the address setting DTO
	 */
	private AddressSettingDTO initAddressSettingDTO() {

		AddressSettingDTO addressSettingDTO = new AddressSettingDTO();
		List<String> idAddressList = new ArrayList<>();
		idAddressList.add("1");
		idAddressList.add("2");
		idAddressList.add("3");
		idAddressList.add("4");
		addressSettingDTO.setTableAbacusName("test-table");
		addressSettingDTO.setAddressListId(new Long(1));
		addressSettingDTO.setIdAddressList(idAddressList);
		addressSettingDTO.setIdExtern("123");
		addressSettingDTO.setParentId(new Long(1));
		return addressSettingDTO;
	}

	/**
	 * Should success find by id address setting DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldFailFindByIdAddressSettingDTO() {

		// GIVEN
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			addressSettingService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("AddressSetting with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success find by id address setting DTO.
	 *
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindByIdAddressSettingDTO() throws ResourcesNotFoundException {

		// GIVEN
		AddressSetting addressSetting = initAddressSetting();
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		given(addressSettingRepository.findById(any(Long.class)))
				.willReturn(Optional.of(addressSetting));
		// WHEN
		AddressSettingDTO result = addressSettingService.find(new Long(1));
		// THEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should return list address setting.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListAddressSetting() {

		// GIVEN
		AddressSetting addressSetting = initAddressSetting();
		AddressSettingDTO addressSettingDTO = initAddressSettingDTO();
		doReturn(Collections.singletonList(addressSetting)).when(addressSettingRepository)
				.findAll(any(Predicate.class));
		given(mapper.map(addressSetting, AddressSettingDTO.class)).willReturn(addressSettingDTO);
		// WHEN
		List<AddressSettingDTO> addressSettingDTOs = addressSettingService.find(addressSettingDTO);
		// THEN
		Assertions.assertThat(addressSettingDTOs).isNotNull();
	}

	/**
	 * Sould success create address setting.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void souldSuccessCreateAddressSetting() {

		// GIVEN
		AddressSetting addressSetting = initAddressSetting();
		AddressSetting addressSettingWithoutId = initAddressSettingWithoutId();
		AddressSettingDTO addressSettingDTO = initAddressSettingDTO();
		given(mapper.map(addressSettingDTO, AddressSetting.class))
				.willReturn(addressSettingWithoutId);
		given(addressSettingRepository.save(addressSettingWithoutId)).willReturn(addressSetting);
		doReturn(initUserDTO()).when(userClient).find();
		// WHEN
		AddressSettingDTO addressSettingDTOResult = addressSettingService.save(addressSettingDTO);
		// THEN
		Assertions.assertThat(addressSettingDTOResult).isNotNull();

	}

	/**
	 * Sould fail update address setting.
	 *
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void souldFailUpdateAddressSetting() throws ResourcesNotFoundException {

		// GIVEN
		AddressSetting addressSetting = initAddressSetting();
		AddressSettingDTO addressSettingDTO = initAddressSettingDTO();
		addressSettingDTO.setId(new Long(1));
		doReturn(initUserDTO()).when(userClient).find();
		given(addressSettingRepository.findById(new Long(1))).willReturn(Optional.empty());
		given(addressSettingRepository.save(addressSetting)).willReturn(addressSetting);
		// WHEN
		try {
			addressSettingService.save(addressSettingDTO.getId(), addressSettingDTO);
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("AddressSetting with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success update address setting DTO.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateAddressSettingDTO() throws ResourcesNotFoundException {

		// GIVEN
		AddressSetting addressSetting = initAddressSetting();
		AddressSettingDTO addressSettingDTO = initAddressSettingDTO();
		addressSettingDTO.setId(new Long(1));
		doReturn(initUserDTO()).when(userClient).find();
		given(addressSettingRepository.findById(new Long(1)))
				.willReturn(Optional.of(addressSetting));
		given(addressSettingRepository.save(addressSetting)).willReturn(addressSetting);
		// WHEN
		AddressSettingDTO addressSettingDTOResult =
				addressSettingService.save(addressSettingDTO.getId(), addressSettingDTO);
		// THEN
		Assertions.assertThat(addressSettingDTOResult).isNotNull();

	}

	/**
	 * Should success load setting from abacus.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessLoadSettingFromAbacus() {

		// GIVEN
		AddressTypeDTO addressTypeDTO = new AddressTypeDTO();
		AddressListDTO addressListDTO = new AddressListDTO();
		addressListDTO.setParentAddressListID(new Integer(1));
		addressListDTO.setAddressListID(1);
		AddressListValueDTO addressListValueDTO = new AddressListValueDTO();
		addressListValueDTO.setAddressListValueID(1);
		addressListValueDTO.setAddressListID(1);
		AddressSettingAbacusDTO settingAbacusDTO = new AddressSettingAbacusDTO();
		settingAbacusDTO.setAddressField("test field");
		AddressSettingDTO addressSettingDTOWithoutId = initAddressSettingDTO();
		AddressSettingDTO addressSettingDTOWithId = initAddressSettingDTO();
		addressSettingDTOWithId.setId(new Long(1));
		AddressSetting addressSettingWithId = initAddressSetting();
		AddressSetting addressSettingWithoutId = initAddressSettingWithoutId();
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(addressSettingDTOWithId).when(mapper).map(addressSettingWithId,
				AddressSettingDTO.class);
		doReturn(addressSettingWithoutId).when(mapper).map(addressSettingDTOWithoutId,
				AddressSetting.class);
		doReturn(addressSettingWithId).when(addressSettingRepository)
				.save(any(AddressSetting.class));
		doReturn(Collections.singletonList(addressTypeDTO)).when(settingTransversClient)
				.findAddressType();
		doReturn(Collections.singletonList(addressListDTO)).when(settingTransversClient)
				.findAddressList();
		doReturn(Collections.singletonList(addressListValueDTO)).when(settingTransversClient)
				.findAddressListValue();
		doReturn(Collections.singletonList(settingAbacusDTO)).when(settingTransversClient)
				.findSettingsAddress();
		// WHEN
		addressSettingService.loadSettingFromAbacus();
		// THEN
		verify(settingTransversClient, times(1)).findAddressType();
		verify(settingTransversClient, times(1)).findAddressList();
		verify(settingTransversClient, times(1)).findAddressListValue();
	}

	/**
	 * Should success reset setting from abacus.
	 */
	@Disabled
	@Test
	void shouldSuccessResetSettingFromAbacus() {

		// GIVEN
		doNothing().when(addressSettingRepository).deleteAll();
		// GIVEN OF loadSettingFromAbacus method
		AddressTypeDTO addressTypeDTO = new AddressTypeDTO();
		AddressListDTO addressListDTO = new AddressListDTO();
		addressListDTO.setParentAddressListID(new Integer(1));
		addressListDTO.setAddressListID(1);
		AddressListValueDTO addressListValueDTO = new AddressListValueDTO();
		addressListValueDTO.setAddressListValueID(1);
		addressListValueDTO.setAddressListID(1);
		AddressSettingAbacusDTO settingAbacusDTO = new AddressSettingAbacusDTO();
		settingAbacusDTO.setAddressField("test field");
		AddressSettingDTO addressSettingDTOWithoutId = initAddressSettingDTO();
		AddressSettingDTO addressSettingDTOWithId = initAddressSettingDTO();
		addressSettingDTOWithId.setId(new Long(1));
		AddressSetting addressSettingWithId = initAddressSetting();
		AddressSetting addressSettingWithoutId = initAddressSettingWithoutId();
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(addressSettingDTOWithId).when(mapper).map(addressSettingWithId,
				AddressSettingDTO.class);
		doReturn(addressSettingWithoutId).when(mapper).map(addressSettingDTOWithoutId,
				AddressSetting.class);
		doReturn(addressSettingWithId).when(addressSettingRepository)
				.save(any(AddressSetting.class));
		doReturn(Collections.singletonList(addressTypeDTO)).when(settingTransversClient)
				.findAddressType();
		doReturn(Collections.singletonList(addressListDTO)).when(settingTransversClient)
				.findAddressList();
		doReturn(Collections.singletonList(addressListValueDTO)).when(settingTransversClient)
				.findAddressListValue();
		doReturn(Collections.singletonList(settingAbacusDTO)).when(settingTransversClient)
				.findSettingsAddress();
		// WHEN
		addressSettingService.resetSettingFromAbacus();

	}

	/**
	 * Should success reset setting from Abacus.
	 * 
	 * @author ManelLamloum
	 */
	@Disabled
	@Test
	void shouldSuccessResetPrimaryKey() {

		// GIVEN
		Query q = mock(Query.class);
		doNothing().when(addressSettingServiceMock).loadSettingFromAbacus();
		when(entityManager.createNativeQuery(any(String.class))).thenReturn(q);
		doReturn(1).when(Mockito.mock(Query.class)).executeUpdate();
		doNothing().when(Mockito.mock(TransactionStatus.class)).flush();
		// doReturn(1).when(Mockito.mock(TransactionTemplate.class)).execute(null);
		// when(q.executeUpdate()).thenReturn(1);
		// doNothing().when(entityManager).flush();
		// WHEN
		addressSettingService.resetSettingFromAbacus();
		// THEN
		verify(addressSettingService, times(1)).resetSettingFromAbacus();
	}
}
