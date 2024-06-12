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
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingDocumentProductRepository;
import com.acm.service.impl.SettingDocumentProductServiceImpl;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingDocumentProduct;
import com.acm.utils.models.SettingDocumentType;

/**
 * {@link SettingDocumentProductServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingDocumentProductServiceTest {

	/** The setting document product service. */
	@InjectMocks
	private SettingDocumentProductServiceImpl settingDocumentProductService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The setting document product repository. */
	@Mock
	private SettingDocumentProductRepository settingDocumentProductRepository;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The setting historique service. */
	@Mock
	private SettingHistoriqueService settingHistoriqueService;

	/**
	 * Inits the setting document product DTO.
	 *
	 * @return the setting document product DTO
	 */
	private SettingDocumentProductDTO initSettingDocumentProductDTO() {

		SettingDocumentProductDTO settingDocumentProductDTO = new SettingDocumentProductDTO();
		settingDocumentProductDTO.setId(new Long(1));
		settingDocumentProductDTO.setProductId(1);
		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		settingDocumentTypeDTO.setId(new Long(1));
		settingDocumentProductDTO.setSettingDocumentTypeDTO(settingDocumentTypeDTO);
		return settingDocumentProductDTO;
	}

	/**
	 * Inits the setting document product.
	 *
	 * @return the setting document product
	 */
	private SettingDocumentProduct initSettingDocumentProduct() {

		SettingDocumentProduct settingDocumentProduct = new SettingDocumentProduct();
		settingDocumentProduct.setId(new Long(1));
		settingDocumentProduct.setProductId(1);
		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setId(new Long(1));
		settingDocumentProduct.setSettingDocumentType(settingDocumentType);
		return settingDocumentProduct;
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
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Should return list setting document product DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingDocumentProductDTO() {

		// GIVEN
		SettingDocumentProductDTO settingDocumentProductDTO = initSettingDocumentProductDTO();
		SettingDocumentProduct settingDocumentProduct = initSettingDocumentProduct();
		given(settingDocumentProductRepository.findAll())
				.willReturn(Collections.singletonList(new SettingDocumentProduct()));
		given(mapper.map(settingDocumentProduct, SettingDocumentProductDTO.class))
				.willReturn(settingDocumentProductDTO);

		// WHEN
		List<SettingDocumentProductDTO> result =
				settingDocumentProductService.find(settingDocumentProductDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success create.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreate() {

		// GIVEN
		SettingDocumentProductDTO settingDocumentProductDTO = initSettingDocumentProductDTO();
		SettingDocumentProduct settingDocumentProduct = initSettingDocumentProduct();
		doReturn(initUserDTO()).when(userClient).find();

		doReturn(settingDocumentProductDTO).when(mapper).map(settingDocumentProduct,
				SettingDocumentProductDTO.class);
		doReturn(settingDocumentProduct).when(mapper).map(settingDocumentProductDTO,
				SettingDocumentProduct.class);
		given(settingDocumentProductRepository.save(any(SettingDocumentProduct.class)))
				.willReturn(settingDocumentProduct);

		// WHEN
		SettingDocumentProductDTO result =
				settingDocumentProductService.save(settingDocumentProductDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
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
		SettingDocumentProductDTO settingDocumentProductDTO = initSettingDocumentProductDTO();
		SettingDocumentProduct settingDocumentProduct = initSettingDocumentProduct();
		given(settingDocumentProductRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingDocumentProduct));
		doReturn(settingDocumentProductDTO).when(mapper).map(settingDocumentProduct,
				SettingDocumentProductDTO.class);
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());
		doReturn(initUserDTO()).when(userClient).find();
		given(settingDocumentProductRepository.save(any(SettingDocumentProduct.class)))
				.willReturn(settingDocumentProduct);

		// WHEN
		SettingDocumentProductDTO result = settingDocumentProductService
				.save(settingDocumentProductDTO.getId(), settingDocumentProductDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();

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
		SettingDocumentProductDTO settingDocumentProductDTO = initSettingDocumentProductDTO();
		SettingDocumentProduct settingDocumentProduct = initSettingDocumentProduct();

		given(settingDocumentProductRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingDocumentProduct));
		doReturn(settingDocumentProductDTO).when(mapper).map(settingDocumentProduct,
				SettingDocumentProductDTO.class);
		doReturn(settingDocumentProduct).when(mapper).map(settingDocumentProductDTO,
				SettingDocumentProduct.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingDocumentProductRepository.save(any(SettingDocumentProduct.class)))
				.willReturn(settingDocumentProduct);
		given(settingHistoriqueService.save(any(SettingHistoriqueDTO.class)))
				.willReturn(new SettingHistoriqueDTO());

		// WHEN
		SettingDocumentProductDTO result = settingDocumentProductService.find(new Long(1));
		// THEN
		Assertions.assertThat(result).isNotNull();
	}
}
