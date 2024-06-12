/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

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
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ProductCategoryRepository;
import com.acm.service.impl.ProductCategoryServiceImpl;
import com.acm.utils.dtos.ProductCategoryDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.ProductCategory;

/**
 * {@link ProductCategoryServiceTest} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
class ProductCategoryServiceTest {

	/** The product category service. */
	@InjectMocks
	private ProductCategoryServiceImpl productCategoryService;

	/** The product category repository. */
	@Mock
	private ProductCategoryRepository productCategoryRepository;

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
	 * Inits the product category.
	 *
	 * @return the product category
	 */
	private ProductCategory initProductCategory() {

		ProductCategory productCategory = new ProductCategory();
		productCategory.setAcmVersion(0);
		return productCategory;
	}

	/**
	 * Inits the product category DTO.
	 *
	 * @return the product category DTO
	 */
	private ProductCategoryDTO initProductCategoryDTO() {

		ProductCategoryDTO productCategoryDTO = new ProductCategoryDTO();
		productCategoryDTO.setId(new Long(1));
		productCategoryDTO.setCode("code");
		productCategoryDTO.setDescription("Test Desc 1");
		return productCategoryDTO;
	}

	/**
	 * Initialize a new UserDTO.
	 * 
	 * @return a new UserDTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin("login");
		userDTO.setPrenom("benTest");
		return userDTO;
	}

	/**
	 * Should success save.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessSave() {

		// GIVEN
		ProductCategoryDTO productCategoryDTO = initProductCategoryDTO();
		ProductCategory productCategory = initProductCategory();
		given(productCategoryRepository.save(any(ProductCategory.class)))
				.willReturn(productCategory);
		given(mapper.map(productCategory, ProductCategoryDTO.class)).willReturn(productCategoryDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		ProductCategoryDTO productCategoryDTOReponse =
				productCategoryService.save(productCategoryDTO);

		// THEN
		assertThat(productCategoryDTOReponse).isNotNull();
	}

	/**
	 * Should success update.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		ProductCategoryDTO productCategoryDTO = initProductCategoryDTO();
		productCategoryDTO.setId(new Long(1));
		ProductCategory productCategory = mapper.map(productCategoryDTO, ProductCategory.class);
		productCategory.setAcmVersion(0);
		given(productCategoryRepository.findById(any(Long.class)))
				.willReturn(Optional.of(productCategory));
		given(productCategoryRepository.save(any(ProductCategory.class)))
				.willReturn(productCategory);
		given(mapper.map(productCategory, ProductCategoryDTO.class)).willReturn(productCategoryDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		ProductCategoryDTO productCategoryDTOReponse =
				productCategoryService.save(productCategory.getId(), productCategoryDTO);

		// THEN
		assertThat(productCategoryDTOReponse).isNotNull();
	}

	/**
	 * Should success find calendar event by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindProductCategoryByID() throws ResourcesNotFoundException {

		// GIVEN
		given(productCategoryRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initProductCategory()));

		// WHEN
		ProductCategoryDTO productCategoryDTO = productCategoryService.find(new Long(1));

		// THEN
		assertThat(productCategoryDTO).isNotNull();
	}

	/**
	 * Should success find List productCategory.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindListProductCategory() {

		// GIVEN
		ProductCategory productCategory = initProductCategory();
		ProductCategoryDTO productCategoryDTO = initProductCategoryDTO();
		given(mapper.map(productCategory, ProductCategoryDTO.class)).willReturn(productCategoryDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		List<ProductCategoryDTO> result = productCategoryService.find(productCategoryDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

}
