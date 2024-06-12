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
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ProductDetailsRepository;
import com.acm.service.impl.ProductDetailsServiceImpl;
import com.acm.utils.dtos.ProductDetailsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.ProductDetails;

/**
 * {@link ProductDetailsServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class ProductDetailsServiceTest {

	/** The product details service. */
	@InjectMocks
	private ProductDetailsServiceImpl productDetailsService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The product details repository. */
	@Mock
	private ProductDetailsRepository productDetailsRepository;

	/** The environment. */
	@Mock
	private Environment environment;

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
	 * Should success find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindById() throws ResourcesNotFoundException {

		// GIVEN
		ProductDetails productDetails = new ProductDetails();
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		given(productDetailsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(productDetails));
		given(mapper.map(productDetails, ProductDetailsDTO.class)).willReturn(productDetailsDTO);

		// WHEN
		ProductDetailsDTO result = productDetailsService.find(new Long(1));
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldFailFindById() throws ResourcesNotFoundException {

		// GIVEN
		given(productDetailsRepository.findById(any(Long.class))).willReturn(Optional.empty());
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			productDetailsService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("ProductDetails with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success find product details DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindProductDetailsDTO() {

		// GIVEN
		ProductDetails productDetails = new ProductDetails();
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		given(productDetailsRepository.findAll())
				.willReturn(Collections.singletonList(productDetails));
		given(mapper.map(productDetails, ProductDetailsDTO.class)).willReturn(productDetailsDTO);

		// WHEN
		List<ProductDetailsDTO> result = productDetailsService.find(productDetailsDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find product details DTO by id product not empty.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindProductDetailsDTOByIdProductNotEmpty() {

		// GIVEN
		ProductDetails productDetails = new ProductDetails();
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		productDetailsDTO.setIdProduct(new Long(1));
		given(productDetailsRepository.findAll())
				.willReturn(Collections.singletonList(productDetails));
		given(mapper.map(productDetails, ProductDetailsDTO.class)).willReturn(productDetailsDTO);

		// WHEN
		List<ProductDetailsDTO> result = productDetailsService.find(productDetailsDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();

	}

	/**
	 * Should success create product details DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateProductDetailsDTO() {

		// GIVEN
		ProductDetails productDetails = new ProductDetails();
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		doReturn(productDetailsDTO).when(mapper).map(productDetails, ProductDetailsDTO.class);
		doReturn(productDetails).when(mapper).map(productDetailsDTO, ProductDetails.class);
		given(productDetailsRepository.save(any(ProductDetails.class))).willReturn(productDetails);
		doReturn(initUserDTO()).when(userClient).find();
		// WHEN
		ProductDetailsDTO result = productDetailsService.save(productDetailsDTO);
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
		ProductDetails productDetails = new ProductDetails();
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		productDetailsDTO.setId(new Long(1));
		given(productDetailsRepository.findById(any(Long.class)))
				.willReturn(Optional.of(productDetails));
		given(productDetailsRepository.save(any(ProductDetails.class))).willReturn(productDetails);
		doReturn(initUserDTO()).when(userClient).find();
		// WHEN
		ProductDetailsDTO result =
				productDetailsService.save(productDetailsDTO.getId(), productDetailsDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail update.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldFailUpdate() throws ResourcesNotFoundException {

		// GIVEN
		ProductDetails productDetails = new ProductDetails();
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		productDetailsDTO.setId(new Long(1));
		given(productDetailsRepository.findById(any(Long.class))).willReturn(Optional.empty());
		given(productDetailsRepository.save(any(ProductDetails.class))).willReturn(productDetails);
		doReturn(initUserDTO()).when(userClient).find();
		// WHEN
		try {
			productDetailsService.save(productDetailsDTO.getId(), productDetailsDTO);
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("ProductDetails with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success delete all.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessDeleteAll() {

		// GIVEN
		ProductDetailsDTO productDetailsDTO = new ProductDetailsDTO();
		productDetailsDTO.setId(new Long(1));
		doNothing().when(productDetailsRepository).delete(any(ProductDetails.class));

		// WHEN
		productDetailsService.deleteAll(productDetailsDTO);
		// THEN
		verify(productDetailsRepository, times(1)).delete(any(ProductDetails.class));
	}

}
