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

import com.acm.client.ReportingClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ProductQueryExepction;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ProductRepository;
import com.acm.service.impl.ProductServiceImpl;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ProductDetailsDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.models.Product;

/**
 * {@link ProductServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class ProductServiceTest {

	/** The product service. */
	@InjectMocks
	private ProductServiceImpl productService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The product repository. */
	@Mock
	private ProductRepository productRepository;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The setting level service. */
	@Mock
	private SettingLevelService settingLevelService;

	/** The setting document type service. */
	@Mock
	private SettingDocumentTypeService settingDocumentTypeService;

	/** The mail sender client. */
	@Mock
	private ReportingClient mailSenderClient;

	/** The setting level process service. */
	@Mock
	private SettingLevelProcessService settingLevelProcessService;

	/** The setting document product service. */
	@Mock
	private SettingDocumentProductService settingDocumentProductService;

	/** The transvers client. */
	@Mock
	private TransversClient transversClient;

	/** The product details service. */
	@Mock
	private ProductDetailsService productDetailsService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the product DTO.
	 * 
	 * @author ManelLamloum
	 * @return the product DTO
	 */
	private ProductDTO initProductDTO() {

		ProductDTO productDTO = new ProductDTO();
		productDTO.setId(new Long(1));
		productDTO.setCode("code-test");
		productDTO.setCustomerType(CustomerType.INDIV.name());
		productDTO.setProductGrp(Boolean.FALSE);
		productDTO.setProductIndiv(Boolean.FALSE);
		productDTO.setProductOrg(Boolean.TRUE);
		return productDTO;
	}

	/**
	 * Inits the product.
	 * 
	 * @author ManelLamloum
	 * @return the product
	 */
	private Product initProduct() {

		Product product = new Product();
		product.setId(new Long(1));
		product.setCode("code-test");
		return product;
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
	 * Inits the setting document type DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting document type DTO
	 */
	private SettingDocumentTypeDTO initSettingDocumentTypeDTO() {

		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		settingDocumentTypeDTO.setId(new Long(1));
		settingDocumentTypeDTO.setCode("code-test");
		return settingDocumentTypeDTO;
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
		Product product = new Product();
		ProductDTO productDTO = new ProductDTO();
		given(productRepository.findById(any(Long.class))).willReturn(Optional.of(new Product()));
		given(mapper.map(product, ProductDTO.class)).willReturn(productDTO);

		// WHEN
		ProductDTO result = productService.find(new Long(1));
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
		given(productRepository.findById(any(Long.class))).willReturn(Optional.empty());
		// WHEN
		try {
			productService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("Product with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should return list product DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListProductDTO() {

		// GIVEN
		ProductDTO productDTO = initProductDTO();
		Product product = new Product();
		given(productRepository.findAll()).willReturn(Collections.singletonList(product));
		given(mapper.map(product, ProductDTO.class)).willReturn(productDTO);

		// WHEN
		List<ProductDTO> result = productService.find(productDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success create product DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessCreateProductDTO() {

		// GIVEN
		Product product = new Product();
		ProductDTO productDTO = new ProductDTO();
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(productDTO).when(mapper).map(product, ProductDTO.class);
		doReturn(product).when(mapper).map(productDTO, Product.class);
		given(productRepository.save(any(Product.class))).willReturn(product);

		// WHEN
		ProductDTO result = productService.save(productDTO);

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
		Product product = new Product();
		ProductDTO productDTO = initProductDTO();
		given(productRepository.save(any(Product.class))).willReturn(product);
		given(productRepository.findById(any(Long.class))).willReturn(Optional.of(product));
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(productDTO).when(mapper).map(product, ProductDTO.class);

		// WHEN
		ProductDTO result = productService.save(productDTO.getId(), productDTO);
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
		ProductDTO productDTO = new ProductDTO();
		given(productRepository.findById(any(Long.class))).willReturn(Optional.empty());
		doReturn(initUserDTO()).when(userClient).find();
		// WHEN
		try {
			productService.save(new Long(1), productDTO);
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("Product with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success process by batch.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessProcessByBatch() throws ResourcesNotFoundException {

		// GIVEN
		Product product = new Product();
		ProductDTO productDTO = initProductDTO();

		given(productRepository.findByCode(any(String.class)))
				.willReturn(Collections.singletonList(product));
		given(settingLevelService.find(any(SettingLevelDTO.class)))
				.willReturn(Collections.singletonList(new SettingLevelDTO()));
		given(settingDocumentTypeService.find(any(SettingDocumentTypeDTO.class)))
				.willReturn(Collections.singletonList(new SettingDocumentTypeDTO()));
		// Given of save(productDTO) method :
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(productDTO).when(mapper).map(product, ProductDTO.class);
		doReturn(product).when(mapper).map(productDTO, Product.class);
		given(productRepository.save(any(Product.class))).willReturn(product);
		// Given of save(Long,ProductDTO)
		given(productRepository.findById(any(Long.class))).willReturn(Optional.of(product));
		// Given of sendMail() method :
		doNothing().when(mailSenderClient).sendMail(any(MailDTO.class), any(String.class));
		// WHEN
		ProductDTO result = productService.processByBatch(productDTO, "test");
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success with empty list process by batch.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessWithEmptyListProcessByBatch() throws ResourcesNotFoundException {

		// GIVEN
		Product product = initProduct();
		ProductDTO productDTO = initProductDTO();
		SettingDocumentTypeDTO settingDocumentTypeDTO = initSettingDocumentTypeDTO();
		given(productRepository.findByCode(any(String.class))).willReturn(new ArrayList<>());
		given(settingLevelService.find(any(SettingLevelDTO.class)))
				.willReturn(Collections.singletonList(new SettingLevelDTO()));
		given(settingLevelProcessService.save(any(SettingLevelProcessDTO.class)))
				.willReturn(new SettingLevelProcessDTO());
		given(settingDocumentTypeService.find(any(SettingDocumentTypeDTO.class)))
				.willReturn(Collections.singletonList(settingDocumentTypeDTO));

		given(settingDocumentProductService.save(any(SettingDocumentProductDTO.class)))
				.willReturn(new SettingDocumentProductDTO());
		// Given of save(productDTO) method :
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(productDTO).when(mapper).map(product, ProductDTO.class);
		doReturn(product).when(mapper).map(productDTO, Product.class);
		given(productRepository.save(any(Product.class))).willReturn(product);
		// Given of save(Long,ProductDTO)
		given(productRepository.findById(any(Long.class))).willReturn(Optional.of(product));
		// Given of sendMail() method :
		doNothing().when(mailSenderClient).sendMail(any(MailDTO.class), any(String.class));
		// WHEN
		ProductDTO result = productService.processByBatch(productDTO, "test");
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success return list product DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessReturnListProductDTO() {

		// GIVEN
		ProductDTO productDTO = new ProductDTO();
		Product product = new Product();
		given(productRepository.findAll()).willReturn(Collections.singletonList(new Product()));
		doReturn(productDTO).when(mapper).map(product, ProductDTO.class);

		// WHEN
		List<ProductDTO> result = productService.find();
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success load setting from abacus.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ProductQueryExepction 
	 */
	@Test
	String shouldSuccessLoadSettingFromAbacus() throws ResourcesNotFoundException, ProductQueryExepction {

		// GIVEN
		Product product = new Product();
		ProductDTO productDTO = initProductDTO();
		given(transversClient.findProducts())
				.willReturn(Collections.singletonList(new ProductDTO()));
		doReturn(Collections.singletonList(new ProductDetailsDTO())).when(productDetailsService)
				.find(any(ProductDetailsDTO.class));
		doReturn(new ProductDetailsDTO()).when(productDetailsService)
				.save(any(ProductDetailsDTO.class));

		// Given of processBatch() method :
		given(productRepository.findByCode(any(String.class)))
				.willReturn(Collections.singletonList(product));
		given(settingLevelService.find(any(SettingLevelDTO.class)))
				.willReturn(Collections.singletonList(new SettingLevelDTO()));
		given(settingDocumentTypeService.find(any(SettingDocumentTypeDTO.class)))
				.willReturn(Collections.singletonList(new SettingDocumentTypeDTO()));
		doReturn(initUserDTO()).when(userClient).find();
		doReturn(productDTO).when(mapper).map(product, ProductDTO.class);
		doReturn(product).when(mapper).map(productDTO, Product.class);
		given(productRepository.save(any(Product.class))).willReturn(product);
		given(productRepository.findById(any(Long.class))).willReturn(Optional.of(product));
		doNothing().when(mailSenderClient).sendMail(any(MailDTO.class), any(String.class));
		// Given of find() method
		given(productRepository.findAll()).willReturn(Collections.singletonList(product));
		// Given of save() method
		given(productRepository.save(any(Product.class))).willReturn(product);
		// WHEN
		return productService.loadSettingFromAbacus();
	}
}
