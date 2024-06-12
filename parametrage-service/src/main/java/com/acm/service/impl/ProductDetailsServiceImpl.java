/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ProductDetailsRepository;
import com.acm.service.ProductDetailsService;
import com.acm.utils.dtos.ProductDetailsDTO;
import com.acm.utils.models.Product;
import com.acm.utils.models.ProductDetails;
import com.acm.utils.models.QProductDetails;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ProductDetailsServiceImpl} ProductServiceImpl.
 *
 * @author MoezMhiri
 * @since 1.0.9
 */
@Service
public class ProductDetailsServiceImpl implements ProductDetailsService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ProductDetailsServiceImpl.class);

	/** The product details repository. */
	@Autowired
	private ProductDetailsRepository productDetailsRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductDetailsService#find(java.lang.Long)
	 */
	@Override
	public ProductDetailsDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find ProductDetails by ID : {}", id);
		ProductDetails productDetails = productDetailsRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(productDetails)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ProductDetails.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ ProductDetails.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(productDetails, ProductDetailsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductDetailsService#find(com.acm.utils.dtos.ProductDetailsDTO)
	 */
	@Override
	public List<ProductDetailsDTO> find(ProductDetailsDTO productDetailsDTO) {

		Preconditions.checkNotNull(productDetailsDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QProduct
		QProductDetails qProductDetails = QProductDetails.productDetails;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qProductDetails.enabled.eq(Boolean.TRUE));

		// find by product ID
		if (!ACMValidationUtils.isNullOrEmpty(productDetailsDTO.getIdProduct())) {
			predicate
					.and(qProductDetails.product.eq(new Product(productDetailsDTO.getIdProduct())));
		}

		Iterable<ProductDetails> iterable = productDetailsRepository.findAll(predicate);
		List<ProductDetails> productDetails = new ArrayList<>();
		iterable.forEach(productDetails::add);
		logger.info("{} : ProductDetails was founded", productDetails.size());

		List<ProductDetailsDTO> productDetailsDTOs = new ArrayList<>();
		productDetails.forEach(productDetail -> productDetailsDTOs
				.add(mapper.map(productDetail, ProductDetailsDTO.class)));
		return productDetailsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductDetailsService#save(com.acm.utils.dtos.ProductDetailsDTO)
	 */
	@Override
	public ProductDetailsDTO save(ProductDetailsDTO productDetailsDTO) {

		Preconditions.checkNotNull(productDetailsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ProductDetails productDetails = mapper.map(productDetailsDTO, ProductDetails.class);

		CommonFunctions.mapperToSave(productDetails, userClient, logger);

		ProductDetails newProductDetails = productDetailsRepository.save(productDetails);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ProductDetails.class.getSimpleName());
		return mapper.map(newProductDetails, ProductDetailsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductDetailsService#save(java.lang.Long,
	 * com.acm.utils.dtos.ProductDetailsDTO)
	 */
	@Override
	public ProductDetailsDTO save(Long id, ProductDetailsDTO productDetailsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(productDetailsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Product with ID = {}", id);
		ProductDetails oldProductDetails = productDetailsRepository.findById(id).orElse(null);

		// check if object is null
		if (oldProductDetails == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ProductDetails.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ProductDetails.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldProductDetails)
		mapper.map(productDetailsDTO, oldProductDetails);
		CommonFunctions.mapperToUpdate(oldProductDetails, userClient, logger);

		// update & persist data in DB
		ProductDetails newProductDetails = productDetailsRepository.save(oldProductDetails);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ProductDetails.class.getSimpleName());
		return mapper.map(newProductDetails, ProductDetailsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductDetailsService#deleteById(com.acm.utils. dtos. ProductDetailsDTO)
	 */
	@Override
	public void deleteAll(ProductDetailsDTO productDetailsDTO) {

		Preconditions.checkNotNull(productDetailsDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete by Id : {}", productDetailsDTO.getId());
		productDetailsRepository.delete(new ProductDetails(productDetailsDTO.getId()));
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, ProductDetailsDTO.class.getSimpleName());
	}
}
