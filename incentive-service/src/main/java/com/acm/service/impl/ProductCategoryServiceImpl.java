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
import com.acm.repository.ProductCategoryRepository;
import com.acm.service.ProductCategoryService;
import com.acm.utils.dtos.ProductCategoryDTO;
import com.acm.utils.models.ProductCategory;
import com.acm.utils.models.QProductCategory;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ProductCategoryServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Service
public class ProductCategoryServiceImpl implements ProductCategoryService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ProductCategoryServiceImpl.class);

	/** The incentive setting constant repository. */
	@Autowired
	private ProductCategoryRepository productCategoryRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductCategoryServiceImp#find(java.lang.Long)
	 */
	@Override
	public ProductCategoryDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find productCategory by ID : {}", id);
		ProductCategory productCategory = productCategoryRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(productCategory)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ProductCategory.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ ProductCategory.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(productCategory, ProductCategoryDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductCategoryServiceImp#find(com.acm.utils.dtos. ProductCategoryDTO)
	 */
	@Override
	public List<ProductCategoryDTO> find(ProductCategoryDTO productCategoryDto) {

		Preconditions.checkNotNull(productCategoryDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QProductCategory
		QProductCategory qProductCategory = QProductCategory.productCategory;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qProductCategory.enabled.eq(Boolean.TRUE));

		Iterable<ProductCategory> iterable = productCategoryRepository.findAll(predicate);
		List<ProductCategory> productCategorys = new ArrayList<>();
		iterable.forEach(productCategorys::add);
		// mapping data
		List<ProductCategoryDTO> productCategoryDTOs = new ArrayList<>();
		productCategorys.forEach(productCategory -> productCategoryDTOs
				.add(mapper.map(productCategory, ProductCategoryDTO.class)));
		logger.info("{} : productCategory was founded", productCategorys.size());
		return productCategoryDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductCategoryServiceImp#save(com.acm.utils.dtos. ProductCategoryDTO)
	 */
	@Override
	public ProductCategoryDTO save(ProductCategoryDTO productCategoryDto) {

		Preconditions.checkNotNull(productCategoryDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		ProductCategory productCategory = mapper.map(productCategoryDto, ProductCategory.class);
		CommonFunctions.mapperToSave(productCategory, userClient, logger);

		ProductCategory newProductCategory = productCategoryRepository.save(productCategory);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ProductCategory.class.getSimpleName());
		return mapper.map(newProductCategory, ProductCategoryDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ProductCategoryServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.ProductCategoryDTO)
	 */
	@Override
	public ProductCategoryDTO save(Long id, ProductCategoryDTO productCategoryDto)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(productCategoryDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		logger.info("Update ProductCategory  with ID = {}", id);
		ProductCategory oldProductCategory = productCategoryRepository.findById(id).orElse(null);
		// check if object is null
		if (oldProductCategory == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ProductCategory.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ProductCategory.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldProductCategory)
		mapper.map(productCategoryDto, oldProductCategory);
		CommonFunctions.mapperToUpdate(oldProductCategory, userClient, logger);
		ProductCategory newProductCategory = productCategoryRepository.save(oldProductCategory);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ProductCategory.class.getSimpleName());
		return mapper.map(newProductCategory, ProductCategoryDTO.class);
	}

}
