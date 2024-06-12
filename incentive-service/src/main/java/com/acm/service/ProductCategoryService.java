/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ProductCategoryDTO;

/**
 * {@link ProductCategoryService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public interface ProductCategoryService {

	/**
	 * Find by ID.
	 *
	 * @author HaythemBenizid
	 * @param idProductCategory the id setting level
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductCategoryDTO find(Long idProductCategory) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param productCategoryDTO the incentive setting constant DTO
	 * @return the list
	 */
	List<ProductCategoryDTO> find(ProductCategoryDTO productCategoryDTO);

	/**
	 * The method used for saving the given {@link ProductCategoryDTO}.
	 *
	 * @author HaythemBenizid
	 * @param productCategoryDTO the productCategory DTO
	 * @return the ProductCategoryService DTO
	 */
	ProductCategoryDTO save(ProductCategoryDTO productCategoryDTO);

	/**
	 * The method used for updating the given {@link ProductCategoryDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param productCategoryDTO the productCategory DTO
	 * @return the ProductCategoryService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductCategoryDTO save(Long id, ProductCategoryDTO productCategoryDTO)
			throws ResourcesNotFoundException;
}
