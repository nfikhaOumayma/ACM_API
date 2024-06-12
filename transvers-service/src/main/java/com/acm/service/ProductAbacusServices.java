/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.ProductDTO;

/**
 * {@link ProductAbacusServices} class.
 *
 * @author YesserSomai
 * @since 0.11.0
 */
public interface ProductAbacusServices {

	/**
	 * Find Product from Abacus By ID.
	 *
	 * @author YesserSomai
	 * @param productId the product id
	 * @return the product DTO
	 */
	ProductDTO find(Long productId);

	/**
	 * Find all.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<ProductDTO> find();

}
