/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ProductDetailsDTO;

/**
 * {@link ProductDetailsService} interface.
 *
 * @author MoezMhiri
 * @since 1.0.9
 */
public interface ProductDetailsService {

	/**
	 * Find {@link ProductDetailsDTO} by given ID.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the ProductService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductDetailsDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link ProductDetailsDTO} by given params.
	 *
	 * @author MoezMhiri
	 * @param productDetailsDTO the product details DTO
	 * @return the list
	 */
	List<ProductDetailsDTO> find(ProductDetailsDTO productDetailsDTO);

	/**
	 * The method used for saving the given {@link ProductDTO}.
	 *
	 * @author MoezMhiri
	 * @param productDetailsDTO the product details DTO
	 * @return the ProductDetails DTO
	 */
	ProductDetailsDTO save(ProductDetailsDTO productDetailsDTO);

	/**
	 * The method used for updating the given {@link ProductDTO} by ID.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @param productDetailsDTO the product Details DTO
	 * @return the Product Details DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductDetailsDTO save(Long id, ProductDetailsDTO productDetailsDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete {@link productDetailsDTO} by given params.
	 * 
	 * @author MoezMhiri
	 * @param productDetailsDTO the productDetails DTO
	 */
	void deleteAll(ProductDetailsDTO productDetailsDTO);

}
