/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ProductQueryExepction;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ProductDTO;

/**
 * {@link ProductService} interface.
 * 
 * @author HaythemBenizid
 * @since 1.0.2
 */
public interface ProductService {

	/**
	 * Find {@link ProductDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the ProductService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find by ids.
	 * 
	 * @author idridi
	 * @param ids the ids
	 * @return the list
	 */
	List<ProductDTO> findByIds(List<Long> ids);

	/**
	 * Find {@link List} of {@link ProductDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @return the list
	 */
	List<ProductDTO> find(ProductDTO productDTO);

	/**
	 * Find {@link List} of {@link ProductDTO} by given params.
	 *
	 * @author HamedChaouachi
	 * @param productDTO the product DTO
	 * @return the list
	 */
	List<ProductDTO> findAll(ProductDTO productDTO);

	/**
	 * The method used for saving the given {@link ProductDTO}.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @return the ProductService DTO
	 */
	ProductDTO save(ProductDTO productDTO);

	/**
	 * The method used the given {@link ProductDTO} for processing data By Batch.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @param token the token
	 * @return the ProductService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductDTO processByBatch(ProductDTO productDTO, String token)
			throws ResourcesNotFoundException;

	/**
	 * The method used for updating the given {@link ProductDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param productDTO the product DTO
	 * @return the ProductService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ProductDTO save(Long id, ProductDTO productDTO) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link ProductDTO}.
	 *
	 * @author YesserSomai
	 * @return the list
	 */
	List<ProductDTO> find();

	/**
	 * Load/Refresh setting from ABACUS.
	 *
	 * @author HaythemBenizid
	 * @return the string
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ProductQueryExepction the product query exepction
	 */
	String loadSettingFromAbacus() throws ResourcesNotFoundException, ProductQueryExepction;
}
