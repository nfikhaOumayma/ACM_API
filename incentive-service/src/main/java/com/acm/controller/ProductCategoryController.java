/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ProductCategoryService;
import com.acm.utils.dtos.ProductCategoryDTO;

/**
 * This class @{link ProductCategoryController} used to control all the ProductCategory requests.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/product-category")
public class ProductCategoryController {

	/** The productCategory service. */
	@Autowired
	private ProductCategoryService productCategoryService;

	/**
	 * Find {@link List} of {@link ProductCategoryDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param productCategoryDTO the incentive setting constant DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ProductCategoryDTO> find(@RequestBody ProductCategoryDTO productCategoryDTO) {

		return productCategoryService.find(productCategoryDTO);
	}

	/**
	 * Creates the new ProductCategory DATA.
	 *
	 * @author HaythemBenizid
	 * @param productCategoryDTO the incentive setting constant DTO
	 * @return the incentive setting based on DTO
	 */
	@PostMapping("/create")
	public ProductCategoryDTO create(@RequestBody ProductCategoryDTO productCategoryDTO) {

		return productCategoryService.save(productCategoryDTO);
	}

	/**
	 * Update the given {@link ProductCategoryDTO} by id.
	 *
	 * @author HaythemBenizid
	 * @param productCategoryDTO the incentive setting constant DTO
	 * @return the incentive setting based on DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ProductCategoryDTO update(@RequestBody ProductCategoryDTO productCategoryDTO)
			throws ResourcesNotFoundException {

		return productCategoryService.save(productCategoryDTO.getId(), productCategoryDTO);
	}
}
