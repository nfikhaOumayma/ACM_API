/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.ProductAbacusServices;
import com.acm.utils.dtos.ProductDTO;

/**
 * {@link LoadDataProductController} class.
 *
 * @author YesserSomai
 * @since 0.11.0
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataProductController {

	/** The user abacus service. */
	@Autowired
	private ProductAbacusServices productAbacusServices;

	/**
	 * Find Product by ID.
	 *
	 * @author YesserSomai
	 * @param productId the product id
	 * @return the product DTO
	 */
	@GetMapping("/product/{productid}")
	public ProductDTO find(@PathVariable("productid") Long productId) {

		return productAbacusServices.find(productId);
	}

	/**
	 * Find all.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/products/")
	public List<ProductDTO> find() {

		return productAbacusServices.find();
	}
}
