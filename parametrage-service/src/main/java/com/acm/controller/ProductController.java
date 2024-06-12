/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.boot.configurationprocessor.json.JSONObject;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ProductQueryExepction;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ProductService;
import com.acm.utils.dtos.ProductDTO;

/**
 * This class @{link ProductController} used to control all the Product requests.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@RestController
@RequestMapping("/products")
public class ProductController {

	/** The Product service. */
	@Autowired
	private ProductService productService;

	/**
	 * Find Product by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the product DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public ProductDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return productService.find(id);
	}

	/**
	 * Find by ids.
	 *
	 * @author idridi
	 * @param ids the ids
	 * @return the list
	 */
	@PostMapping("/find-by-ids")
	public List<ProductDTO> findByIds(@RequestBody List<Long> ids) {

		return productService.findByIds(ids);
	}

	/**
	 * Find {@link List} of {@link ProductDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ProductDTO> find(@RequestBody ProductDTO productDTO) {

		return productService.find(productDTO);
	}

	/**
	 * Create the Product.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @return the Product DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public ProductDTO create(@RequestBody ProductDTO productDTO) throws ResourcesNotFoundException {

		return productService.save(productDTO);
	}

	/**
	 * Update the Product by id.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @return the product DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ProductDTO update(@RequestBody ProductDTO productDTO) throws ResourcesNotFoundException {

		return productService.save(productDTO.getId(), productDTO);
	}

	/**
	 * Find {@link List} of {@link ProductDTO} list.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<ProductDTO> findProducts() {

		return productService.find();
	}

	/**
	 * Load/Refresh setting from ABACUS.
	 *
	 * @author HaythemBenizid
	 * @return the response entity OK
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ProductQueryExepction the product query exepction
	 */
	@GetMapping("/reload-setting")
	public String resetSettingFromAbacus()
			throws ResourcesNotFoundException, ProductQueryExepction {

		return JSONObject.quote(productService.loadSettingFromAbacus());

	}
}
