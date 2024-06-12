/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ProductDetailsService;
import com.acm.utils.dtos.ProductDetailsDTO;

/**
 * This class @{link ProductDetailsController} used to control all the Product details requests.
 *
 * @author MoezMhiri
 * @since 1.0.9
 */
@RestController
@RequestMapping("/product-details")
public class ProductDetailsController {

	/** The product details service. */
	@Autowired
	private ProductDetailsService productDetailsService;

	/**
	 * Find ProductDetails by id.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the productDetails DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public ProductDetailsDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return productDetailsService.find(id);
	}

	/**
	 * Find {@link List} of {@link ProductDetailsDTO} by Requested params.
	 *
	 * @author MoezMhiri
	 * @param productDetailsDTO the productDetails DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ProductDetailsDTO> find(@RequestBody ProductDetailsDTO productDetailsDTO) {

		return productDetailsService.find(productDetailsDTO);
	}

	/**
	 * Create the ProductDetails.
	 *
	 * @author MoezMhiri
	 * @param productDetailsDTO the productDetails DTO
	 * @return the ProductDetails DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public ProductDetailsDTO create(@RequestBody ProductDetailsDTO productDetailsDTO)
			throws ResourcesNotFoundException {

		return productDetailsService.save(productDetailsDTO);
	}

	/**
	 * Update the Product by id.
	 *
	 * @author MoezMhiri
	 * @param productDetailsDTO the product details DTO
	 * @return the product details DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ProductDetailsDTO update(@RequestBody ProductDetailsDTO productDetailsDTO)
			throws ResourcesNotFoundException {

		return productDetailsService.save(productDetailsDTO.getId(), productDetailsDTO);
	}

	/**
	 * Delete.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 */
	@DeleteMapping("/{id}")
	public void delete(@PathVariable("id") Long id) {

		productDetailsService.deleteAll(new ProductDetailsDTO(id));
	}
}
