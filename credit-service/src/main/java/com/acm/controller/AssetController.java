/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AssetService;
import com.acm.utils.dtos.AssetDTO;
import com.acm.utils.dtos.pagination.AssetPaginationDTO;

/**
 * {@link AssetController} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/asset")
public class AssetController {

	/** The asset service. */
	@Autowired
	private AssetService assetService;

	/**
	 * Find.
	 *
	 * @author mlamloum
	 * @param assetDTO the asset DTO
	 * @return the list
	 */
	@GetMapping("/")
	public List<AssetDTO> find(AssetDTO assetDTO) {

		return assetService.find(assetDTO);
	}

	/**
	 * Creates the.
	 * 
	 * @author mlamloum
	 * @param assetDTO the asset DTO
	 * @return the asset DTO
	 */
	@PostMapping("/create")
	public AssetDTO create(@RequestBody AssetDTO assetDTO) {

		return assetService.save(assetDTO);
	}

	/**
	 * Creates the all.
	 *
	 * @author mlamloum
	 * @param assetDTOs the asset DT os
	 * @return the list
	 */
	@PostMapping("/create-all")
	public List<AssetDTO> createAll(@RequestBody List<AssetDTO> assetDTOs) {

		return assetService.saveAll(assetDTOs);
	}

	/**
	 * Update.
	 * 
	 * @author mlamloum
	 * @param assetDTO the asset DTO
	 * @return the asset DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AssetDTO update(@RequestBody AssetDTO assetDTO) throws ResourcesNotFoundException {

		return assetService.save(assetDTO.getId(), assetDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author mlamloum
	 * @param assetPaginationDTO the asset pagination DTO
	 * @return the asset pagination DTO
	 */
	@PostMapping("/find-pagination")
	public AssetPaginationDTO findPagination(@RequestBody AssetPaginationDTO assetPaginationDTO) {

		return assetService.find(assetPaginationDTO);
	}
}
