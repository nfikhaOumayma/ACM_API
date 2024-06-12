/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AssetDTO;
import com.acm.utils.dtos.pagination.AssetPaginationDTO;

/**
 * The Interface AssetService.
 */
public interface AssetService {

	/**
	 * Find.
	 *
	 * @param assetDTO the asset DTO
	 * @return the list
	 */
	List<AssetDTO> find(AssetDTO assetDTO);

	/**
	 * Save.
	 *
	 * @param assetDTO the asset DTO
	 * @return the asset DTO
	 */
	AssetDTO save(AssetDTO assetDTO);

	/**
	 * Save all.
	 *
	 * @param assetDTOs the asset DT os
	 * @return the list
	 */
	List<AssetDTO> saveAll(List<AssetDTO> assetDTOs);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param assetDTO the asset DTO
	 * @return the asset DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AssetDTO save(Long id, AssetDTO assetDTO) throws ResourcesNotFoundException;

	/**
	 * Find.
	 *
	 * @param assetDTO the asset DTO
	 * @return the asset pagination DTO
	 */
	AssetPaginationDTO find(AssetPaginationDTO assetDTO);
}
