/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingDocumentProductDTO;

/**
 * {@link SettingDocumentProductService} interface.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
public interface SettingDocumentProductService {

	/**
	 * Find {@link List} of {@link SettingDocumentProductDTO} by given params. using Querydsl.
	 *
	 * @author HaythemBenizid
	 * @param settingDocumentProductDTO the setting document DTO
	 * @return the list
	 */
	List<SettingDocumentProductDTO> find(SettingDocumentProductDTO settingDocumentProductDTO);

	/**
	 * Find {@link SettingDocumentProductDTO} by given ID.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 * @return the settingDocumentProduct DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingDocumentProductDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find all document product.
	 *
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	List<SettingDocumentProductDTO> find();

	/**
	 * The method used for saving the given {@link SettingDocumentProductDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingDocumentProductDTO the settingDocumentProduct DTO
	 * @return the SettingDocumentProductService DTO
	 */
	SettingDocumentProductDTO save(SettingDocumentProductDTO settingDocumentProductDTO);

	/**
	 * The method used for updating the given {@link SettingDocumentProductDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingDocumentProductDTO the settingDocumentProduct DTO
	 * @return the SettingDocumentProductService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingDocumentProductDTO save(Long id, SettingDocumentProductDTO settingDocumentProductDTO)
			throws ResourcesNotFoundException;

	/**
	 * Update status document type product.
	 *
	 * @param settingDocumentProductDTO the setting document product DTO
	 * @param status the status
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateStatus(SettingDocumentProductDTO settingDocumentProductDTO, Boolean status)
			throws ResourcesNotFoundException;

}
