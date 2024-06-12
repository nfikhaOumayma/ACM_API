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
import com.acm.service.SettingDocumentProductService;
import com.acm.utils.dtos.SettingDocumentProductDTO;

/**
 * This class @{link SettingDocumentProductController} used to control all the
 * SettingDocumentProduct requests.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@RestController
@RequestMapping("/setting-document-products")
public class SettingDocumentProductController {

	/** The SettingDocumentProduct service. */
	@Autowired
	private SettingDocumentProductService settingDocumentProductService;

	/**
	 * Find list SettingDocumentProduct by given params.
	 *
	 * @param settingDocumentProductDTO the setting motif rejets DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingDocumentProductDTO> find(
			@RequestBody SettingDocumentProductDTO settingDocumentProductDTO) {

		return settingDocumentProductService.find(settingDocumentProductDTO);
	}

	/**
	 * Create the SettingDocumentProduct.
	 *
	 * @author HaythemBenizid
	 * @param settingDocumentProductDTO the settingDocumentProduct DTO
	 * @return the SettingDocumentProduct DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingDocumentProductDTO create(
			@RequestBody SettingDocumentProductDTO settingDocumentProductDTO)
			throws ResourcesNotFoundException {

		return settingDocumentProductService.save(settingDocumentProductDTO);
	}

	/**
	 * Update the SettingDocumentProduct by id.
	 *
	 * @author HaythemBenizid
	 * @param settingDocumentProductDTO the settingDocumentProduct DTO
	 * @return the settingDocumentProduct DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingDocumentProductDTO update(
			@RequestBody SettingDocumentProductDTO settingDocumentProductDTO)
			throws ResourcesNotFoundException {

		return settingDocumentProductService.save(settingDocumentProductDTO.getId(),
				settingDocumentProductDTO);
	}

	/**
	 * Find all category type.
	 * 
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	@GetMapping("/")
	public List<SettingDocumentProductDTO> find() {

		return settingDocumentProductService.find();
	}

	/**
	 * disable document product from database.
	 *
	 * @author AbdelkarimTurki
	 * @param settingDocumentProductDTO the setting Document Product DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/disable-document-product")
	public void disableDocumentType(
			@RequestBody SettingDocumentProductDTO settingDocumentProductDTO)
			throws ResourcesNotFoundException {

		settingDocumentProductService.updateStatus(settingDocumentProductDTO, Boolean.FALSE);
	}

	/**
	 * enable document product from database.
	 *
	 * @author AbdelkarimTurki
	 * @param settingDocumentProductDTO the setting Document Product DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/enable-document-product")
	public void enableDocumentType(@RequestBody SettingDocumentProductDTO settingDocumentProductDTO)
			throws ResourcesNotFoundException {

		settingDocumentProductService.updateStatus(settingDocumentProductDTO, Boolean.TRUE);
	}
}
