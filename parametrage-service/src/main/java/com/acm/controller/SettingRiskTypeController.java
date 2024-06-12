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
import com.acm.service.SettingRiskTypeService;
import com.acm.utils.dtos.SettingTypeRiskDTO;

/**
 * The Class SettingRiskTypeController.
 */
@RestController
@RequestMapping("/setting-risk-type")
public class SettingRiskTypeController {

	/** The setting topup service. */
	@Autowired
	private SettingRiskTypeService settingRiskTypeService;

	/**
	 * Find.
	 *
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingTypeRiskDTO> find() {

		return settingRiskTypeService.find();
	}

	/**
	 * Creates the.
	 *
	 * @param settingTypeRiskDTO the setting type risk DTO
	 * @return the setting type risk DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingTypeRiskDTO create(@RequestBody SettingTypeRiskDTO settingTypeRiskDTO)
			throws ResourcesNotFoundException {

		return settingRiskTypeService.save(settingTypeRiskDTO);
	}

	/**
	 * Update.
	 *
	 * @param settingTypeRiskDTO the setting type risk DTO
	 * @return the setting type risk DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingTypeRiskDTO update(@RequestBody SettingTypeRiskDTO settingTypeRiskDTO)
			throws ResourcesNotFoundException {

		return settingRiskTypeService.update(settingTypeRiskDTO);
	}

	/**
	 * Delete.
	 *
	 * @param idRiskType the id risk type
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@DeleteMapping("/delete/{idRiskType}")
	public void delete(@PathVariable("idRiskType") Long idRiskType)
			throws ResourcesNotFoundException {

		settingRiskTypeService.delete(idRiskType);
	}

}
