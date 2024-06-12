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
import com.acm.service.SettingGurantorCollateralService;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;

/**
 * This class @{link SettingGurantorCollateralController} used to control all the
 * SettingGurantorCollateral requests.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
@RestController
@RequestMapping("/setting-gurantor-collateral")
public class SettingGurantorCollateralController {

	/** The SettingGurantorCollateral service. */
	@Autowired
	private SettingGurantorCollateralService settingGurantorCollateralService;

	/**
	 * Find list SettingGurantorCollateral by given params.
	 *
	 * @param settingGurantorCollateralDTO the setting gurantor collateral DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingGurantorCollateralDTO> find(
			@RequestBody SettingGurantorCollateralDTO settingGurantorCollateralDTO) {

		return settingGurantorCollateralService.find(settingGurantorCollateralDTO);
	}

	/**
	 * Create the SettingGurantorCollateral.
	 *
	 * @author HaythemBenizid
	 * @param settingGurantorCollateralDTO the settingGurantorCollateral DTO
	 * @return the SettingGurantorCollateral DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingGurantorCollateralDTO create(
			@RequestBody SettingGurantorCollateralDTO settingGurantorCollateralDTO)
			throws ResourcesNotFoundException {

		return settingGurantorCollateralService.save(settingGurantorCollateralDTO);
	}

	/**
	 * Update the SettingGurantorCollateral by id.
	 *
	 * @author HaythemBenizid
	 * @param settingGurantorCollateralDTO the settingGurantorCollateral DTO
	 * @return the settingGurantorCollateral DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingGurantorCollateralDTO update(
			@RequestBody SettingGurantorCollateralDTO settingGurantorCollateralDTO)
			throws ResourcesNotFoundException {

		return settingGurantorCollateralService.save(settingGurantorCollateralDTO.getId(),
				settingGurantorCollateralDTO);
	}
}
