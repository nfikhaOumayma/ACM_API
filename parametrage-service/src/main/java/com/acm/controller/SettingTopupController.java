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

import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingTopupService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.SettingTopupDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;

/**
 * {@link SettingTopupController} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/setting-topup")
public class SettingTopupController {

	/** The setting topup service. */
	@Autowired
	private SettingTopupService settingTopupService;

	/**
	 * Find.
	 * 
	 * @author mlamloum
	 * @param settingTopupDTO the setting topup DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingTopupDTO> find(@RequestBody SettingTopupDTO settingTopupDTO) {

		return settingTopupService.find(settingTopupDTO);
	}

	/**
	 * Creates the.
	 *
	 * @author mlamloum
	 * @param settingTopupDTO the setting topup DTO
	 * @return the setting topup DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public SettingTopupDTO create(@RequestBody SettingTopupDTO settingTopupDTO)
			throws ResourcesNotFoundException {

		return settingTopupService.save(settingTopupDTO);
	}

	/**
	 * Update.
	 * 
	 * @author mlamloum
	 * @param settingTopupDTO the setting topup DTO
	 * @return the setting topup DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingTopupDTO update(@RequestBody SettingTopupDTO settingTopupDTO)
			throws ResourcesNotFoundException {

		return settingTopupService.save(settingTopupDTO.getId(), settingTopupDTO);
	}

	/**
	 * Check validity.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the setting topup DTO
	 * @throws ParametrageException the parametrage exception
	 */
	@PostMapping("/check-validity")
	public SettingTopupValidityDTO checkValidity(@RequestBody LoanDTO loanDTO)
			throws ParametrageException {

		return settingTopupService.checkValidity(loanDTO);
	}

}
