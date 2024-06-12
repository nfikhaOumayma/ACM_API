/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmIhmFieldGroupeService;
import com.acm.utils.dtos.AcmIhmFieldGroupeDTO;
import com.acm.utils.dtos.AddressSettingDTO;

/**
 * This class @{link AcmIhmFieldGroupeController} used to control all the {@link AddressSettingDTO}
 * requests.
 *
 * @author ManelLamloum
 * @since 1.0.5
 */
@RestController
@RequestMapping("/acm-ihm-field-groupe")
public class AcmIhmFieldGroupeController {
	/** The acm ihm field service. */
	@Autowired
	private AcmIhmFieldGroupeService acmIhmFieldGroupeService;

	/**
	 * Update.
	 *
	 * @ManelLamloum
	 * @param acmIhmFieldGroupeDTO the acm ihm field groupe DTO
	 * @return the acm ihm field groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmIhmFieldGroupeDTO update(@RequestBody AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO)
			throws ResourcesNotFoundException {

		return acmIhmFieldGroupeService.updateHabilitation(acmIhmFieldGroupeDTO);
	}
}
