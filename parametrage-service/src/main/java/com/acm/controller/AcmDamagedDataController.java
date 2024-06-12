/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.AcmDamageDataService;
import com.acm.utils.dtos.AcmDamagedDataDTO;

/**
 * {@link AcmDamagedDataController} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/acm-damaged-data")
public class AcmDamagedDataController {

	/** The acm damage customer service. */
	@Autowired
	private AcmDamageDataService acmDamageDataService;

	/**
	 * Save.
	 * 
	 * @author mlamloum
	 * @param acmDamagedDataDTO the acm damaged data DTO
	 * @return the acm damaged data DTO
	 */
	@PostMapping("/create")
	public AcmDamagedDataDTO save(@RequestBody AcmDamagedDataDTO acmDamagedDataDTO) {

		return acmDamageDataService.save(acmDamagedDataDTO);
	}
}
