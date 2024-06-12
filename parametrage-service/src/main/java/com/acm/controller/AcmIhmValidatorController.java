/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.AcmIhmValidatorService;
import com.acm.utils.dtos.AcmIhmValidatorDTO;

/**
 * {@link AcmIhmValidatorController} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@RestController
@RequestMapping("/acm-ihm-validator")
public class AcmIhmValidatorController {

	/** The acm ihm validator service. */
	@Autowired
	private AcmIhmValidatorService acmIhmValidatorService;

	/**
	 * Find AcmIhmValidatorDTO.
	 *
	 * @param acmIhmValidatorDTO the acm ihm validator DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmIhmValidatorDTO> find(@RequestBody AcmIhmValidatorDTO acmIhmValidatorDTO) {

		return acmIhmValidatorService.find(acmIhmValidatorDTO);
	}

}
