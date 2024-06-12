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
import com.acm.service.AcmIhmFormService;
import com.acm.utils.dtos.AcmIhmFormDTO;

/**
 * {@link AcmIhmFormController} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@RestController
@RequestMapping("/acm-ihm-form")
public class AcmIhmFormController {

	/** The acm ihm field service. */
	@Autowired
	private AcmIhmFormService acmIhmFormService;

	/**
	 * Find by ihm route.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmIhmFormDTO> find(@RequestBody AcmIhmFormDTO acmIhmFormDTO) {

		return acmIhmFormService.find(acmIhmFormDTO);
	}

	/**
	 * Create ACM IHM form.
	 * 
	 * @author YesserSomai
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @return acmIhmFormDTO the acm ihm form DTO
	 */
	@PostMapping("/create")
	public AcmIhmFormDTO create(@RequestBody AcmIhmFormDTO acmIhmFormDTO) {

		return acmIhmFormService.save(acmIhmFormDTO);

	}

	/**
	 * Update ACM IHM form.
	 *
	 * @author YesserSomai
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @return acmIhmFormDTO the acm ihm form DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmIhmFormDTO update(@RequestBody AcmIhmFormDTO acmIhmFormDTO)
			throws ResourcesNotFoundException {

		return acmIhmFormService.save(acmIhmFormDTO, acmIhmFormDTO.getId());

	}
}
