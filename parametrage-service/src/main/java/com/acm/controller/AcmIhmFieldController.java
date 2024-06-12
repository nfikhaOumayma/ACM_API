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
import com.acm.service.AcmIhmFieldService;
import com.acm.utils.dtos.AcmIhmFieldDTO;

/**
 * {@link AcmIhmFieldController } class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@RestController
@RequestMapping("/acm-ihm-field")
public class AcmIhmFieldController {

	/** The acm ihm field service. */
	@Autowired
	private AcmIhmFieldService acmIhmFieldService;

	/**
	 * Find AcmIhmFieldDTO.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFieldDTO the acm ihm field DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/")
	public List<AcmIhmFieldDTO> find(@RequestBody AcmIhmFieldDTO acmIhmFieldDTO)
			throws ResourcesNotFoundException {

		return acmIhmFieldService.find(acmIhmFieldDTO);
	}

	/**
	 * Update AcmIhmFieldDTO.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFieldDTO the acm ihm field DTO
	 * @return the acm ihm field DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public AcmIhmFieldDTO update(@RequestBody AcmIhmFieldDTO acmIhmFieldDTO)
			throws ResourcesNotFoundException {

		return acmIhmFieldService.save(acmIhmFieldDTO.getId(), acmIhmFieldDTO);
	}

	/**
	 * Save all.
	 *
	 * @author YesserSomai
	 * @param acmIhmFieldDTOs the acm ihm field DT os
	 * @return the list
	 */
	@PostMapping("/save-all")
	public List<AcmIhmFieldDTO> saveAll(@RequestBody List<AcmIhmFieldDTO> acmIhmFieldDTOs) {

		return acmIhmFieldService.saveAll(acmIhmFieldDTOs);
	}

}
