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
import com.acm.service.IncentiveSettingConstantService;
import com.acm.utils.dtos.IncentiveSettingConstantDTO;

/**
 * This class @{link IncentiveSettingConstantController} used to control all the
 * IncentiveSettingConstant requests.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-setting-constant")
public class IncentiveSettingConstantController {

	/** The incentiveSettingConstant service. */
	@Autowired
	private IncentiveSettingConstantService incentiveSettingConstantService;

	/**
	 * Find {@link List} of {@link IncentiveSettingConstantDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingConstantDTO the incentive setting constant DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveSettingConstantDTO> find(
			@RequestBody IncentiveSettingConstantDTO incentiveSettingConstantDTO) {

		return incentiveSettingConstantService.find(incentiveSettingConstantDTO);
	}

	/**
	 * Creates the new IncentiveSettingConstant DATA.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingConstantDTO the incentive setting constant DTO
	 * @return the incentive setting based on DTO
	 */
	@PostMapping("/create")
	public IncentiveSettingConstantDTO create(
			@RequestBody IncentiveSettingConstantDTO incentiveSettingConstantDTO) {

		return incentiveSettingConstantService.save(incentiveSettingConstantDTO);
	}

	/**
	 * Update the given {@link IncentiveSettingConstantDTO} by id.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingConstantDTO the incentive setting constant DTO
	 * @return the incentive setting based on DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public IncentiveSettingConstantDTO update(
			@RequestBody IncentiveSettingConstantDTO incentiveSettingConstantDTO)
			throws ResourcesNotFoundException {

		return incentiveSettingConstantService.save(incentiveSettingConstantDTO.getId(),
				incentiveSettingConstantDTO);
	}

	/**
	 * Find by categories.
	 * 
	 * @author idridi
	 * @param categories the categories
	 * @return the list
	 */
	@PostMapping("/find-by-categories")
	public List<IncentiveSettingConstantDTO> findByCategories(
			@RequestBody List<String> categories) {

		return incentiveSettingConstantService.findByCategories(categories);
	}
}
