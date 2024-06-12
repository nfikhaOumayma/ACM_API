/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.IncentiveSettingBranchProdLevelService;
import com.acm.utils.dtos.IncentiveSettingBranchProdLevelDTO;

/**
 * This class @{link IncentiveSettingBranchProdLevelController} used to control all the
 * IncentiveSettingBranchProdLevel requests.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-setting-prod-level")
public class IncentiveSettingBranchProdLevelController {

	/** The incentiveSettingBranchProdLevel service. */
	@Autowired
	private IncentiveSettingBranchProdLevelService incentiveSettingBranchProdLevelService;

	/**
	 * Find {@link List} of {@link IncentiveSettingBranchProdLevelDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingBranchProdLevelDTO the incentive setting branch prod level DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveSettingBranchProdLevelDTO> find(
			@RequestBody IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO) {

		return incentiveSettingBranchProdLevelService.find(incentiveSettingBranchProdLevelDTO);
	}

	/**
	 * Creates the new IncentiveSettingBranchProdLevel DATA.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingBranchProdLevelDTO the incentive setting branch prod level DTO
	 * @return the incentive setting based on DTO
	 */
	@PostMapping("/create")
	public IncentiveSettingBranchProdLevelDTO create(
			@RequestBody IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO) {

		return incentiveSettingBranchProdLevelService.save(incentiveSettingBranchProdLevelDTO);
	}

	/**
	 * Update the given {@link IncentiveSettingBranchProdLevelDTO} by id.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingBranchProdLevelDTO the incentive setting branch prod level DTO
	 * @return the incentive setting based on DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public IncentiveSettingBranchProdLevelDTO update(
			@RequestBody IncentiveSettingBranchProdLevelDTO incentiveSettingBranchProdLevelDTO)
			throws ResourcesNotFoundException {

		return incentiveSettingBranchProdLevelService.save(
				incentiveSettingBranchProdLevelDTO.getId(), incentiveSettingBranchProdLevelDTO);
	}

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		incentiveSettingBranchProdLevelService.delete(id);
	}
}
