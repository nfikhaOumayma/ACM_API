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
import com.acm.service.IncentiveSettingService;
import com.acm.utils.dtos.IncentiveSettingDTO;

/**
 * This class @{link IncentiveSettingController} used to control all the IncentiveSetting requests.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-setting")
public class IncentiveSettingController {

	/** The incentiveSetting service. */
	@Autowired
	private IncentiveSettingService incentiveSettingService;

	/**
	 * Find {@link List} of {@link IncentiveSettingDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingDTO the incentive setting DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveSettingDTO> find(@RequestBody IncentiveSettingDTO incentiveSettingDTO) {

		return incentiveSettingService.find(incentiveSettingDTO);
	}

	/**
	 * Creates the new IncentiveSetting DATA.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingDTO the incentive setting DTO
	 * @return the incentive setting based on DTO
	 */
	@PostMapping("/create")
	public IncentiveSettingDTO create(@RequestBody IncentiveSettingDTO incentiveSettingDTO) {

		return incentiveSettingService.save(incentiveSettingDTO);
	}

	/**
	 * Update the given {@link IncentiveSettingDTO} by id.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingDTO the incentive setting DTO
	 * @return the incentive setting based on DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public IncentiveSettingDTO update(@RequestBody IncentiveSettingDTO incentiveSettingDTO)
			throws ResourcesNotFoundException {

		return incentiveSettingService.save(incentiveSettingDTO.getId(), incentiveSettingDTO);
	}

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		incentiveSettingService.delete(id);
	}
}
