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

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.IncentiveSettingRunService;
import com.acm.utils.dtos.IncentiveSettingRunDTO;

/**
 * This class @{link IncentiveSettingRunController} used to control all the IncentiveSettingRun
 * requests.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-setting-run")
public class IncentiveSettingRunController {

	/** The incentiveSettingRun service. */
	@Autowired
	private IncentiveSettingRunService incentiveSettingRunService;

	/**
	 * Find {@link List} of {@link IncentiveSettingRunDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param incentiveSettingRunDTO the incentive setting run DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveSettingRunDTO> find(
			@RequestBody IncentiveSettingRunDTO incentiveSettingRunDTO) {

		return incentiveSettingRunService.find(incentiveSettingRunDTO);
	}

	/**
	 * Find by code.
	 * 
	 * @author idridi
	 * @param incentiveSettingRunDTO the incentive setting run DTO
	 * @return the incentive setting run DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-by-code")
	public IncentiveSettingRunDTO findByCode(
			@RequestBody IncentiveSettingRunDTO incentiveSettingRunDTO)
			throws ResourcesNotFoundException {

		return incentiveSettingRunService.findByCode(incentiveSettingRunDTO.getCode());
	}

	/**
	 * Update status.
	 * 
	 * @author HaythemBenizid
	 * @param incentiveSettingRunDTO the incentive setting run DTO
	 * @return the incentive setting run DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-status")
	public IncentiveSettingRunDTO updateStatus(
			@RequestBody IncentiveSettingRunDTO incentiveSettingRunDTO)
			throws ResourcesNotFoundException {

		return incentiveSettingRunService.updateStatus(incentiveSettingRunDTO);
	}

	/**
	 * Update apply discount apply branch.
	 * 
	 * @author idridi
	 * @param incentiveSettingRunDTO the incentive setting run DTO
	 * @return the incentive setting run DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-apply-discount-branch")
	public IncentiveSettingRunDTO updateApplyDiscountApplyBranch(
			@RequestBody IncentiveSettingRunDTO incentiveSettingRunDTO)
			throws ResourcesNotFoundException {

		return incentiveSettingRunService.updateApplyDiscountOrBranch(incentiveSettingRunDTO);
	}
}
