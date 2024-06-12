/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.IncentiveOperationRunService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.IncentiveHistoryDTO;

/**
 * This class @{link IncentiveOperationRunController} used to control all the Incentive requests.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-run-operation")
public class IncentiveOperationRunController {

	/** The incentive operation run service. */
	@Autowired
	private IncentiveOperationRunService incentiveOperationRunService;

	/**
	 * Run incentive calculate Procedure.
	 * 
	 * @author HaythemBenizid
	 * @return the string
	 */
	@GetMapping("/calculate")
	public String runIncentiveCalculate() {

		incentiveOperationRunService.runIncentiveCalculate();
		return JSONObject.quote("Run Calculate DONE !!");
	}

	/**
	 * Generate incentive report.
	 *
	 * @author HaythemBenizid
	 * @param incentiveHistoryDTO the incentive history DTO
	 * @return the byte[]
	 */
	@PostMapping("/report-excel")
	public byte[] generateIncentiveReport(@RequestBody IncentiveHistoryDTO incentiveHistoryDTO) {

		return incentiveOperationRunService.generateIncentiveReport(incentiveHistoryDTO.getYear(),
				incentiveHistoryDTO.getMonth());
	}

	/**
	 * Gets the run year.
	 * 
	 * @author HaythemBenizid
	 * @return the run year
	 */
	@GetMapping("/get-run-year")
	public List<AcmStatutsDTO> getRunYear() {

		List<Integer> years = incentiveOperationRunService.getRunYear();
		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		int key = 1;
		for (Object year : years) {
			acmStatutsDTOs.add(new AcmStatutsDTO(key, String.valueOf(year)));
			key++;
		}
		return acmStatutsDTOs;
	}

	/**
	 * Gets the run month.
	 * 
	 * @author HaythemBenizid
	 * @return the run month
	 */
	@GetMapping("/get-run-month")
	public List<AcmStatutsDTO> getRunMonth() {

		List<Integer> months = incentiveOperationRunService.getRunMonth();
		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		int key = 1;
		for (Object month : months) {
			acmStatutsDTOs.add(new AcmStatutsDTO(key, String.valueOf(month)));
			key++;
		}
		return acmStatutsDTOs;
	}
}