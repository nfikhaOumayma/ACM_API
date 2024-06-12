/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.json.service.IScoreService;
import com.acm.utils.dtos.ScreeningDTO;

/**
 * This class @{link IScoreAPIController}.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@RestController
@RequestMapping("/api")
public class IScoreAPIController {

	/** The i score service. */
	@Autowired
	private IScoreService iScoreService;

	/**
	 * Send I-SCORE request.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	@PostMapping("/run-iscore")
	public ScreeningDTO sendRequestIScore(@RequestBody ScreeningDTO screeningDTO) {

		return iScoreService.requestAPIIScore(screeningDTO);
	}

	/**
	 * Generate I-SCORE report.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the byte[]
	 */
	@PostMapping("/report/iscore")
	public byte[] generateIScoreReport(@RequestBody ScreeningDTO screeningDTO) {

		return iScoreService.generateIScoreReport(screeningDTO);
	}
}
