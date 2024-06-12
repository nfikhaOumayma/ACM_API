/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.soap.service.CheckActionService;
import com.acm.utils.dtos.ScreeningDTO;

/**
 * This class @{link SOAPAPIController}.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@RestController
@RequestMapping("/soap-api")
public class SOAPAPIController {

	/** The check action service. */
	@Autowired
	private CheckActionService checkActionService;

	/**
	 * Send I-SCORE request.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	@PostMapping("/soap-iscore")
	public ScreeningDTO sendRequestIScore(@RequestBody ScreeningDTO screeningDTO) {

		return checkActionService.requestSOAPIScore(screeningDTO);
	}

	/**
	 * Generate I score report.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the byte[]
	 */
	@PostMapping("/report/soap-iscore")
	public byte[] generateIScoreReport(@RequestBody ScreeningDTO screeningDTO) {

		return checkActionService.generateIScoreReport(screeningDTO);
	}

	/**
	 * Request SOAPI score and generate I score report.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	@PostMapping("/soap-iscore-report")
	public ScreeningDTO requestSOAPIScoreAndGenerateIScoreReport(
			@RequestBody ScreeningDTO screeningDTO) {

		return checkActionService.requestSOAPIScoreAndGenerateIScoreReport(screeningDTO);
	}
}
