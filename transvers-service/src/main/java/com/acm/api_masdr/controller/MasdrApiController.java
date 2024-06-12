/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_masdr.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_masdr.service.MasdrService;
import com.acm.utils.dtos.EmploymentStatusInfoMasdrAPIDTO;

/**
 * The Class MasdrApiController.
 */
@RestController
@RequestMapping("/masder-api")
public class MasdrApiController {

	/** The masder service. */
	@Autowired
	private MasdrService masderService;

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	@GetMapping("/token")
	public String getToken() {

		return masderService.getToken();
	}

	/**
	 * Mofeed api.
	 *
	 * @param identity the identity
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@GetMapping("/mofeed/{identity}/{loanId}")
	public ResponseEntity<EmploymentStatusInfoMasdrAPIDTO> mofeedApi(
			@PathVariable("identity") String identity, @PathVariable("loanId") Long loanId) {

		return masderService.mofeedApi(identity, loanId);

	}

}
