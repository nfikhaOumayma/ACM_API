package com.acm.api_emdha.controller;


/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_emdha.service.EmdhaApiService;
import com.acm.utils.dtos.LoanDTO;

/**
 * The Class MobishastraController.
 */
@RestController
@RequestMapping("/emdha-api")
public class EmdhaApiController {

	/** The emdha api service. */
	@Autowired
	private EmdhaApiService emdhaApiService;

	/**
	 * Emdha api.
	 *
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	@PostMapping("/sign")
	public boolean emdhaApi(@RequestBody LoanDTO loanDTO) {

		return emdhaApiService.emdhaApi(loanDTO);

	}
}
