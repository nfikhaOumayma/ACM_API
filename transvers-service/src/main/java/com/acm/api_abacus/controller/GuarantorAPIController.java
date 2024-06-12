/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.controller;

import java.io.IOException;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.service.GuarantorAbacusApiService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.GuarantorDTO;

/**
 * This class @{link GuarantorAPIController}.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@RestController
@RequestMapping("/load-data-api-abacus")
public class GuarantorAPIController {

	/** The guarantor abacus api service. */
	@Autowired
	private GuarantorAbacusApiService guarantorAbacusApiService;

	/**
	 * create the guarantor.
	 *
	 * @author HaythemBenizid
	 * @param guarantorDTO the guarantor DTO
	 * @return the guarantor DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/add-guarantor")
	public GuarantorDTO create(@RequestBody GuarantorDTO guarantorDTO)
			throws IOException, ApiAbacusException {

		return guarantorAbacusApiService.save(guarantorDTO);
	}

	/**
	 * Creates the.
	 *
	 * @author mlamloum
	 * @param guarantorDTOs the guarantor DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 */
	@PostMapping("/add-guarantors")
	public void create(@RequestBody List<GuarantorDTO> guarantorDTOs) throws IOException,
			ApiAbacusException, IllegalArgumentException, IllegalAccessException {

		guarantorAbacusApiService.save(guarantorDTOs);
	}
}
