/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_simah.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_simah.service.SimahService;
import com.acm.utils.dtos.AuthResponseSimahApiDTO;
import com.acm.utils.dtos.RequestEnquiryNewCustomerSimahApiDTO;
import com.acm.utils.dtos.RequestGetScoreDTO;

/**
 * The Class SimahController.
 */
@RestController
@RequestMapping("/simah")
public class SimahController {

	/** The simah service. */
	@Autowired
	private SimahService simahService;

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	@PostMapping("/token")
	public ResponseEntity<AuthResponseSimahApiDTO> getToken() {

		return simahService.getToken();
	}

	/**
	 * Gets the score.
	 *
	 * @param request the request
	 * @return the score
	 */
	@PostMapping("/score")
	public ResponseEntity<String> getScore(@RequestBody RequestGetScoreDTO request) {

		return simahService.getScore(request);
	}

	/**
	 * Enquiry new customer.
	 *
	 * @param request the request
	 * @param loanId the loan id
	 * @return the response entity
	 */
	@PostMapping("/enquiry-new-customer/{loanId}")
	public ResponseEntity<String> enquiryNewCustomer(
			@RequestBody RequestEnquiryNewCustomerSimahApiDTO request,
			@PathVariable("loanId") Long loanId) {

		return simahService.enquiryCustomer(request, loanId);
	}
}
