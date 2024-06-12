/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_simah.service;

import org.springframework.http.ResponseEntity;

import com.acm.utils.dtos.AuthResponseSimahApiDTO;
import com.acm.utils.dtos.RequestEnquiryNewCustomerSimahApiDTO;
import com.acm.utils.dtos.RequestGetScoreDTO;

/**
 * The Interface SimahService.
 */
public interface SimahService {

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	ResponseEntity<AuthResponseSimahApiDTO> getToken();

	/**
	 * Gets the score.
	 *
	 * @param request the request
	 * @return the score
	 */
	ResponseEntity<String> getScore(RequestGetScoreDTO request);

	/**
	 * Enquiry customer.
	 *
	 * @param request the request
	 * @param loanId the loan id
	 * @return the response entity
	 */
	ResponseEntity<String> enquiryCustomer(RequestEnquiryNewCustomerSimahApiDTO request,
			Long loanId);

}
