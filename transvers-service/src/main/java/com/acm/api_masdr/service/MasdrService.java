/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_masdr.service;

import org.springframework.http.ResponseEntity;

import com.acm.utils.dtos.EmploymentStatusInfoMasdrAPIDTO;

/**
 * The Interface MasdrService.
 */
public interface MasdrService {

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	String getToken();

	/**
	 * Mofeed api.
	 *
	 * @param identity the identity
	 * @param loanId the loan id
	 * @return the response entity
	 */
	ResponseEntity<EmploymentStatusInfoMasdrAPIDTO> mofeedApi(String identity, Long loanId);

}
