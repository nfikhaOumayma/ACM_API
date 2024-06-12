/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.DisburseDTO;
import com.acm.utils.dtos.DisburseResponse;

/**
 * {@link DisburseLoanService } class.
 *
 * @author kouali
 * @since 0.1.0
 */

public interface DisburseLoanService {

	
	/**
	 * Disburse loan.
	 *
	 * @param DisburseDto the disburse dto
	 * @return the disburse response
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	DisburseResponse disburseLoan(DisburseDTO DisburseDto) throws URISyntaxException,
			ApiAbacusException, KeyManagementException, KeyStoreException, NoSuchAlgorithmException;

}
