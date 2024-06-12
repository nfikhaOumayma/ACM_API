/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;

/**
 * {@link FeeRepaymentService} class.
 *
 * @author Salmen Fatnassi
 * @since 1.0.8
 */
public interface FeeRepaymentService {

	/**
	 * Find details.
	 *
	 * @author Salmen Fatnassi
	 * @param idAccount the id account
	 * @return the long
	 */
	Long findFeeRepayment(Long idAccount);

	/**
	 * Find cu fee id.
	 *
	 * @author kouali
	 * @param idAccount the id account
	 * @return the long
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	Long findCuFeeId(Long idAccount) throws ApiAbacusException, IOException;

}
