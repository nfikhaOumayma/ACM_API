/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;
import java.util.List;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.GuarantorDTO;

/**
 * {@link GuarantorAbacusApiService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface GuarantorAbacusApiService {

	/**
	 * Add Guarantor by given params in ABACUS-DB using API.
	 *
	 * @author HaythemBenizid
	 * @param guarantorDTO the guarantor DTO
	 * @return the guarantor DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	GuarantorDTO save(GuarantorDTO guarantorDTO) throws IOException, ApiAbacusException;

	/**
	 * Save.
	 * 
	 * @author mlamloum
	 * @param guarantorDTOs the guarantor DT os
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws IllegalArgumentException the illegal argument exception
	 * @throws IllegalAccessException the illegal access exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	void save(List<GuarantorDTO> guarantorDTOs) throws IOException, IllegalArgumentException,
			IllegalAccessException, ApiAbacusException;
}
