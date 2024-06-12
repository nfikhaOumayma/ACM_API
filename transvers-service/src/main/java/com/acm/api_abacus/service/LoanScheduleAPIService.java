/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;
import java.util.List;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.LoanSchedulesApiDTO;

/**
 * The Interface LoanScheduleAPIService.
 */
public interface LoanScheduleAPIService {

	/**
	 * Gets the all schedules.
	 *
	 * @param customerId the customer id
	 * @return the all schedules
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	List<LoanSchedulesApiDTO> getAllSchedules(String customerId)
			throws IOException, ApiAbacusException;
}
