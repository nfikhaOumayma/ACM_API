/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.ApplicationFeeDTO;

/**
 * {@link ApplicationFeeService} class.
 *
 * @author moezMhiri
 * @since 1.1.5
 */
public interface ApplicationFeeService {

	/**
	 * Find application fee.
	 *
	 * @param idAccount the id account
	 * @return the long
	 */
	Long findApplicationFee(Long idAccount);

	/**
	 * Find fees.
	 *
	 * @author kouali
	 * @return the list
	 */
	List<ApplicationFeeDTO> findFees();

	/**
	 * Check fees.
	 *
	 * @author kouali
	 * @param idAccount the id account
	 * @param lstId the lst id
	 * @return the long
	 */
	Long checkFees(Long idAccount, List<Long> lstId);
}
