/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;

/**
 * {@link CuAccountPortfolioTransferredService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface CuAccountPortfolioTransferredService {

	/**
	 * Find.
	 *
	 * @param lastCuAccountPortfolioTransferred the last cu account portfolio transferred
	 * @return the list
	 */
	List<CUAccountPortfolioTransferredDTO> find(Long lastCuAccountPortfolioTransferred);

	/**
	 * Find last.
	 *
	 * @return the CU account portfolio transferred DTO
	 */
	Long findLast();
}
