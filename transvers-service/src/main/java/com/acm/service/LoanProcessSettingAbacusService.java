/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.LoanProcessSettingDTO;

/**
 * {@link LoanProcessSettingAbacusService} class.
 *
 * @author RadhouaneHomrani
 * @since 0.2.0
 */
public interface LoanProcessSettingAbacusService {

	/**
	 * Find LoanProcessSetting by given id product.
	 * 
	 * @author RadhouaneHomrani
	 * @param productId the product id
	 * @return the loan process DTO
	 */
	List<LoanProcessSettingDTO> find(Long productId);
}
