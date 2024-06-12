/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.CollaterolDTO;

/**
 * {@link CollateralAbacusService} interface.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public interface CollateralAbacusService {

	/**
	 * Find Collaterol DATA by idLoan from AbacusDb.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list Collaterol DTO
	 */
	List<CollaterolDTO> find(Long idLoan);

	/**
	 * Find active and inactive collaterals.
	 *
	 * @author mlamloum
	 * @param idLoans the id loans
	 * @return the list
	 */
	List<AcmCollateralDTO> findActiveAndInactiveCollaterols(List<Long> idLoans);

}
