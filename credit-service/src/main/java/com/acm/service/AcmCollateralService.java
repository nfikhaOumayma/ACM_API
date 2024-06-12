/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link AcmCollateralService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface AcmCollateralService {

	/**
	 * Find.
	 * 
	 * @author mlamloum
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the list
	 */
	List<AcmCollateralDTO> find(AcmCollateralDTO acmCollateralDTO);

	/**
	 * Save.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 */
	void save(LoanDTO loanDTO);

	/**
	 * Save or update or delete.
	 *
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the acm collateral DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmCollateralDTO saveOrUpdateOrDelete(AcmCollateralDTO acmCollateralDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @author mlamloum
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the acm collateral DTO
	 */
	AcmCollateralDTO save(AcmCollateralDTO acmCollateralDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the acm collateral DTO
	 */
	AcmCollateralDTO save(Long id, AcmCollateralDTO acmCollateralDTO);

	/**
	 * Delete.
	 *
	 * @param acmCollateralDTO the acm collateral DTO
	 */
	void delete(AcmCollateralDTO acmCollateralDTO);
}
