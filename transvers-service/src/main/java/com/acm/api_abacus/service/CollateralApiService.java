/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link CollateralApiService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface CollateralApiService {

	/**
	 * Save.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the acm collateral DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	List<AcmCollateralDTO> save(LoanDTO loanDTO)
			throws IOException, ApiAbacusException, URISyntaxException;

}
