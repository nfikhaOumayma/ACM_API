/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.UserDTO;

/**
 * {@link UserAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface UserAbacusService {

	/**
	 * Find list of ABACUS users (method used by batch).
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	List<UserDTO> find(Long limite);

	/**
	 * Find list of ALL portfolios from ABACUS DB.
	 *
	 * @author Salmen Fatnassi
	 * @return the list of portfolios
	 */
	List<PortfolioDTO> find();
}
