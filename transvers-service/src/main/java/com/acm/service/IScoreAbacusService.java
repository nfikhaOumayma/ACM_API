/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.IScoreDTO;

/**
 * {@link IScoreAbacusService} interface.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public interface IScoreAbacusService {

	/**
	 * Find {@link List} of {@link IScoreDTO} data.
	 *
	 * @author YesserSomai
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the list
	 */

	List<IScoreDTO> findIScore(String startDate, String endDate);

}
