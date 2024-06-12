/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.service;

import com.acm.utils.dtos.ScreeningDTO;

/**
 * {@link IScoreService} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public interface IScoreService {

	/**
	 * Request API REST I-Score.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	ScreeningDTO requestAPIIScore(ScreeningDTO screeningDTO);

	/**
	 * Generate I-SCORE report.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the byte[]
	 */
	byte[] generateIScoreReport(ScreeningDTO screeningDTO);

}
