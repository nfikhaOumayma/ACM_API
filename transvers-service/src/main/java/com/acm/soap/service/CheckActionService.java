/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.service;

import com.acm.utils.dtos.ScreeningDTO;

/**
 * {@link CheckActionService} class.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
public interface CheckActionService {

	/**
	 * Request SOAP I-Score.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	ScreeningDTO requestSOAPIScore(ScreeningDTO screeningDTO);

	/**
	 * Generate I score report.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the byte[]
	 */
	byte[] generateIScoreReport(ScreeningDTO screeningDTO);

	/**
	 * Request SOAP I-score and Generate I score report.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 */
	ScreeningDTO requestSOAPIScoreAndGenerateIScoreReport(ScreeningDTO screeningDTO);
}
