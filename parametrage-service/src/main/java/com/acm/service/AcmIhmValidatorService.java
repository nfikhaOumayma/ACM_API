/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AcmIhmValidatorDTO;

/**
 * {@link AcmIhmValidatorService} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmIhmValidatorService {

	/**
	 * Find IHM Validators.
	 * 
	 * @author ManelLamloum
	 * @param acmIhmValidatorDTO the acm ihm validator DTO
	 * @return the list
	 */
	List<AcmIhmValidatorDTO> find(AcmIhmValidatorDTO acmIhmValidatorDTO);

	/**
	 * Find IHM Validator by id.
	 *
	 * @author ManelLamloum
	 * @param id the id
	 * @return the acm ihm validator DTO
	 */
	AcmIhmValidatorDTO findById(Long id);
}
