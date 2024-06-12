/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmIhmFormDTO;

// TODO: Auto-generated Javadoc
/**
 * {@link AcmIhmFormService} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmIhmFormService {

	/**
	 * Find by ihm route.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @return the list
	 */
	List<AcmIhmFormDTO> find(AcmIhmFormDTO acmIhmFormDTO);

	/**
	 * Create ACM IHM form.
	 *
	 * @author YesserSomai
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @return acmIhmFormDTO the acm ihm form DTO
	 */
	AcmIhmFormDTO save(AcmIhmFormDTO acmIhmFormDTO);

	/**
	 * update ACM IHM form.
	 *
	 * @author YesserSomai
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @param id the acm Ihm Form DTO ID;
	 * @return the acm ihm form DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmIhmFormDTO save(AcmIhmFormDTO acmIhmFormDTO, Long id) throws ResourcesNotFoundException;

}
