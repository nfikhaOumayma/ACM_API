/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmIhmFieldDTO;

/**
 * {@link AcmIhmFieldService} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmIhmFieldService {

	/**
	 * Find AcmIhmFieldDTO.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFieldDTO the acm ihm field DTO
	 * @return the list
	 */
	List<AcmIhmFieldDTO> find(AcmIhmFieldDTO acmIhmFieldDTO);

	/**
	 * Save AcmIhmFieldDTO.
	 *
	 * @author ManelLamloum
	 * @param id the id
	 * @param acmIhmFieldDTO the acm ihm field DTO
	 * @return the acm ihm field DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmIhmFieldDTO save(Long id, AcmIhmFieldDTO acmIhmFieldDTO) throws ResourcesNotFoundException;

	/**
	 * Save all.
	 *
	 * @author YesserSomai
	 * @param acmIhmFieldDTOs the acm ihm field DT os
	 * @return the list
	 */
	List<AcmIhmFieldDTO> saveAll(List<AcmIhmFieldDTO> acmIhmFieldDTOs);

}
