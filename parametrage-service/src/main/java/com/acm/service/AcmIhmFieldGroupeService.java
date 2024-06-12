/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmIhmFieldGroupeDTO;

/**
 * {@link AcmIhmFieldGroupeService} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmIhmFieldGroupeService {

	/**
	 * Find.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFieldGroupeDTO the acm ihm field groupe DTO
	 * @return the list
	 */
	List<AcmIhmFieldGroupeDTO> find(AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO);

	/**
	 * Update habilitation.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFieldGroupeDTO the acm ihm field groupe DTO
	 * @return the acm ihm field groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmIhmFieldGroupeDTO updateHabilitation(AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @author ManelLamloum
	 * @param acmIhmFieldGroupeDTO the acm ihm field groupe DTO
	 * @return the acm ihm field groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmIhmFieldGroupeDTO save(AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO)
			throws ResourcesNotFoundException;
}
