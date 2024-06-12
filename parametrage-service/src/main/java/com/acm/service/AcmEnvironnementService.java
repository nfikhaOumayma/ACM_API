/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmEnvironnementDTO;

/**
 * {@link AcmEnvironnementService} interface.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface AcmEnvironnementService {

	/**
	 * Find {@link AcmEnvironnementDTO} by given Key.
	 *
	 * @author HaythemBenizid
	 * @param key the key
	 * @return the acm environnement DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmEnvironnementDTO find(String key) throws ResourcesNotFoundException;

	/**
	 * Find from setting table like given KEY.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acm environnement DTO
	 * @return the list
	 */
	List<AcmEnvironnementDTO> findLikeKey(AcmEnvironnementDTO acmEnvironnementDTO);

	/**
	 * Find by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acm environnement DTO
	 * @return the list
	 */
	List<AcmEnvironnementDTO> find(AcmEnvironnementDTO acmEnvironnementDTO);

	/**
	 * The method used for saving the given {@link AcmEnvironnementDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acmEnvironnement DTO
	 * @return the acmEnvironnement DTO
	 */
	AcmEnvironnementDTO save(AcmEnvironnementDTO acmEnvironnementDTO);

	/**
	 * The method used for updating the given {@link AcmEnvironnementDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param acmEnvironnementDTO the acmEnvironnement DTO
	 * @return the acmEnvironnement DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmEnvironnementDTO save(Long id, AcmEnvironnementDTO acmEnvironnementDTO)
			throws ResourcesNotFoundException;

	/**
	 * Update limite (USED in BATCH cases).
	 *
	 * @author HaythemBenizid
	 * @param key the key
	 * @param limite the limite
	 * @return the acm environnement DTO
	 */
	AcmEnvironnementDTO updateLimite(String key, String limite);

	/**
	 * Find All .
	 *
	 * @author YesserSomai
	 * @return the List of acmEnvironnement DTO
	 */
	List<AcmEnvironnementDTO> find();

	/**
	 * Find by category.
	 *
	 * @author Ines Dridi
	 * @param category the category
	 * @return the acm environnement DTO
	 */
	List<AcmEnvironnementDTO> findByCategory(String category);

	/**
	 * Find by keys.
	 *
	 * @author Ines Dridi
	 * @param keys the keys
	 * @return the list
	 */
	List<AcmEnvironnementDTO> findByKeys(List<String> keys);

	/**
	 * Check athorisation connected user.
	 *
	 * @author idridi
	 * @param acmEnvironnementKey the acm environnement key
	 * @return the boolean
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	Boolean checkAthorisationConnectedUser(String acmEnvironnementKey)
			throws ResourcesNotFoundException;

}
