/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AcmThirdPartyDTO;
import com.acm.utils.dtos.pagination.AcmThirdPartyPaginationDTO;

/**
 * The Interface AcmThirdPartyService.
 */
public interface AcmThirdPartyService {

	
	/**
	 * Save.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	AcmThirdPartyDTO save(AcmThirdPartyDTO acmThirdPartyDTO);


	/**
	 * Find.
	 *
	 * @param acmThirdPartyPaginationDTO the acm third party pagination DTO
	 * @return the acm third party pagination DTO
	 */
	AcmThirdPartyPaginationDTO find(
			AcmThirdPartyPaginationDTO acmThirdPartyPaginationDTO);

	
	/**
	 * Save.
	 *
	 * @param id the id
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	AcmThirdPartyDTO save(Long id,
			AcmThirdPartyDTO acmThirdPartyDTO);

	
	/**
	 * Save enable.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	AcmThirdPartyDTO saveEnable(
			AcmThirdPartyDTO acmThirdPartyDTO);

	
	
	/**
	 * Find third party.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the list
	 */
	List<AcmThirdPartyDTO> findThirdParty(
			AcmThirdPartyDTO acmThirdPartyDTO);

}
