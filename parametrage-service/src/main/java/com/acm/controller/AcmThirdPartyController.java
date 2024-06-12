/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.AcmThirdPartyService;
import com.acm.utils.dtos.AcmThirdPartyDTO;
import com.acm.utils.dtos.pagination.AcmThirdPartyPaginationDTO;

/**
 * The Class AcmThirdPartyController.
 */
@RestController
@RequestMapping("/acm-third-party")
public class AcmThirdPartyController {

	
	/** The third party service. */
	@Autowired
	private AcmThirdPartyService thirdPartyService;


	/**
	 * Creates the.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	@PostMapping("/create")
	public AcmThirdPartyDTO create(
			@RequestBody AcmThirdPartyDTO acmThirdPartyDTO) {

		return thirdPartyService.save(acmThirdPartyDTO);
	}

	
	/**
	 * Find third party pagination.
	 *
	 * @param acmThirdPartyPaginationDTO the acm third party pagination DTO
	 * @return the acm third party pagination DTO
	 */
	@PostMapping("/find-pagination")
	public AcmThirdPartyPaginationDTO findThirdPartyPagination(
			@RequestBody AcmThirdPartyPaginationDTO acmThirdPartyPaginationDTO) {

		return thirdPartyService.find(acmThirdPartyPaginationDTO);
	}


	/**
	 * Save.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	@PostMapping("/update")
	public AcmThirdPartyDTO save(
			@RequestBody AcmThirdPartyDTO acmThirdPartyDTO) {

		return thirdPartyService.save(acmThirdPartyDTO.getId(),
				acmThirdPartyDTO);
	}

	
	/**
	 * Save enable.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the acm third party DTO
	 */
	@PostMapping("/enable")
	public AcmThirdPartyDTO saveEnable(
			@RequestBody AcmThirdPartyDTO acmThirdPartyDTO) {

		return thirdPartyService.saveEnable(acmThirdPartyDTO);
	}

	
	/**
	 * Find third party.
	 *
	 * @param acmThirdPartyDTO the acm third party DTO
	 * @return the list
	 */
	@PostMapping("/find-third-parties")
	public List<AcmThirdPartyDTO> findThirdParty(
			@RequestBody AcmThirdPartyDTO acmThirdPartyDTO) {

		return thirdPartyService.findThirdParty(acmThirdPartyDTO);
	}

}
