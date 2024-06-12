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

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmCollateralService;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.LoanDTO;

/**
 * {@link AcmCollateralController } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/acm-collateral")
public class AcmCollateralController {

	/** The acm collateral service. */
	@Autowired
	private AcmCollateralService acmCollateralService;

	/**
	 * Save.
	 * 
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 */
	@PostMapping("/create-all")
	public void save(@RequestBody LoanDTO loanDTO) {

		acmCollateralService.save(loanDTO);
	}

	/**
	 * Find.
	 * 
	 * @author mlamloum
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AcmCollateralDTO> find(@RequestBody AcmCollateralDTO acmCollateralDTO) {

		return acmCollateralService.find(acmCollateralDTO);
	}

	/**
	 * Save or update or delete.
	 *
	 * @param acmCollateralDTO the acm collateral DTO
	 * @return the acm collateral DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save-update-delete/")
	public AcmCollateralDTO saveOrUpdateOrDelete(@RequestBody AcmCollateralDTO acmCollateralDTO)
			throws ResourcesNotFoundException {

		return acmCollateralService.saveOrUpdateOrDelete(acmCollateralDTO);
	}
}
