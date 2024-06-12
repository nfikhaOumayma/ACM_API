/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.CuAccountPortfolioTransferredService;
import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;

/**
 * The Class CuAccountPortfolioTransferredController.
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataAccountPortfolioTransferredController {

	/** The cu account portfolio transferred service. */
	@Autowired
	CuAccountPortfolioTransferredService cuAccountPortfolioTransferredService;

	/**
	 * Find cu account portfolio transferred.
	 *
	 * @param indexCuAccountPortfolioTransferred the index cu account portfolio transferred
	 * @return the list
	 */
	@GetMapping("/find-account-portfolio-list/{indexCuAccountPortfolioTransferred}")
	public List<CUAccountPortfolioTransferredDTO> findCuAccountPortfolioTransferred(
			@PathVariable("indexCuAccountPortfolioTransferred") Long indexCuAccountPortfolioTransferred) {

		return cuAccountPortfolioTransferredService.find(indexCuAccountPortfolioTransferred);
	}

	/**
	 * Find cu account portfolio transferred last.
	 * 
	 * @return the long
	 */
	@GetMapping("/get-cu-account-portfolio-transferred-last-id")
	public Long findCuAccountPortfolioTransferredLast() {

		return cuAccountPortfolioTransferredService.findLast();
	}
}
