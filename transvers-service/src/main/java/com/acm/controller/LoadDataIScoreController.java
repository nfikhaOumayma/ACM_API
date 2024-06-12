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

import com.acm.service.IScoreAbacusService;
import com.acm.utils.dtos.IScoreDTO;

/**
 * This class @{link LoadDataIScoreController}.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataIScoreController {

	/** The I score abacus service. */
	@Autowired
	private IScoreAbacusService iScoreAbacusService;

	/**
	 * Find i-score list.
	 *
	 * @author YesserSomai
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the list
	 */
	@GetMapping("/i-score/{startDate}/{endDate}")
	public List<IScoreDTO> findIScore(@PathVariable("startDate") String startDate,
			@PathVariable("endDate") String endDate) {

		return iScoreAbacusService.findIScore(startDate, endDate);
	}
}
