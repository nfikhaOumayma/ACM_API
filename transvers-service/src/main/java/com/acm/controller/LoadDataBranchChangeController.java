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

import com.acm.service.BranchChangeService;
import com.acm.utils.dtos.BranchChangeDTO;

/**
 * {@link LoadDataBranchChangeController} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataBranchChangeController {

	/** The branch change service. */
	@Autowired
	BranchChangeService branchChangeService;

	/**
	 * Find.
	 * 
	 * @author mlamloum
	 * @param lastBranchChangeIdSynchronized the last branch change id synchronized
	 * @return the list
	 */
	@GetMapping("/find-branch-change-list/{lastBranchChangeIdSynchronized}")
	public List<BranchChangeDTO> find(
			@PathVariable("lastBranchChangeIdSynchronized") String lastBranchChangeIdSynchronized) {

		return branchChangeService.find(lastBranchChangeIdSynchronized);
	}
}
