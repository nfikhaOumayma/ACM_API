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

import com.acm.service.LoadDataAccountGLService;

/**
 * {@link LoadDataAccountGLController} class.
 *
 * @author yesser.somai
 * @since 1.0.7
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataAccountGLController {

	/** The load data account GL service. */
	@Autowired
	LoadDataAccountGLService loadDataAccountGLService;

	/**
	 * Find account GL list.
	 *
	 * @author yesser.somai
	 * @param branchId the branch id
	 * @return the list of Account GL List
	 */
	@GetMapping("/find-account-list/{branchId}")
	public List<String> findAccountGlList(@PathVariable("branchId") Long branchId) {

		return loadDataAccountGLService.findAccountGlList(branchId);
	}

}
