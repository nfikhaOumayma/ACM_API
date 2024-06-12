/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.ApplicationFeeService;
import com.acm.utils.dtos.ApplicationFeeDTO;

/**
 * {@link LoadDataApplicationFeeController} class.
 *
 * @author MoezMhiri
 * @since 1.1.5
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataApplicationFeeController {

	/** The application fee service. */
	@Autowired
	private ApplicationFeeService applicationFeeService;

	/**
	 * Find application fee.
	 *
	 * @author MoezMhiri
	 * @param idAccount the id account
	 * @return the long
	 */
	@GetMapping("/application-fee/{idAccount}")
	public Long findApplicationFee(@PathVariable("idAccount") Long idAccount) {

		return applicationFeeService.findApplicationFee(idAccount);
	}

	/**
	 * Find application fee by product.
	 *
	 * @return the list
	 */
	@GetMapping("/application-fees")
	public List<ApplicationFeeDTO> findFeeBy() {

		return applicationFeeService.findFees();
	}

	/**
	 * Check fee.
	 *
	 * @param idAccount the id account
	 * @param listIds the list ids
	 * @return the long
	 */
	@PostMapping("/check-fee/{idAccount}")
	Long checkFee(@PathVariable("idAccount") Long idAccount, @RequestBody List<Long> listIds) {

		return applicationFeeService.checkFees(idAccount, listIds);
	}
}
