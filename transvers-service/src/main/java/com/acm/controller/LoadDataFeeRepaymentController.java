/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.service.FeeRepaymentService;

/**
 * {@link LoadDataFeeRepaymentController} class.
 *
 * @author Salmen Fatnassi
 * @since 1.0.8
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataFeeRepaymentController {

	/** The fee repayment service. */
	@Autowired
	private FeeRepaymentService feeRepaymentService;

	/**
	 * Find fee repayment.
	 *
	 * @author Salmen Fatnassi
	 * @param idAccount the id account
	 * @return the long
	 */
	@GetMapping("/fee-repayment/{idAccount}")
	public Long findFeeRepayment(@PathVariable("idAccount") Long idAccount) {

		return feeRepaymentService.findFeeRepayment(idAccount);
	}

	/**
	 * Gets the cu fee id by id acount.
	 *
	 * @param idAccount the id account
	 * @return the cu fee id by id acount
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@GetMapping("/getIdFeeByIdAccount/{accountId}")
	public Long getCuFeeIdByIdAcount(@PathVariable("accountId") Long idAccount)
			throws ApiAbacusException, IOException {

		return feeRepaymentService.findCuFeeId(idAccount);
	}

}
