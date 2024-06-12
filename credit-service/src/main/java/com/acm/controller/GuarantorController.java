/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.CustomerService;
import com.acm.utils.dtos.LoanDTO;

/**
 * This class @{link GuarantorController} used to control all the guarantors requests.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@RestController
@RequestMapping("/guarantors")
public class GuarantorController {

	/** The Customer service. */
	@Autowired
	private CustomerService customerService;

	/**
	 * Adds the guarantors.
	 *
	 * @author salmen.fatnassi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/add-guarantors")
	public LoanDTO addGuarantors(@RequestBody LoanDTO loanDTO) throws ResourcesNotFoundException {

		return customerService.addGuarantors(loanDTO);
	}

}
