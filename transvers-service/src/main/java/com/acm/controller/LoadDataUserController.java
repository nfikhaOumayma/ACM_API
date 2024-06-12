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

import com.acm.service.UserAbacusService;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.UserDTO;

/**
 * This class @{link LoadDataUserController}.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataUserController {

	/** The user abacus service. */
	@Autowired
	private UserAbacusService userAbacusService;

	/**
	 * Find user.
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the user DTO
	 */
	@GetMapping("/users/{limite}")
	public List<UserDTO> findAll(@PathVariable("limite") Long limite) {

		return userAbacusService.find(limite);
	}

	/**
	 * load all portfolio.
	 * 
	 * @author Salmen Fatnassi
	 * @return the list
	 */
	@GetMapping("/portfolio")
	public List<PortfolioDTO> findAll() {

		return userAbacusService.find();
	}

}
