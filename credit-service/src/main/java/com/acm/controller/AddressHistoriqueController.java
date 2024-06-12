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

import com.acm.service.AddressHistoriqueService;
import com.acm.utils.dtos.AddressHistoriqueDTO;

/**
 * This class @{link AddressHistoriqueController} used to control all the
 * {@link AddressHistoriqueDTO} requests.
 *
 * @author YesserSomai
 * @since 1.0.14
 */
@RestController
@RequestMapping("/address-historique")
public class AddressHistoriqueController {

	/** The AddressHistorique service. */
	@Autowired
	private AddressHistoriqueService addressHistoriqueService;

	/**
	 * Find {@link List} of {@link AddressHistoriqueDTO} by Requested params.
	 *
	 * @author YesserSomai
	 * @param addressHistoriqueDTO the addressHistorique DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AddressHistoriqueDTO> find(@RequestBody AddressHistoriqueDTO addressHistoriqueDTO) {

		return addressHistoriqueService.find(addressHistoriqueDTO);
	}

}
