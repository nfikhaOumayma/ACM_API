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

import com.acm.service.HabilitationIHMRouteService;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;

/**
 * {@link HabilitationIHMRouteController} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@RestController
@RequestMapping("/ihm-route")
public class HabilitationIHMRouteController {

	/** The product details service. */
	@Autowired
	private HabilitationIHMRouteService habilitationIHMRouteService;

	/**
	 * Find HabilitationIHMRouteDTO.
	 *
	 * @author ManelLamloum
	 * @param habilitationIHMRouteDTO the habilitation IHM route DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<HabilitationIHMRouteDTO> find(
			@RequestBody HabilitationIHMRouteDTO habilitationIHMRouteDTO) {

		return habilitationIHMRouteService.find(habilitationIHMRouteDTO);
	}

}
