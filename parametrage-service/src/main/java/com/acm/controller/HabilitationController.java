/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.HabilitationIHMRouteService;
import com.acm.service.HabilitationService;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;

/**
 * {@link HabilitationController} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RestController
@RequestMapping("/habilitations")
public class HabilitationController {

	/** The habilitation service. */
	@Autowired
	private HabilitationService habilitationService;

	/** The habilitation IHM route service. */
	@Autowired
	private HabilitationIHMRouteService habilitationIHMRouteService;

	/**
	 * find the habilitations for the CONNECTED USER {@link HabilitationDTO}.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 * @throws Exception the exception
	 */
	@GetMapping("/")
	public List<HabilitationDTO> find() throws Exception {

		return habilitationService.find();
	}

	/**
	 * find the habilitations {@link HabilitationDTO}.
	 *
	 * @author MoezMhiri
	 * @param habilitationDTO the habilitation DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-all")
	public List<HabilitationDTO> findAll(@RequestBody HabilitationDTO habilitationDTO)
			throws ResourcesNotFoundException {

		return habilitationService.findAll(habilitationDTO);
	}

	/**
	 * Find IHM route by Client (default ACM).
	 * 
	 * @author HaythemBenizid
	 * @param habilitationIHMRouteDTO the habilitation IHM route DTO
	 * @return the list
	 */
	@PostMapping("/find-ihm-route")
	public List<HabilitationIHMRouteDTO> find(
			@RequestBody HabilitationIHMRouteDTO habilitationIHMRouteDTO) {

		return habilitationIHMRouteService.find(habilitationIHMRouteDTO);
	}

	/**
	 * Creates the HabilitationDTO by new value.
	 * 
	 * @author MoezMhiri
	 * @param habilitationDTO the habilitation DTO
	 * @return the habilitation DTO
	 */
	@PostMapping("/create")
	public HabilitationDTO create(@RequestBody HabilitationDTO habilitationDTO) {

		return habilitationService.save(habilitationDTO);
	}

	/**
	 * update All.
	 * 
	 * @author MoezMhiri
	 * @param habilitationDTOs the list habiliation DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update-all")
	public List<HabilitationDTO> updateAll(@RequestBody List<HabilitationDTO> habilitationDTOs)
			throws ResourcesNotFoundException {

		return habilitationService.updateAll(habilitationDTOs);
	}

	/**
	 * Gets the mac.
	 *
	 * @return the mac
	 * @throws ParseException the parse exception
	 */
	@GetMapping("/mac")
	public List<String> getMac() throws ParseException {

		List<String> lst = new ArrayList<>();
		String mac = habilitationService.getMacAdressFromServer();
		lst.add(mac);
		return lst;

	}
}
