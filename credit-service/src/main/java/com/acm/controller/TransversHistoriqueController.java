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

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.TransversHistoriqueService;
import com.acm.utils.dtos.TransversHistoriqueDTO;

/**
 * This class @{link TransversHistoryController} used to control all the TransversHistory requests.
 *
 * @author MoezMhiri
 * @since 1.0.12
 */
@RestController
@RequestMapping("/transvers-historique")
public class TransversHistoriqueController {

	/** The TransversHistoriquer service. */
	@Autowired
	private TransversHistoriqueService transversHistoriqueService;

	/**
	 * Find TransversHistorique by id=> FULL data with list Address && UDFs.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the transvers historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public TransversHistoriqueDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return transversHistoriqueService.find(id);
	}

	/**
	 * Find {@link List} of {@link TransversHistoriquerDTO} by Requested params.
	 *
	 * @author MoezMhiri
	 * @param historiqueDTO the historique DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<TransversHistoriqueDTO> find(@RequestBody TransversHistoriqueDTO historiqueDTO) {

		return transversHistoriqueService.find(historiqueDTO);
	}

	/**
	 * Create the TransversHistoriquer.
	 *
	 * @author MoezMhiri
	 * @param historiqueDTO the historique DTO
	 * @return the transvers historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public TransversHistoriqueDTO create(@RequestBody TransversHistoriqueDTO historiqueDTO)
			throws ResourcesNotFoundException {

		return transversHistoriqueService.save(historiqueDTO);
	}

}
