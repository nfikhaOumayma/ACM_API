/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.GroupeUsersFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.GroupeService;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.pagination.GroupePaginationDTO;

/**
 * The {@link GroupeController} class used to control all the User request .
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RestController
@RequestMapping("/groupes")
public class GroupeController {

	/** The groupe service. */
	@Autowired
	private GroupeService groupeService;

	/**
	 * Find GROUPE by given params.
	 *
	 * @author HaythemBenizid
	 * @param groupeDTO the groupe DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<GroupeDTO> find(@RequestBody GroupeDTO groupeDTO) {

		return groupeService.find(groupeDTO);
	}

	/**
	 * Find all GROUPE.
	 *
	 * @author MoezMhiri
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<GroupeDTO> findAll() {

		return groupeService.find();
	}

	/**
	 * Creates the GroupeDTO by new value.
	 * 
	 * @author HaythemBenizid
	 * @param groupeDTO the groupe DTO
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the resource not found exception
	 */
	@PostMapping("/create")
	public GroupeDTO create(@RequestBody GroupeDTO groupeDTO) throws ResourcesNotFoundException {

		return groupeService.save(groupeDTO);
	}

	/**
	 * Update the parameter by id.
	 * 
	 * @author HaythemBenizid
	 * @param groupeDTO the groupe DTO
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public GroupeDTO update(@RequestBody GroupeDTO groupeDTO) throws ResourcesNotFoundException {

		return groupeService.save(groupeDTO.getId(), groupeDTO);
	}

	/**
	 * Update enabled (Enabled / Disabled).
	 * 
	 * @author MoezMhiri
	 * @param groupeDTO the groupe DTO
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GroupeUsersFoundException the users found exception
	 */
	@PutMapping("/update-enabled")
	public GroupeDTO updateWithEnabled(@RequestBody GroupeDTO groupeDTO)
			throws ResourcesNotFoundException, GroupeUsersFoundException {

		return groupeService.updateEnabled(groupeDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author MoezMhiri
	 * @param groupePaginationDTO the groupe pagination DTO
	 * @return the groupe pagination DTO
	 */
	@PostMapping("/find-groupe-pagination")
	public GroupePaginationDTO findPagination(
			@RequestBody GroupePaginationDTO groupePaginationDTO) {

		return groupeService.find(groupePaginationDTO);
	}

	/**
	 * Find groupe by code.
	 * 
	 * @author idridi
	 * @param codeGroupe the code groupe
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find-groupe-by-code")
	public GroupeDTO findGroupeByCode(@RequestBody String codeGroupe)
			throws ResourcesNotFoundException {

		return groupeService.findByCode(codeGroupe);
	}
}
