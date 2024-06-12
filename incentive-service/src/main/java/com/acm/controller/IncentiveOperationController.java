/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.IncentiveRegistrationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.IncentiveOperationService;
import com.acm.utils.dtos.IncentiveOperationDTO;
import com.acm.utils.dtos.pagination.IncentiveOperationPaginationDTO;

/**
 * This class @{link IncentiveOperationController} used to control all the IncentiveOperationSetting
 * requests.
 * 
 * @author idridi
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-operation")
public class IncentiveOperationController {

	/** The incentive operation service. */
	@Autowired
	private IncentiveOperationService incentiveOperationService;

	/**
	 * Find by id.
	 * 
	 * @author idridi
	 * @param id the id
	 * @return the incentive operation DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public IncentiveOperationDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return incentiveOperationService.find(id);
	}

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveOperationDTO> find(
			@RequestBody IncentiveOperationDTO incentiveOperationDTO) {

		return incentiveOperationService.find(incentiveOperationDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author idridi
	 * @param incentiveOperationPaginationDTO the incentive operation pagination DTO
	 * @return the incentive operation pagination DTO
	 */
	@PostMapping("/find-pagination")
	public IncentiveOperationPaginationDTO findPagination(
			@RequestBody IncentiveOperationPaginationDTO incentiveOperationPaginationDTO) {

		return incentiveOperationService.find(incentiveOperationPaginationDTO);
	}

	/**
	 * Creates the.
	 *
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the incentive operation DTO
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	@PostMapping("/create")
	public IncentiveOperationDTO create(@RequestBody IncentiveOperationDTO incentiveOperationDTO)
			throws IncentiveRegistrationException {

		return incentiveOperationService.save(incentiveOperationDTO);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the incentive operation DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	@PutMapping("/update")
	public IncentiveOperationDTO update(@RequestBody IncentiveOperationDTO incentiveOperationDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException {

		return incentiveOperationService.save(incentiveOperationDTO.getId(), incentiveOperationDTO);
	}

	/**
	 * Delete.
	 *
	 * @author idridi
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		incentiveOperationService.delete(id);
	}

	/**
	 * Save status by given product ID.
	 * 
	 * @author idridi
	 * @param incentiveOperationDTO the incentive operation DTO
	 * @return the list
	 */
	@PostMapping("/enable")
	public List<IncentiveOperationDTO> saveStatus(
			@RequestBody IncentiveOperationDTO incentiveOperationDTO) {

		return incentiveOperationService.saveStatus(incentiveOperationDTO);
	}
}
