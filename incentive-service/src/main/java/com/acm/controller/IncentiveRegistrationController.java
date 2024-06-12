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
import com.acm.service.IncentiveRegistrationService;
import com.acm.utils.dtos.IncentiveRegistrationDTO;
import com.acm.utils.dtos.pagination.IncentiveRegistrationPaginationDTO;

/**
 * This class @{link IncentiveRegestrationController} used to control all the
 * IncentiveRegestrationSetting requests.
 * 
 * @author idridi
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-registration")
public class IncentiveRegistrationController {

	/** The incentive registration service. */
	@Autowired
	private IncentiveRegistrationService incentiveRegistrationService;

	/**
	 * Find by id.
	 * 
	 * @author idridi
	 * @param id the id
	 * @return the incentive registration DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public IncentiveRegistrationDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return incentiveRegistrationService.find(id);
	}

	/**
	 * Find.
	 *
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveRegistrationDTO> find(
			@RequestBody IncentiveRegistrationDTO incentiveRegistrationDTO) {

		return incentiveRegistrationService.find(incentiveRegistrationDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationPaginationDTO the incentive registration pagination DTO
	 * @return the incentive registration pagination DTO
	 */
	@PostMapping("/find-pagination")
	public IncentiveRegistrationPaginationDTO findPagination(
			@RequestBody IncentiveRegistrationPaginationDTO incentiveRegistrationPaginationDTO) {

		return incentiveRegistrationService.find(incentiveRegistrationPaginationDTO);
	}

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the incentive registration DTO
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	@PostMapping("/create")
	public IncentiveRegistrationDTO create(
			@RequestBody IncentiveRegistrationDTO incentiveRegistrationDTO)
			throws IncentiveRegistrationException {

		return incentiveRegistrationService.save(incentiveRegistrationDTO);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the incentive registration DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	@PutMapping("/update")
	public IncentiveRegistrationDTO update(
			@RequestBody IncentiveRegistrationDTO incentiveRegistrationDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException {

		return incentiveRegistrationService.save(incentiveRegistrationDTO.getId(),
				incentiveRegistrationDTO);
	}

	/**
	 * Delete.
	 *
	 * @author idridi
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		incentiveRegistrationService.delete(id);
	}

	/**
	 * Save enable.
	 * 
	 * @author idridi
	 * @param incentiveRegistrationDTO the incentive registration DTO
	 * @return the incentive registration DTO
	 */
	@PostMapping("/enable")
	public List<IncentiveRegistrationDTO> saveStatus(
			@RequestBody IncentiveRegistrationDTO incentiveRegistrationDTO) {

		return incentiveRegistrationService.saveStatus(incentiveRegistrationDTO);
	}
}
