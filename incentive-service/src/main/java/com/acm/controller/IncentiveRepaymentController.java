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
import com.acm.service.IncentiveRepaymentService;
import com.acm.utils.dtos.IncentiveRepaymentDTO;
import com.acm.utils.dtos.pagination.IncentiveRepaymentPaginationDTO;

/**
 * This class @{link IncentiveRepaymentController} used to control all the IncentiveRepaymentSetting
 * requests.
 *
 * @author idridi
 * @since 1.0.8
 */
@RestController
@RequestMapping("/incentive-repayment")
public class IncentiveRepaymentController {

	/** The incentive repayment service. */
	@Autowired
	private IncentiveRepaymentService incentiveRepaymentService;

	/**
	 * Find by id.
	 * 
	 * @author idridi
	 * @param id the id
	 * @return the incentive repayment DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public IncentiveRepaymentDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return incentiveRepaymentService.find(id);
	}

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param incentiveRepaymentDTO the incentive repayment DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<IncentiveRepaymentDTO> find(
			@RequestBody IncentiveRepaymentDTO incentiveRepaymentDTO) {

		return incentiveRepaymentService.find(incentiveRepaymentDTO);
	}

	/**
	 * Find pagination.
	 * 
	 * @author idridi
	 * @param incentiveRepaymentPaginationDTO the incentive repayment pagination DTO
	 * @return the incentive repayment pagination DTO
	 */
	@PostMapping("/find-pagination")
	public IncentiveRepaymentPaginationDTO findPagination(
			@RequestBody IncentiveRepaymentPaginationDTO incentiveRepaymentPaginationDTO) {

		return incentiveRepaymentService.find(incentiveRepaymentPaginationDTO);
	}

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param incentiveRepaymentDTO the incentive repayment DTO
	 * @return the incentive repayment DTO
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	@PostMapping("/create")
	public IncentiveRepaymentDTO create(@RequestBody IncentiveRepaymentDTO incentiveRepaymentDTO)
			throws IncentiveRegistrationException {

		return incentiveRepaymentService.save(incentiveRepaymentDTO);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param incentiveRepaymentDTO the incentive repayment DTO
	 * @return the incentive repayment DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IncentiveRegistrationException the incentive registration exception
	 */
	@PutMapping("/update")
	public IncentiveRepaymentDTO update(@RequestBody IncentiveRepaymentDTO incentiveRepaymentDTO)
			throws ResourcesNotFoundException, IncentiveRegistrationException {

		return incentiveRepaymentService.save(incentiveRepaymentDTO.getId(), incentiveRepaymentDTO);
	}

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		incentiveRepaymentService.delete(id);
	}

}
