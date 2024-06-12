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
import com.acm.service.ConditionalApproveService;
import com.acm.utils.dtos.AcmConditionnalApproveDTO;

/**
 * The Class ConditionnalApproveController.
 */
@RestController
@RequestMapping("/conditionalApprove")
public class ConditionnalApproveController {

	/** The conditional approve service. */
	@Autowired
	private ConditionalApproveService conditionalApproveService;

	/**
	 * Creates the.
	 *
	 * @param acmConditionnalApproveDTOs the acm conditionnal approve DT os
	 * @return the list
	 */
	@PostMapping("/create")
	public List<AcmConditionnalApproveDTO> create(
			@RequestBody List<AcmConditionnalApproveDTO> acmConditionnalApproveDTOs) {

		return conditionalApproveService.create(acmConditionnalApproveDTOs);
	}

	/**
	 * Find by id loan.
	 *
	 * @param acmConditionnalApproveDTO the acm conditionnal approve DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/find")
	public List<AcmConditionnalApproveDTO> findByIdLoan(
			@RequestBody AcmConditionnalApproveDTO acmConditionnalApproveDTO)
			throws ResourcesNotFoundException {

		return conditionalApproveService.find(acmConditionnalApproveDTO);
	}

	/**
	 * Update.
	 *
	 * @param acmConditionnalApproveDTO the acm conditionnal approve DTO
	 * @return the acm conditionnal approve DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update")
	public AcmConditionnalApproveDTO update(
			@RequestBody AcmConditionnalApproveDTO acmConditionnalApproveDTO)
			throws ResourcesNotFoundException {

		return conditionalApproveService.update(acmConditionnalApproveDTO);
	}

	/**
	 * Count conditionnal approve.
	 *
	 * @param idLoan the id loan
	 * @return the long
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("count/loan/{idLoan}")
	public Long countConditionnalApprove(@PathVariable("idLoan") Long idLoan)
			throws ResourcesNotFoundException {

		return conditionalApproveService.countByIdLoanAndConditionnalValidation(idLoan);
	}

	/**
	 * Count conditionnal approve item.
	 *
	 * @param idItem the id item
	 * @return the long
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("count/item/{idItem}")
	public Long countConditionnalApproveItem(@PathVariable("idItem") Long idItem)
			throws ResourcesNotFoundException {

		return conditionalApproveService.countByItem(idItem);
	}

}
