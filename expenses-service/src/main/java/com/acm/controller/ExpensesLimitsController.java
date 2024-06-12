/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ExpensesLimitsService;
import com.acm.utils.dtos.ExpensesLimitDTO;

/**
 * This class @{link ExpensesTypeController} used to control all the Expenses Type requests.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@RestController
@RequestMapping("/expenses-limit")
public class ExpensesLimitsController {

	/** The expenses type service. */
	@Autowired
	private ExpensesLimitsService expensesLimitsService;

	/**
	 * Find Expenses Limit.
	 * 
	 * @author YesserSomai
	 * @param expensesLimitDTO the expenses limit DTO
	 * @return the list of Expenses Limit DTO
	 */
	@PostMapping("/")
	public List<ExpensesLimitDTO> find(@RequestBody ExpensesLimitDTO expensesLimitDTO) {

		return expensesLimitsService.find(expensesLimitDTO);
	}

	/**
	 * Save Expenses Limit.
	 * 
	 * @author YesserSomai
	 * @param expensesLimitDTOs the expenses limit DT os
	 * @return the list of Expenses Limit DTO
	 */
	@PostMapping("/save")
	public List<ExpensesLimitDTO> save(@RequestBody List<ExpensesLimitDTO> expensesLimitDTOs) {

		return expensesLimitsService.save(expensesLimitDTOs);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param expensesLimitDTOs the expenses limit DT os
	 * @return the expenses limit DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update")
	public ExpensesLimitDTO update(@RequestBody ExpensesLimitDTO expensesLimitDTOs)
			throws ResourcesNotFoundException {

		return expensesLimitsService.save(expensesLimitDTOs.getId(), expensesLimitDTOs);
	}

	/**
	 * Update.
	 *
	 * @param lasUpdateDateKey the las update date key
	 * @return the date
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/refresh-limit-expenses")
	public Date update(@RequestBody String lasUpdateDateKey) throws ResourcesNotFoundException {

		return expensesLimitsService.refreshLimits(lasUpdateDateKey);
	}
}
