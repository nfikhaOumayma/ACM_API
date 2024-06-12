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
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.ExpenseDrAndCrAccountsEmptyException;
import com.acm.exceptions.type.ExpensesLimitNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ExpensesService;
import com.acm.utils.dtos.ExpensesCountDTO;
import com.acm.utils.dtos.ExpensesDTO;
import com.acm.utils.dtos.pagination.ExpensesPaginationDTO;

/**
 * This class @{link ExpensesController} used to control all the Expenses requests.
 * 
 * @author Ines Dridi
 * @since 1.1.3
 */
@RestController
@RequestMapping("/expenses")
public class ExpensesController {

	/** The Expenses service. */
	@Autowired
	private ExpensesService expensesService;

	/**
	 * Find pagination.
	 * 
	 * @author Ines Dridi
	 * @param expensesPaginationDTO the expenses pagination DTO
	 * @return the expenses pagination DTO
	 */
	@PostMapping("/find-pagination")
	public ExpensesPaginationDTO findPagination(
			@RequestBody ExpensesPaginationDTO expensesPaginationDTO) {

		return expensesService.find(expensesPaginationDTO);
	}

	/**
	 * Update.
	 *
	 * @param expensesDTO the expenses DTO
	 * @return the expenses DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws ExpenseDrAndCrAccountsEmptyException the expense dr and cr accounts empty exception
	 * @throws ExpensesLimitNotFoundException the expenses limit not found exception
	 */
	@PutMapping("/update")
	public ExpensesDTO update(@RequestBody ExpensesDTO expensesDTO)
			throws ResourcesNotFoundException, ApiAbacusException,
			ExpenseDrAndCrAccountsEmptyException, ExpensesLimitNotFoundException {

		return expensesService.save(expensesDTO.getId(), expensesDTO);
	}

	/**
	 * Create.
	 * 
	 * @author YesserSomai
	 * @param expensesDTO the expenses DTO
	 * @return the expenses DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public ExpensesDTO create(@RequestBody ExpensesDTO expensesDTO)
			throws ResourcesNotFoundException {

		return expensesService.save(expensesDTO);
	}

	/**
	 * Count.
	 * 
	 * @author ManelLamloum
	 * @return the expenses count DTO
	 */
	@GetMapping("/count")
	public ExpensesCountDTO count() {

		return expensesService.count();
	}

	/**
	 * Find account gl list.
	 *
	 * @author yesser.somai
	 * @param branchId the branch id
	 * @return the list of Account GL List
	 */
	@GetMapping("/find-account-list/{branchId}")
	public List<String> findAccountGlList(@PathVariable("branchId") Long branchId) {

		return expensesService.findAccountGlList(branchId);
	}

	/**
	 * Find Expenses by id.
	 *
	 * @author oussema.madiouni
	 * @param expensesId the expenses id
	 * @return Expense Data
	 */
	@GetMapping("/{expensesId}")
	public ExpensesDTO findExpensesById(@PathVariable("expensesId") Long expensesId) {

		return expensesService.findExpensesById(expensesId);
	}
}
