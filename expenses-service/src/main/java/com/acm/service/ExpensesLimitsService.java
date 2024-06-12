/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.Date;
import java.util.List;

import com.acm.exceptions.type.ExpenseDrAndCrAccountsEmptyException;
import com.acm.exceptions.type.ExpensesLimitNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ExpensesLimitDTO;

/**
 * {@link ExpensesLimitsService} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
public interface ExpensesLimitsService {

	/**
	 * Find Expenses Limit.
	 * 
	 * @author YesserSomai
	 * @param expensesLimitDTO the expenses limit DTO
	 * @return the list of Expenses Limit DTO
	 */
	List<ExpensesLimitDTO> find(ExpensesLimitDTO expensesLimitDTO);

	/**
	 * Save Expenses Limit.
	 *
	 * @author YesserSomai
	 * @param expensesLimitDTOs the expenses limit DT os
	 * @return the list of Expenses Limit DTO
	 */
	List<ExpensesLimitDTO> save(List<ExpensesLimitDTO> expensesLimitDTOs);

	/**
	 * refrech Limits used in batch service.
	 *
	 * @author YesserSomai
	 * @param lasUpdateDateKey the las update date key
	 * @return the list
	 */
	Date refreshLimits(String lasUpdateDateKey);

	/**
	 * Save.
	 * 
	 * @author HaythemBenizid
	 * @param expensesLimitDTO the expenses limit DTO
	 * @return the expenses limit DTO
	 */
	ExpensesLimitDTO save(ExpensesLimitDTO expensesLimitDTO);

	/**
	 * Save.
	 *
	 * @author idridi
	 * @param id the id
	 * @param expensesLimitDTO the expenses limit DTO
	 * @return the expenses limit DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ExpensesLimitDTO save(Long id, ExpensesLimitDTO expensesLimitDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find by type and branch id.
	 *
	 * @param expensesLimitDTO the expenses limit DTO
	 * @return the expenses limit DTO
	 * @throws ExpenseDrAndCrAccountsEmptyException the expense dr and cr accounts empty exception
	 * @throws ExpensesLimitNotFoundException the expenses limit not found exception
	 */
	ExpensesLimitDTO findByTypeAndBranchId(ExpensesLimitDTO expensesLimitDTO)
			throws ExpenseDrAndCrAccountsEmptyException, ExpensesLimitNotFoundException;
}
