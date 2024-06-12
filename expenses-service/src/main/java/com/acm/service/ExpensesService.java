/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.ExpenseDrAndCrAccountsEmptyException;
import com.acm.exceptions.type.ExpensesLimitNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ExpensesCountDTO;
import com.acm.utils.dtos.ExpensesDTO;
import com.acm.utils.dtos.pagination.ExpensesPaginationDTO;

/**
 * {@link ExpensesService} class.
 *
 * @author Ines Dridi
 * @since 1.1.3
 */
public interface ExpensesService {

	/**
	 * Find.
	 *
	 * @author Ines Dridi
	 * @param expensesPaginationDTO the expenses pagination DTO
	 * @return the expenses pagination DTO
	 */
	ExpensesPaginationDTO find(ExpensesPaginationDTO expensesPaginationDTO);

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param expensesDTO the expenses DTO
	 * @return the expenses DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws ExpenseDrAndCrAccountsEmptyException the expense dr and cr accounts empty exception
	 * @throws ExpensesLimitNotFoundException the expenses limit not found exception
	 */
	ExpensesDTO save(Long id, ExpensesDTO expensesDTO)
			throws ResourcesNotFoundException, ApiAbacusException,
			ExpenseDrAndCrAccountsEmptyException, ExpensesLimitNotFoundException;

	/**
	 * Save.
	 * 
	 * @author YesserSomai
	 * @param expensesDTO the expenses DTO
	 * @return the expenses DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ExpensesDTO save(ExpensesDTO expensesDTO) throws ResourcesNotFoundException;

	/**
	 * Count.
	 * 
	 * @author ManelLamloum
	 * @return the expenses count DTO
	 */
	ExpensesCountDTO count();

	/**
	 * Find account gl list.
	 *
	 * @author yesser.somai
	 * @param branchId the branch id
	 * @return the list of Account GL List
	 */
	List<String> findAccountGlList(Long branchId);

	/**
	 * Find Expense By Id.
	 *
	 * @author oussema.madiouni
	 * @param id expense
	 * @return expense
	 */
	ExpensesDTO findExpensesById(Long id);

}
