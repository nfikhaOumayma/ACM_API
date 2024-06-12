/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.ExpensesLimit;

/**
 * {@link ExpensesLimitsRepository} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@Repository
public interface ExpensesLimitsRepository extends JpaRepository<ExpensesLimit, Long>,
		CrudRepository<ExpensesLimit, Long>, QuerydslPredicateExecutor<ExpensesLimit> {

	/**
	 * delete By IdBranch.
	 *
	 * @author yesser somai
	 * @param idBranch the id branch
	 */
	@Transactional
	void deleteByIdBranch(Long idBranch);

	/**
	 * Find by id expenses type and id branch.
	 * 
	 * @author idridi
	 * @param idExpensesType the id expenses type
	 * @param idBranch the id branch
	 * @return the expenses limit
	 */
	ExpensesLimit findByIdExpensesTypeAndIdBranch(Long idExpensesType, Long idBranch);

}
