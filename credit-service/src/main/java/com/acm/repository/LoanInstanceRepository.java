/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanInstance;

/**
 * Class provides Repo dao for {@link LoanInstance} table.
 * 
 * @author HaythemBenizid
 * @since 1.0.6
 */
@Repository
public interface LoanInstanceRepository
		extends JpaRepository<LoanInstance, Long>, QuerydslPredicateExecutor<LoanInstance>,
		CrudRepository<LoanInstance, Long>, PagingAndSortingRepository<LoanInstance, Long> {

	/**
	 * Delete all {@link LoanInstance} by given loan.
	 *
	 * @author HaythemBenizid
	 * @param loan the loan
	 * @return The return value, of type long, indicates how many records the method deleted
	 */
	@Transactional
	Long deleteByLoan(Loan loan);
}
