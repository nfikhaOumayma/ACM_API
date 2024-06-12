/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmConditionnalApprove;
import com.acm.utils.models.Loan;

/**
 * {@link ConditionnalApproveRepository} class.
 *
 * @author kouali
 * @since 0.1.0
 */
@Repository
public interface ConditionnalApproveRepository extends JpaRepository<AcmConditionnalApprove, Long>,
		QuerydslPredicateExecutor<AcmConditionnalApprove>,
		CrudRepository<AcmConditionnalApprove, Long>,
		PagingAndSortingRepository<AcmConditionnalApprove, Long> {

	/**
	 * Find by loan.
	 *
	 * @author kouali
	 * @param loan the loan
	 * @return the list
	 */
	List<AcmConditionnalApprove> findByLoan(Loan loan);

	/**
	 * Count by loan and status.
	 *
	 * @author kouali
	 * @param loanId the loan id
	 * @return the long
	 */
	@Query("select count(1) from AcmConditionnalApprove where loan.id = :loanId and  (conditionnalValidation is null or conditionnalValidation is false)")
	Long countByLoanAndConditionnalValidationIsNullOrIsFalse(Long loanId);

	/**
	 * Count by item and conditionnal validation is null or is false.
	 *
	 * @param idItem the id item
	 * @return the long
	 */
	@Query("select count(1) from AcmConditionnalApprove where item.id = :idItem and  (conditionnalValidation is null or conditionnalValidation is false)")
	Long countByItemAndConditionnalValidationIsNullOrIsFalse(Long idItem);

}
