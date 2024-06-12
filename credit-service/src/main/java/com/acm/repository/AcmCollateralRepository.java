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

import com.acm.utils.models.AcmCollateral;
import com.acm.utils.models.Loan;

/**
 * {@link AcmCollateralRepository} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface AcmCollateralRepository
		extends JpaRepository<AcmCollateral, Long>, QuerydslPredicateExecutor<AcmCollateral>,
		CrudRepository<AcmCollateral, Long>, PagingAndSortingRepository<AcmCollateral, Long> {
	/**
	 * Delete by loan.
	 * 
	 * @author ManelLamloum
	 * @param loan the loan
	 */
	@Transactional
	void deleteByLoan(Loan loan);
}
