/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanApprovalHistorique;

/**
 * Class provides service dao for {@link LoanApprovalHistorique} table.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@Repository
public interface LoanApprovalHistoriqueRepository
		extends JpaRepository<LoanApprovalHistorique, Long>,
		QuerydslPredicateExecutor<LoanApprovalHistorique>,
		CrudRepository<LoanApprovalHistorique, Long>,
		PagingAndSortingRepository<LoanApprovalHistorique, Long> {

	/**
	 * Find Loan Approval Historique by loan ID order by approvalDate.
	 *
	 * @author HaythemBenizid
	 * @param loan the loan
	 * @param enabled the enabled
	 * @return the list
	 */
	List<LoanApprovalHistorique> findByLoanAndEnabledOrderByApprovalDateDesc(Loan loan,
			Boolean enabled);
}
