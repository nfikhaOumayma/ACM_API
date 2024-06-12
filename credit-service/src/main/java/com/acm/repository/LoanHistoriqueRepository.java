/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Loan;
import com.acm.utils.models.LoanHistorique;

/**
 * Class provides service dao for {@link LoanHistorique} table.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Repository
public interface LoanHistoriqueRepository extends JpaRepository<LoanHistorique, Long>,
		CrudRepository<LoanHistorique, Long>, PagingAndSortingRepository<LoanHistorique, Long> {

	/**
	 * Find Historiques by loan ID default order by dateUpdate DESC.
	 * 
	 * @author HaythemBenizid
	 * @param loan the loan
	 * @param techniqueInformation the techniqueInformation
	 * @return the list
	 */
	List<LoanHistorique> findByLoanAndTechniqueInformationOrderByDateUpdateDesc(Loan loan,
			Boolean techniqueInformation);
}
