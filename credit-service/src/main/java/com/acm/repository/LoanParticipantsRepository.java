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

import com.acm.utils.models.LoanParticipants;

/**
 * Class provides service dao for {@link LoanParticipants} table.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
@Repository
public interface LoanParticipantsRepository
		extends JpaRepository<LoanParticipants, Long>, QuerydslPredicateExecutor<LoanParticipants>,
		CrudRepository<LoanParticipants, Long>, PagingAndSortingRepository<LoanParticipants, Long> {

	/**
	 * Find by id loan and username and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @param username the username
	 * @param enabled the enabled
	 * @return the list
	 */
	List<LoanParticipants> findByIdLoanAndUsernameAndEnabled(Long idLoan, String username,
			Boolean enabled);
}
