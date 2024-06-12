/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Expenses;

/**
 * {@link ExpensesRepository} class.
 *
 * @author Ines Dridi
 * @since 1.1.3
 */
@Repository
public interface ExpensesRepository
		extends JpaRepository<Expenses, Long>, CrudRepository<Expenses, Long>,
		QuerydslPredicateExecutor<Expenses>, PagingAndSortingRepository<Expenses, Long> {

	/**
	 * Count by status.
	 * 
	 * @author ManelLamloum
	 * @param status the status
	 * @return the long
	 */
	Long countByStatut(Integer status);

}
