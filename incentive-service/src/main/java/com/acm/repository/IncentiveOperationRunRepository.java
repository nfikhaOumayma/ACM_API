/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.jpa.repository.query.Procedure;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.IncentiveOperationRun;

/**
 * Class provides Repository DAO for {@link IncentiveOperationRun} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveOperationRunRepository extends JpaRepository<IncentiveOperationRun, Long>,
		QuerydslPredicateExecutor<IncentiveOperationRun>,
		CrudRepository<IncentiveOperationRun, Long> {

	/**
	 * Run procedure calculate incentive.
	 *
	 * @author HaythemBenizid
	 * @return the integer
	 */
	@Procedure(procedureName = "ACM_PROC_INCENTIVE_OPERATION")
	Integer runProcedureCalculateIncentive();

	/**
	 * Gets {@link IncentiveOperationRun} by year and month.
	 * 
	 * @author HaythemBenizid
	 * @param year the year
	 * @param month the month
	 * @return the by year and month
	 */
	@Query("SELECT incentive FROM IncentiveOperationRun incentive WHERE year(incentive.runDate) = ?1 AND month(incentive.runDate) = ?2 ORDER BY incentive.branch ASC")
	List<IncentiveOperationRun> getByYearAndMonth(int year, int month);

	/**
	 * Gets the run year.
	 * 
	 * @author HaythemBenizid
	 * @return the run year
	 */
	@Query(value = "SELECT DISTINCT DATENAME(YEAR,RUN_DATE) as YEAR FROM ACM_INCENTIVE_RUN_OPERATION",
			nativeQuery = true)
	List<Integer> getRunYear();

	/**
	 * Gets the run month.
	 * 
	 * @author HaythemBenizid
	 * @return the run month
	 */
	@Query(value = "SELECT DISTINCT MONTH( RUN_DATE ) as MONTH_NB FROM ACM_INCENTIVE_RUN_OPERATION",
			nativeQuery = true)
	List<Integer> getRunMonth();
}
