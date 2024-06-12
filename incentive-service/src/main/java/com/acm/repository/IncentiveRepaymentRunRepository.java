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

import com.acm.utils.models.IncentiveRepaymentRun;

/**
 * Class provides Repository DAO for {@link IncentiveRepaymentRun} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveRepaymentRunRepository extends JpaRepository<IncentiveRepaymentRun, Long>,
		QuerydslPredicateExecutor<IncentiveRepaymentRun>,
		CrudRepository<IncentiveRepaymentRun, Long> {

	/**
	 * Run procedure calculate incentive.
	 *
	 * @author HaythemBenizid
	 * @return the integer
	 */
	@Procedure(procedureName = "ACM_PROC_INCENTIVE_REPAYMENT_INSUANCE")
	Integer runProcedureCalculateIncentive();

	/**
	 * Gets {@link IncentiveRepaymentRun} by year and month.
	 * 
	 * @author HaythemBenizid
	 * @param year the year
	 * @param month the month
	 * @return the by year and month
	 */
	@Query("SELECT incentive FROM IncentiveRepaymentRun incentive WHERE year(incentive.runDate) = ?1 AND month(incentive.runDate) = ?2")
	List<IncentiveRepaymentRun> getByYearAndMonth(int year, int month);

	/**
	 * Gets the run year.
	 * 
	 * @author HaythemBenizid
	 * @return the run year
	 */
	@Query(value = "SELECT distinct DATENAME(YEAR,RUN_DATE) as YEAR FROM ACM_INCENTIVE_RUN_REPAYMENT",
			nativeQuery = true)
	List<Integer> getRunYear();

	/**
	 * Gets the run month.
	 * 
	 * @author HaythemBenizid
	 * @return the run month
	 */
	@Query(value = "SELECT distinct MONTH( RUN_DATE ) as MONTH_NB FROM ACM_INCENTIVE_RUN_REPAYMENT",
			nativeQuery = true)
	List<Integer> getRunMonth();
}
