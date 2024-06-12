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

import com.acm.utils.models.RenewalCondition;

import feign.Param;

/**
 * {@link RenewalConditionRepository} class.
 *
 * @author idridi
 * @since 1.0.8
 */
@Repository
public interface RenewalConditionRepository
		extends JpaRepository<RenewalCondition, Long>, QuerydslPredicateExecutor<RenewalCondition>,
		CrudRepository<RenewalCondition, Long>, PagingAndSortingRepository<RenewalCondition, Long> {

	/**
	 * Find by year and last paid amount.
	 * 
	 * @author idridi
	 * @param renewalYear the renewal year
	 * @param lastPaidAmount the last paid amount
	 * @return the list
	 */
	@Query("SELECT renewalConditionSetting FROM RenewalCondition renewalConditionSetting WHERE year = :renewalYear AND min_amount <= :lastPaidAmount And max_amount >= :lastPaidAmount")
	List<RenewalCondition> findByYearAndLastPaidAmount(@Param("renewalYear") Integer renewalYear,
			@Param("lastPaidAmount") Long lastPaidAmount);

	/**
	 * Find by max year and last paid amount.
	 * 
	 * @author idridi
	 * @param lastPaidAmount the last paid amount
	 * @return the list
	 */
	@Query("  SELECT renewalConditionSetting FROM RenewalCondition renewalConditionSetting WHERE min_amount <= :lastPaidAmount AND max_amount >= :lastPaidAmount AND year = (SELECT max(year) FROM RenewalCondition)")
	List<RenewalCondition> findByMaxYearAndLastPaidAmount(
			@Param("lasPaidAmount") Long lastPaidAmount);

}
