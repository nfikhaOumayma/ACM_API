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

import com.acm.utils.models.IncentiveRepayment;

/**
 * Class provides Repository DAO for {@link IncentiveRepayment} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveRepaymentRepository extends JpaRepository<IncentiveRepayment, Long>,
		QuerydslPredicateExecutor<IncentiveRepayment>, CrudRepository<IncentiveRepayment, Long>,
		PagingAndSortingRepository<IncentiveRepayment, Long> {

	/**
	 * Find by role and active customer id and productivity id and risk level id.
	 * 
	 * @author idridi
	 * @param role the role
	 * @param activeCustomerId the active customer id
	 * @param productivityId the productivity id
	 * @param riskLevelId the risk level id
	 * @return the list
	 */
	List<IncentiveRepayment> findByRoleAndActiveCustomerIdAndProductivityIdAndRiskLevelId(
			String role, Long activeCustomerId, Long productivityId, Long riskLevelId);

	/**
	 * Find by ordre greater than.
	 * 
	 * @author idridi
	 * @param ordre the ordre
	 * @return the list
	 */
	List<IncentiveRepayment> findByOrdreGreaterThan(Long ordre);

}
