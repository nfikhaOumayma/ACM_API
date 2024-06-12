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

import com.acm.utils.models.IncentiveOperation;

/**
 * Class provides Repository DAO for {@link IncentiveOperation} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveOperationRepository extends JpaRepository<IncentiveOperation, Long>,
		QuerydslPredicateExecutor<IncentiveOperation>, CrudRepository<IncentiveOperation, Long>,
		PagingAndSortingRepository<IncentiveOperation, Long> {

	/**
	 * Find by product id.
	 * 
	 * @author idridi
	 * @param productId the product id
	 * @return the list
	 */
	List<IncentiveOperation> findByProductId(Long productId);

	/**
	 * Find by ordre greater than.
	 * 
	 * @author idridi
	 * @param ordre the ordre
	 * @return the list
	 */
	List<IncentiveOperation> findByOrdreGreaterThan(Long ordre);

	/**
	 * Find by role.
	 * 
	 * @author idridi
	 * @param role the role
	 * @return the list
	 */
	List<IncentiveOperation> findByRole(String role);

}
