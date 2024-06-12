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

import com.acm.utils.models.IncentiveRegistration;
import com.acm.utils.models.IncentiveSettingConstant;

/**
 * Class provides Repository DAO for {@link IncentiveRegistration} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveRegistrationRepository extends JpaRepository<IncentiveRegistration, Long>,
		QuerydslPredicateExecutor<IncentiveRegistration>,
		CrudRepository<IncentiveRegistration, Long>,
		PagingAndSortingRepository<IncentiveRegistration, Long> {

	/**
	 * Find by product id.
	 * 
	 * @author idridi
	 * @param productId the product id
	 * @return the list
	 */
	List<IncentiveRegistration> findByProductId(Long productId);

	/**
	 * Find by ordre greater than.
	 * 
	 * @author idridi
	 * @param ordre the ordre
	 * @return the list
	 */
	List<IncentiveRegistration> findByOrdreGreaterThan(Long ordre);

	/**
	 * Find by role and customer type.
	 * 
	 * @author idridi
	 * @param role the role
	 * @param customerType the customer type
	 * @return the list
	 */
	List<IncentiveRegistration> findByRoleAndCustomerType(String role,
			IncentiveSettingConstant customerType);

}
