/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Address;

/**
 * Class provides service dao for {@link Address} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface AddressRepository
		extends JpaRepository<Address, Long>, QuerydslPredicateExecutor<Address>,
		CrudRepository<Address, Long>, PagingAndSortingRepository<Address, Long> {
	/**
	 * Delete by customer id.
	 *
	 * @author MoezMhiri
	 * @param customerId the customer id
	 */
	@Transactional
	void deleteByCustomerId(Long customerId);

	/**
	 * Delete by customer id and id address abacus.
	 * 
	 * @author HaythemBenizid
	 * @param customerId the customer id
	 * @param idAddressAbacus the id address abacus
	 */
	@Transactional
	void deleteByCustomerIdAndIdAddressAbacus(Long customerId, Long idAddressAbacus);
}
