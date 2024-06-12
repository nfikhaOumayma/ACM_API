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

import com.acm.utils.models.Customer;

/**
 * Class provides Repo dao for {@link Customer} table.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@Repository
public interface CustomerRepository
		extends JpaRepository<Customer, Long>, QuerydslPredicateExecutor<Customer>,
		CrudRepository<Customer, Long>, PagingAndSortingRepository<Customer, Long> {

	/**
	 * Find by customer id extern.
	 * 
	 * @author HaythemBenizid
	 * @param customerIdExtern the customer id extern
	 * @return the list
	 */
	List<Customer> findByCustomerIdExtern(Long customerIdExtern);

	/**
	 * Find by ib customer id.
	 * 
	 * @author mlamloum
	 * @param ibCustomerId the ib customer id
	 * @return the list
	 */
	List<Customer> findByIbCustomerId(Long ibCustomerId);

	/**
	 * Find by telephone 1.
	 *
	 * @author Ines Dridi
	 * @param mobileNumber the mobile number
	 * @param type the type
	 * @return the list
	 */
	List<Customer> findByTelephone1AndCustomerType(String mobileNumber, String type);

	/**
	 * Find by telephone 2.
	 *
	 * @author Ines Dridi
	 * @param phoneNumber the phone number
	 * @param type the type
	 * @return the list
	 */
	List<Customer> findByTelephone2AndCustomerType(String phoneNumber, String type);

	/**
	 * Find by solidarity name and enabled.
	 *
	 * @author Ines Dridi
	 * @param solidarityname the solidarityname
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Customer> findBySolidarityNameAndEnabled(String solidarityname, Boolean enbled);

	/**
	 * Find by organization name and enabled.
	 *
	 * @author Ines Dridi
	 * @param organizationName the organization name
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Customer> findByOrganizationNameAndEnabled(String organizationName, Boolean enbled);

	/**
	 * Find by meza card status and enabled.
	 *
	 * @author HaythemBenizid
	 * @param mezaCardStatus the meza card status
	 * @param enbled the enbled
	 * @return the list
	 */
	List<Customer> findByMezaCardStatusAndEnabled(String mezaCardStatus, Boolean enbled);

	/**
	 * Count by meza card status.
	 *
	 * @author MoezMhiri
	 * @param status the status
	 * @param enabled the enabled
	 * @return the long
	 */
	Long countByMezaCardStatusAndEnabled(String status, Boolean enabled);

	/**
	 * Find by identity.
	 *
	 * @author idridi
	 * @param identityNumber the identity number
	 * @param type the type
	 * @return the list
	 */
	List<Customer> findByIdentityAndCustomerType(String identityNumber, String type);
	
	/**
	 * Find by register number.
	 *
	 * @param registerNumber the register number
	 * @return the list
	 */
	List<Customer> findByRegisterNumber(String registerNumber);

}
