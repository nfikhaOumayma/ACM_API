/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Customer;
import com.acm.utils.models.CustomerLinksRelationship;

/**
 * Class provides Repo dao for {@link CustomerLinksRelationship} table.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@Repository
public interface CustomerLinksRelationshipRepository
		extends JpaRepository<CustomerLinksRelationship, Long>,
		QuerydslPredicateExecutor<CustomerLinksRelationship>,
		CrudRepository<CustomerLinksRelationship, Long> {
	/**
	 * Delete by customer id.
	 *
	 * @author MoezMhiri
	 * @param customerId the customer id
	 */
	@Transactional
	void deleteByCustomerId(Long customerId);

	/**
	 * Delete by Loan Id And Category.
	 * 
	 * @author YesserSomai
	 * @param idLoan the Loan Id
	 * @param category the Category
	 */
	@Transactional
	void deleteByIdLoanAndCategory(Long idLoan, String category);

	/**
	 * Delete by Customer Id And Category.
	 * 
	 * @author MoezMhiri
	 * @param customerId the Customer Id
	 * @param category the Category
	 */
	@Transactional
	void deleteByCustomerIdAndCategory(Long customerId, String category);

	/**
	 * Find all members by customer members id.
	 * 
	 * @author Salmen Fatnassi
	 * @param customerMemberId the customer member id
	 * @param category the category
	 * @return the list
	 */
	@Query("SELECT clr FROM  CustomerLinksRelationship clr WHERE clr.customerId in (SELECT DISTINCT c.customerId FROM CustomerLinksRelationship c where c.member.id=:customerMemberId) AND CATEGORY=:category ")
	List<CustomerLinksRelationship> findAllMembersByCustomerMembersId(
			@Param("customerMemberId") Long customerMemberId, @Param("category") String category);

	/**
	 * Find by id loan and category and member and enabled.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @param category the category
	 * @param membre the membre
	 * @param enbled the enbled
	 * @return the list
	 */
	List<CustomerLinksRelationship> findByIdLoanAndCategoryAndMemberAndEnabled(Long idLoan,
			String category, Customer membre, Boolean enbled);
}
