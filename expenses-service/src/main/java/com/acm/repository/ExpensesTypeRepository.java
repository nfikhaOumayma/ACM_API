/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.acm.utils.models.ExpensesType;

/**
 * {@link ExpensesTypeRepository} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@Repository
public interface ExpensesTypeRepository extends JpaRepository<ExpensesType, Long>,
		CrudRepository<ExpensesType, Long>, QuerydslPredicateExecutor<ExpensesType> {

	/**
	 * Update document name.
	 *
	 * @param documentLabel the document label
	 * @param documentID the document ID
	 */
	@Modifying
	@Transactional
	@Query("update ExpensesType set documentLibel= :documentLabel where documentID= :documentID")
	void updateDocumentName(@Param("documentLabel") String documentLabel,
			@Param("documentID") Long documentID);

}
