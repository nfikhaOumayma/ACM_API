/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.CustomerContact;

/**
 * Class provides Repo dao for {@link CustomerContact} table.
 *
 * @author Salmen Fatnassi
 * @since 0.17.0
 */
@Repository
public interface CustomerContactRepository
		extends JpaRepository<CustomerContact, Long>, QuerydslPredicateExecutor<CustomerContact>,
		CrudRepository<CustomerContact, Long>, PagingAndSortingRepository<CustomerContact, Long> {

}
