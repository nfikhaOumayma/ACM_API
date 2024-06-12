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

import com.acm.utils.models.AddressHistorique;

/**
 * Class provides service dao for {@link AddressHistorique} table.
 *
 * @author YesserSomai
 * @since 1.0.14
 */
@Repository
public interface AddressHistoriqueRepository extends JpaRepository<AddressHistorique, Long>,
		QuerydslPredicateExecutor<AddressHistorique>, CrudRepository<AddressHistorique, Long>,
		PagingAndSortingRepository<AddressHistorique, Long> {
}