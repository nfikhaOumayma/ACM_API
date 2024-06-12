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

import com.acm.utils.models.Product;

/**
 * {@link ProductRepository} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Repository
public interface ProductRepository
		extends JpaRepository<Product, Long>, CrudRepository<Product, Long>,
		PagingAndSortingRepository<Product, Long>, QuerydslPredicateExecutor<Product> {

	/**
	 * Find by code.
	 *
	 * @param code the code
	 * @return the list
	 */
	List<Product> findByCode(String code);

	/**
	 * Find by product id abacus in.
	 *
	 * @author idridi
	 * @param ids the ids
	 * @return the list
	 */
	List<Product> findByProductIdAbacusIn(List<Long> ids);
}
