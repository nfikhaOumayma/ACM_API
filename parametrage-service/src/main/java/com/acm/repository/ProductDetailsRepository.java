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

import com.acm.utils.models.ProductDetails;

/**
 * {@link ProductDetailsRepository} class.
 *
 * @author MoezMhiri
 * @since 1.0.9
 */
@Repository
public interface ProductDetailsRepository extends JpaRepository<ProductDetails, Long>,
		CrudRepository<ProductDetails, Long>, PagingAndSortingRepository<ProductDetails, Long>,
		QuerydslPredicateExecutor<ProductDetails> {
}