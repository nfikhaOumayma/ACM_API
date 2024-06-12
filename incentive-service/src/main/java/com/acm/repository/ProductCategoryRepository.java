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

import com.acm.utils.models.ProductCategory;

/**
 * Class provides Repository DAO for {@link ProductCategory} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface ProductCategoryRepository
		extends JpaRepository<ProductCategory, Long>, QuerydslPredicateExecutor<ProductCategory>,
		CrudRepository<ProductCategory, Long>, PagingAndSortingRepository<ProductCategory, Long> {

}
