/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.acm.utils.models.CollectionInstance;

/**
 * {@link CollectionInstanceRepository} class.
 *
 * @author idridi
 * @since 1.1.9
 */
public interface CollectionInstanceRepository extends JpaRepository<CollectionInstance, Long>,
		QuerydslPredicateExecutor<CollectionInstance>, CrudRepository<CollectionInstance, Long>,
		PagingAndSortingRepository<CollectionInstance, Long> {

}
