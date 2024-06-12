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

import com.acm.utils.models.Asset;

/**
 * The Interface AssetRepository.
 */
@Repository
public interface AssetRepository
		extends JpaRepository<Asset, Long>, QuerydslPredicateExecutor<Asset>,
		CrudRepository<Asset, Long>, PagingAndSortingRepository<Asset, Long> {

}
