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

import com.acm.utils.models.IncentiveSettingBranchProdLevel;

/**
 * Class provides Repository DAO for {@link IncentiveSettingBranchProdLevel} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveSettingBranchProdLevelRepository
		extends JpaRepository<IncentiveSettingBranchProdLevel, Long>,
		QuerydslPredicateExecutor<IncentiveSettingBranchProdLevel>,
		CrudRepository<IncentiveSettingBranchProdLevel, Long>,
		PagingAndSortingRepository<IncentiveSettingBranchProdLevel, Long> {

}
