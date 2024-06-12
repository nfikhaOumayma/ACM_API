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

import com.acm.utils.models.IncentiveSettingConstant;

/**
 * Class provides Repository DAO for {@link IncentiveSettingConstant} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveSettingConstantRepository
		extends JpaRepository<IncentiveSettingConstant, Long>,
		QuerydslPredicateExecutor<IncentiveSettingConstant>,
		CrudRepository<IncentiveSettingConstant, Long>,
		PagingAndSortingRepository<IncentiveSettingConstant, Long> {

	/**
	 * Find by category in and enabled.
	 * 
	 * @author idridi
	 * @param categories the categories
	 * @param enabled the enabled
	 * @return the list
	 */
	List<IncentiveSettingConstant> findByCategoryInAndEnabled(List<String> categories,
			Boolean enabled);

}
