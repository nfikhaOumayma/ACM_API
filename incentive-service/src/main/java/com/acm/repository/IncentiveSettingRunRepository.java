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

import com.acm.utils.models.IncentiveSettingRun;

/**
 * Class provides Repository DAO for {@link IncentiveSettingRun} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveSettingRunRepository extends JpaRepository<IncentiveSettingRun, Long>,
		QuerydslPredicateExecutor<IncentiveSettingRun>, CrudRepository<IncentiveSettingRun, Long>,
		PagingAndSortingRepository<IncentiveSettingRun, Long> {

	/**
	 * Find by code.
	 * 
	 * @author HaythemBenizid
	 * @param code the code
	 * @return the list
	 */
	List<IncentiveSettingRun> findByCode(String code);

}
