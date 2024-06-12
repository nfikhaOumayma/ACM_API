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

import com.acm.utils.models.SettingTypeRisk;

@Repository
public interface SettingRiskTypeRepository extends JpaRepository<SettingTypeRisk, Long>,
		QuerydslPredicateExecutor<SettingTypeRisk>, CrudRepository<SettingTypeRisk, Long>,
		PagingAndSortingRepository<SettingTypeRisk, Long> {

	/**
	 * Find by enabled.
	 *
	 * @author kouali
	 * @param b the b
	 * @return the list
	 */
	List<SettingTypeRisk> findByEnabled(boolean b);

}
