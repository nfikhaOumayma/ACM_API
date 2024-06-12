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

import com.acm.utils.models.SettingLevel;
import com.acm.utils.models.SettingLevelProcess;

/**
 * Class provides service dao for {@link SettingLevelProcess} table.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
@Repository
public interface SettingLevelProcessRepository extends JpaRepository<SettingLevelProcess, Long>,
		CrudRepository<SettingLevelProcess, Long>, QuerydslPredicateExecutor<SettingLevelProcess>,
		PagingAndSortingRepository<SettingLevelProcess, Long> {

	/**
	 * Find by setting level.
	 * 
	 * @author HaythemBenizid
	 * @param settingLevel the setting level
	 * @return the list
	 */
	List<SettingLevelProcess> findBySettingLevel(SettingLevel settingLevel);

}
