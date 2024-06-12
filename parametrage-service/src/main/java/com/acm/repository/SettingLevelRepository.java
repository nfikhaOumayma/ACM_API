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

import com.acm.utils.models.SettingLevel;

/**
 * Class provides service dao for {@link SettingLevel} table.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
@Repository
public interface SettingLevelRepository
		extends JpaRepository<SettingLevel, Long>, QuerydslPredicateExecutor<SettingLevel>,
		CrudRepository<SettingLevel, Long>, PagingAndSortingRepository<SettingLevel, Long> {

}
