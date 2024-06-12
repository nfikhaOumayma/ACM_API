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

import com.acm.utils.models.SettingRequiredStep;

/**
 * Class provides service dao for {@link SettingRequiredStep} table.
 *
 * @author YesserSomai
 * @since 1.0.3
 */
@Repository
public interface SettingRequiredStepRepository extends JpaRepository<SettingRequiredStep, Long>,
		QuerydslPredicateExecutor<SettingRequiredStep>, CrudRepository<SettingRequiredStep, Long>,
		PagingAndSortingRepository<SettingRequiredStep, Long> {

}
