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

import com.acm.utils.models.SettingMotifRejets;

/**
 * Class provides Repo dao for {@link SettingMotifRejets} table.
 * 
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Repository
public interface SettingMotifRejetsRepository extends JpaRepository<SettingMotifRejets, Long>,
		QuerydslPredicateExecutor<SettingMotifRejets>, CrudRepository<SettingMotifRejets, Long>,
		PagingAndSortingRepository<SettingMotifRejets, Long> {

}
