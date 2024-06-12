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

import com.acm.utils.models.IncentiveSetting;

/**
 * Class provides Repository DAO for {@link IncentiveSetting} table.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Repository
public interface IncentiveSettingRepository
		extends JpaRepository<IncentiveSetting, Long>, QuerydslPredicateExecutor<IncentiveSetting>,
		CrudRepository<IncentiveSetting, Long>, PagingAndSortingRepository<IncentiveSetting, Long> {

	/**
	 * Find by ordre greater than.
	 * 
	 * @author idridi
	 * @param ordre the ordre
	 * @return the list
	 */
	List<IncentiveSetting> findByOrdreGreaterThan(Long ordre);

}
