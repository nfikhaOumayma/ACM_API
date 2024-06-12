/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.query.Procedure;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SettingListValues;

/**
 * Class provides service dao for {@link SettingListValues} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface SettingListValuesRepository extends JpaRepository<SettingListValues, Long>,
		QuerydslPredicateExecutor<SettingListValues>, CrudRepository<SettingListValues, Long>,
		PagingAndSortingRepository<SettingListValues, Long> {

	/**
	 * Update branches description.
	 * 
	 * @author idridi
	 * @return the integer
	 */
	@Procedure(procedureName = "ACM_PROCEDURE_UPDATE_BRANCHES")
	Integer updateBranchesDescription();

	/**
	 * Find by table abacus name and id extern.
	 *
	 * @author kouali
	 * @param string the string
	 * @param long1 the long 1
	 * @return the setting list values
	 */
	SettingListValues findByTableAbacusNameAndIdExtern(String string, Long long1);

	/**
	 * Find by table abacus name.
	 *
	 * @author kouali
	 * @param string the string
	 * @return the list
	 */
	List<SettingListValues> findByTableAbacusName(String string);
}
