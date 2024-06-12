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

import com.acm.utils.models.UserDefinedFieldGroup;

/**
 * Class provides service dao for {@link UserDefinedFieldGroup} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface UserDefinedFieldGroupRepository extends JpaRepository<UserDefinedFieldGroup, Long>,
		QuerydslPredicateExecutor<UserDefinedFieldGroup>,
		CrudRepository<UserDefinedFieldGroup, Long>,
		PagingAndSortingRepository<UserDefinedFieldGroup, Long> {

	/**
	 * Find by id UD group ABACUS and enabled row.
	 * 
	 * @author HaythemBenizid
	 * @param idUDGroupAbacus the id UD group ABACUS
	 * @param enabled the enabled
	 * @return the list
	 */
	List<UserDefinedFieldGroup> findByIdUDGroupAbacusAndEnabled(Long idUDGroupAbacus,
			Boolean enabled);

	/**
	 * Find by id in.
	 *
	 * @param ids the ids
	 * @param enabled the enabled
	 * @return the list
	 */
	List<UserDefinedFieldGroup> findByIdInAndEnabled(List<Long> ids, Boolean enabled);
}
