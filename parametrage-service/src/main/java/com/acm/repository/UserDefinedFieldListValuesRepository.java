/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.UserDefinedFieldListValues;

/**
 * Class provides service dao for {@link UserDefinedFieldListValues} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface UserDefinedFieldListValuesRepository
		extends JpaRepository<UserDefinedFieldListValues, Long>,
		QuerydslPredicateExecutor<UserDefinedFieldListValues>,
		CrudRepository<UserDefinedFieldListValues, Long>,
		PagingAndSortingRepository<UserDefinedFieldListValues, Long> {

	/**
	 * Find by id UDF list value and enabled.
	 * 
	 * @author yesser somai
	 * @param idUDFListValue the id UDF list value
	 * @param enabled the enabled
	 * @return the list
	 */
	List<UserDefinedFieldListValues> findByIdUDFListValueAndEnabled(Long idUDFListValue,
			Boolean enabled);

	/**
	 * Find by table abacus name and id UDF list link is not.
	 *
	 * @param tableAbacusName the table abacus name
	 * @param idUDFListLink the id UDF list link
	 * @return the list
	 */
	List<UserDefinedFieldListValues> findByTableAbacusNameAndIdUDFListLinkIsNot(
			String tableAbacusName, Long idUDFListLink);

	/**
	 * Delete by id UDF list in or id UDF list link in.
	 *
	 * @param idUDFListValues the id UDF list values
	 * @param idUDFListValues2 the id UDF list values 2
	 */
	@Transactional
	void deleteByIdUDFListInOrIdUDFListLinkIn(List<Long> idUDFListValues,
			List<Long> idUDFListValues2);
}
