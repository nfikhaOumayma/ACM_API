/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.UserDefinedFields;

/**
 * Class provides service dao for {@link UserDefinedFields} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface UserDefinedFieldsRepository extends JpaRepository<UserDefinedFields, Long>,
		QuerydslPredicateExecutor<UserDefinedFields>, CrudRepository<UserDefinedFields, Long>,
		PagingAndSortingRepository<UserDefinedFields, Long> {

	/**
	 * Find by id UDF field ABACUS and enabled row.
	 * 
	 * @author HaythemBenizid
	 * @param idUDFField the id UDF field
	 * @param enabled the enabled
	 * @return the list
	 */
	List<UserDefinedFields> findByIdUDFFieldAndEnabled(Long idUDFField, Boolean enabled);

	/**
	 * Find by id UDF field in and enabled.
	 * 
	 * @author idridi
	 * @param idsUDFFieldAbacus the ids UDF field abacus
	 * @param enabled the enabled
	 * @return the list
	 */
	List<UserDefinedFields> findByIdUDFFieldInAndEnabled(List<Long> idsUDFFieldAbacus,
			Boolean enabled);

	/**
	 * Find by id in and enabled.
	 *
	 * @param ids the ids
	 * @param enabled the enabled
	 * @return the list
	 */
	List<UserDefinedFields> findByIdInAndEnabled(List<Long> ids, Boolean enabled);

	/**
	 * Find by id UDF field is not.
	 *
	 * @param idUDFField the id UDF field
	 * @param fieldType the field type
	 * @return the list
	 */
	@Query("SELECT idUDFListValue FROM UserDefinedFields udfField WHERE  idUDFField != :idUDFField and fieldType = :fieldType")

	List<Long> findIdUDFListValueByIdUDFFieldIsNotAndFieldType(Long idUDFField, Integer fieldType);
}
