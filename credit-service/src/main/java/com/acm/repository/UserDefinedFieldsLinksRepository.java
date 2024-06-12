/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.UserDefinedFields;
import com.acm.utils.models.UserDefinedFieldsLinks;

/**
 * Class provides service dao for {@link UserDefinedFieldsLinks} table.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Repository
public interface UserDefinedFieldsLinksRepository
		extends JpaRepository<UserDefinedFieldsLinks, Long>,
		QuerydslPredicateExecutor<UserDefinedFieldsLinks>,
		CrudRepository<UserDefinedFieldsLinks, Long>,
		PagingAndSortingRepository<UserDefinedFieldsLinks, Long> {

	/**
	 * Delete by loan id.
	 *
	 * @author MoezMhiri
	 * @param elementId the element id
	 * @param category the category
	 */
	@Transactional
	void deleteByElementIdAndCategory(Long elementId, String category);

	/**
	 * Delete by Loan id and id abacus UDF link is null and surveys id is null.
	 *
	 * @author YesserSomai
	 * @param elementId the element id
	 * @param category the category
	 */
	@Transactional
	void deleteByElementIdAndCategoryAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(Long elementId,
			String category);

	/**
	 * Find User Defined Fields by surveys id.
	 *
	 * @author YesserSomai
	 * @param surveysId the surveys id
	 * @return the list of User Defined Fields Links by surveys id
	 */
	List<UserDefinedFieldsLinks> findBySurveysId(Long surveysId);

	/**
	 * Find by element id and category and user defined fields in.
	 *
	 * @param elementId the element id
	 * @param category the category
	 * @param userDefinedFields the user defined fields
	 * @return the list
	 */
	List<UserDefinedFieldsLinks> findByElementIdAndCategoryAndUserDefinedFieldsIn(Long elementId,
			String category, List<UserDefinedFields> userDefinedFields);

	/**
	 * Delete by element id and category and user defined fields in.
	 *
	 * @param elementId the element id
	 * @param category the category
	 * @param idAbacusUdfFields the id abacus udf fields
	 */
	@Modifying
	@Transactional
	@Query(value = "DELETE link FROM acm_udf_link link INNER JOIN acm_udf_field field ON link.ID_ACM_UDF_FIELD  = field.ID_ACM_UDF_FIELD"
			+ " WHERE link.element_id = :elementId AND link.category = :category AND field.id_abacus_udf_field IN :idAbacusUdfFields",
			nativeQuery = true)
	void deleteByElementIdAndCategoryAndUserDefinedFieldsIn(@Param("elementId") Long elementId,
			@Param("category") String category,
			@Param("idAbacusUdfFields") List<Long> idAbacusUdfFields);

	/**
	 * Find top by element id and category order by index group.
	 *
	 * @param elementId the element id
	 * @param category the category
	 * @return the integer
	 */
	@Query("SELECT distinct l.indexGroup FROM UserDefinedFieldsLinks l   WHERE  l.elementId = :elementId AND  l.category = :category ORDER BY  l.indexGroup DESC")
	List<Long> findIndexGroupByElementIdAndCategoryOrderByIndexGroupDesc(Long elementId,
			String category);

}
