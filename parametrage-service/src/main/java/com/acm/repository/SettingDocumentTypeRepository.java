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

import com.acm.utils.models.SettingDocumentType;

/**
 * Class provides Repo dao for {@link SettingDocumentType} table.
 * 
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Repository
public interface SettingDocumentTypeRepository extends JpaRepository<SettingDocumentType, Long>,
		QuerydslPredicateExecutor<SettingDocumentType>, CrudRepository<SettingDocumentType, Long>,
		PagingAndSortingRepository<SettingDocumentType, Long> {

	/**
	 * Find by ctegory.
	 *
	 * @param category the category
	 * @return the setting document type
	 */
	List<SettingDocumentType> findByCategorie(Integer category);

}
