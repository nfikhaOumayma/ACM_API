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

import com.acm.utils.models.AMLData;

/**
 * Class provides service dao for {@link AMLData} table.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
@Repository
public interface AMLDataRepository
		extends JpaRepository<AMLData, Long>, QuerydslPredicateExecutor<AMLData>,
		CrudRepository<AMLData, Long>, PagingAndSortingRepository<AMLData, Long> {

	/**
	 * Find by name.
	 * 
	 * @author HaythemBenizid
	 * @param name the name
	 * @return the list
	 */
	List<AMLData> findByName(String name);

	/**
	 * Find by identity number like.
	 *
	 * @author HaythemBenizid
	 * @param name the name
	 * @param identityNumber the identity number
	 * @return the list
	 */
	List<AMLData> findByNameAndIdentityNumberLike(String name, String identityNumber);

}
