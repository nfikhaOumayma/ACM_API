/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.AcmEnvironnement;

/**
 * Class provides service dao for {@link AcmEnvironnement} table (SHARED REPOSITORY).
 *
 * @author HaythemBenizid
 * @since 0.11.0
 */
@Repository
public interface AcmEnvironnementRepository extends JpaRepository<AcmEnvironnement, Long>,
		CrudRepository<AcmEnvironnement, Long>, PagingAndSortingRepository<AcmEnvironnement, Long>,
		QuerydslPredicateExecutor<AcmEnvironnement> {

	/**
	 * Find by key.
	 * 
	 * @author HaythemBenizid
	 * @param key the key
	 * @return the acm environnement
	 */
	List<AcmEnvironnement> findByKey(String key);

	/**
	 * Find by category.
	 * 
	 * @author Ines Dridi
	 * @param category the category
	 * @return the list
	 */
	List<AcmEnvironnement> findByCategory(String category);

	/**
	 * Find by key in.
	 * 
	 * @author Ines Dridi
	 * @param keys the keys
	 * @return the list
	 */
	List<AcmEnvironnement> findByKeyIn(List<String> keys);

}
