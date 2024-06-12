/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.Item;

/**
 * The Interface ItemRepository.
 */
@Repository
public interface ItemRepository extends JpaRepository<Item, Long>, QuerydslPredicateExecutor<Item>,
		CrudRepository<Item, Long> {

	/**
	 * Find by actual step.
	 *
	 * @param idStep the id step
	 * @return the item
	 */
	Item findByActualStep(Long idStep);

}
