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
import org.springframework.stereotype.Repository;

import com.acm.utils.models.CollectionStep;
import com.acm.utils.models.WorkFlowStep;

/**
 * {@link SettingCollectionRepository} class.
 *
 * @author Maher khemissi
 * @since 1.0.10
 */
@Repository
public interface SettingCollectionRepository extends JpaRepository<CollectionStep, Long>,
		QuerydslPredicateExecutor<CollectionStep>, CrudRepository<CollectionStep, Long> {

	/**
	 * Delete by product id.
	 *
	 * @param productId the product id
	 * @param Process the process
	 */
	@Transactional
	void deleteByProductIdAndProcess(Long productId, String Process);

	/**
	 * Find Collection Steps by product id.
	 *
	 * @param productId the product id
	 * @return the list
	 */
	List<WorkFlowStep> findByProductId(Long productId);
}
