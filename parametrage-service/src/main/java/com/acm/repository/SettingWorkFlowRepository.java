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

import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.models.SettingJournalEntryType;
import com.acm.utils.models.WorkFlowStep;

/**
 * {@link SettingWorkFlowRepository} class.
 *
 * @author yesser somai
 * @since 1.0.10
 */
@Repository
public interface SettingWorkFlowRepository extends JpaRepository<WorkFlowStep, Long>,
		QuerydslPredicateExecutor<WorkFlowStep>, CrudRepository<WorkFlowStep, Long> {

	/**
	 * Delete by product id.
	 *
	 * @param productId the product id
	 * @param Process the process
	 */
	@Transactional
	void deleteByProductIdAndProcess(Long productId, String Process);

	/**
	 * Find WorkFlow Steps by product id.
	 *
	 * @param productId the product id
	 * @return the list
	 */
	List<WorkFlowStep> findByProductId(Long productId);

	/**
	 * Find by journal entry types.
	 *
	 * @param listForFilter the list for filter
	 * @return the list
	 */
	List<WorkFlowStepDTO> findByJournalEntryTypes(List<SettingJournalEntryType> listForFilter);

	/**
	 * Find by enabled and process and product id.
	 *
	 * @param b the b
	 * @param string the string
	 * @param id the id
	 * @return the list
	 */
	List<WorkFlowStep> findByEnabledAndProcessAndProductId(boolean b, String string, Long id);

}
