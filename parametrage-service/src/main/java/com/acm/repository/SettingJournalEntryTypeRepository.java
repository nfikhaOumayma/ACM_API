/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SettingJournalEntryType;

/**
 * The Interface SettingJournalEntryTypeRepository.
 */
@Repository
public interface SettingJournalEntryTypeRepository
		extends JpaRepository<SettingJournalEntryType, Long>,
		QuerydslPredicateExecutor<SettingJournalEntryType>,
		CrudRepository<SettingJournalEntryType, Long>,
		PagingAndSortingRepository<SettingJournalEntryType, Long> {

	/**
	 * Find journal entry types in carrier tbl.
	 *
	 * @param id the id
	 * @return the long
	 */
	@Query(value = " select  count (1) from ACM_SETTING_JOURNAL_ENTRY_TYPE_ACM_WORKFLOW_STEP assoc,ACM_WORKFLOW_STEP workFlow "
			+ "where workFlow.ID_ACM_WORKFLOW_STEP = assoc.ID_ACM_WORKFLOW_STEP "
			+ "and assoc.ID_ACM_SETTING_JOURNAL_ENTRY = :id and workFlow.ACM_ENABLED=1 and workFlow.PROCESS =  'NEW_APPLICATION'",
			nativeQuery = true)
	Long findJournalEntryTypesInCarrierTbl(Long id);
	
	/**
	 * Find journal entry types in workflow step.
	 *
	 * @param id the id
	 * @return the long
	 */
	@Query(value = " select  count (1) from ACM_SETTING_JOURNAL_ENTRY_TYPE_ACM_WORKFLOW_STEP workflow,ACM_SETTING_JOURNAL_ENTRY_TYPE type "
			+ "where type.ID_ACM_SETTING_JOURNAL_ENTRY = workflow.ID_ACM_SETTING_JOURNAL_ENTRY "
			+ "and workflow.ID_ACM_SETTING_JOURNAL_ENTRY = :id and type.ACM_ENABLED=1",
			nativeQuery = true)
	Long findJournalEntryTypesInWorkflowStep(Long id);
}
