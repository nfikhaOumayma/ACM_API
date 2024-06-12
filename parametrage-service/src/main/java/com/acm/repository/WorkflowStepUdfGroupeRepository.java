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

import com.acm.utils.models.WorkflowStepUdfGroupe;

/**
 * The Interface WorkflowStepUdfGroupeRepository.
 */
@Repository
public interface WorkflowStepUdfGroupeRepository extends JpaRepository<WorkflowStepUdfGroupe, Long>,
		CrudRepository<WorkflowStepUdfGroupe, Long>,
		QuerydslPredicateExecutor<WorkflowStepUdfGroupe>,
		PagingAndSortingRepository<WorkflowStepUdfGroupe, Long> {

	/**
	 * Find by id workflow step and id user defined fields is null.
	 *
	 * @param idWorkflowStep the id workflow step
	 * @return the list
	 */
	List<WorkflowStepUdfGroupe> findByIdWorkflowStepAndIdUserDefinedFieldsIsNull(
			Long idWorkflowStep);

	/**
	 * Find by id collection step and id user defined fields is null.
	 *
	 * @param idCollectionStep the id collection step
	 * @return the list
	 */
	List<WorkflowStepUdfGroupe> findByIdCollectionStepAndIdUserDefinedFieldsIsNull(
			Long idCollectionStep);

	/**
	 * Find by id collection step and id user defined field group.
	 *
	 * @param idCollectionStep the id collection step
	 * @param idUserDefinedFieldGroup the id user defined field group
	 * @return the list
	 */
	List<WorkflowStepUdfGroupe> findByIdCollectionStepAndIdUserDefinedFieldGroup(
			Long idCollectionStep, Long idUserDefinedFieldGroup);

	/**
	 * Find by id workflow step and id user defined field group and id user defined fields is not
	 * null.
	 *
	 * @param idWorkflowStep the id workflow step
	 * @param idUserDefinedFieldGroup the id user defined field group
	 * @return the list
	 */
	List<WorkflowStepUdfGroupe> findByIdWorkflowStepAndIdUserDefinedFieldGroupAndIdUserDefinedFieldsIsNotNull(
			Long idWorkflowStep, Long idUserDefinedFieldGroup);

}
