/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.WorkflowStepUdfGroupeDTO;

/**
 * The Interface WorkflowStepUdfGroupeService.
 */
public interface WorkflowStepUdfGroupeService {

	/**
	 * Save.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the workflow step udf groupe DTO
	 */
	WorkflowStepUdfGroupeDTO save(WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO);

	/**
	 * Find.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the list
	 */
	List<WorkflowStepUdfGroupeDTO> find(WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO);

	/**
	 * Save all.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the list
	 */
	List<WorkflowStepUdfGroupeDTO> saveAll(List<WorkflowStepUdfGroupeDTO> workflowStepUdfGroupeDTO);

	/**
	 * Find udf groups by step id.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the list
	 */
	List<UserDefinedFieldGroupDTO> findUdfGroupsByStepId(
			WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO);

	/**
	 * Find udf fields by step id.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @param udfGroupId the udf group id
	 * @return the list
	 */
	List<UserDefinedFieldsDTO> findUdfFieldsByStepId(
			WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO, Long udfGroupId);
}
