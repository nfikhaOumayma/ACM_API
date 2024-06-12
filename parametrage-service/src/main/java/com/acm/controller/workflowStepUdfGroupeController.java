/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.WorkflowStepUdfGroupeService;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.WorkflowStepUdfGroupeDTO;

/**
 * The Class workflowStepUdfGroupeController.
 */
@RestController
@RequestMapping("/workflow_udf_groupe")
public class workflowStepUdfGroupeController {

	/** The workflow step udf groupe service. */
	@Autowired
	private WorkflowStepUdfGroupeService workflowStepUdfGroupeService;

	/**
	 * Creates the.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the workflow step udf groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create")
	public WorkflowStepUdfGroupeDTO create(
			@RequestBody WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO)
			throws ResourcesNotFoundException {

		return workflowStepUdfGroupeService.save(workflowStepUdfGroupeDTO);
	}

	/**
	 * Creates the all.
	 *
	 * @param workflowStepUdfGroupeDTOs the workflow step udf groupe DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/createAll")
	public List<WorkflowStepUdfGroupeDTO> createAll(
			@RequestBody List<WorkflowStepUdfGroupeDTO> workflowStepUdfGroupeDTOs)
			throws ResourcesNotFoundException {

		return workflowStepUdfGroupeService.saveAll(workflowStepUdfGroupeDTOs);
	}

	/**
	 * Find udf groups by step id.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the list
	 */
	@PostMapping("/find-udf-groups-by-step-id")
	public List<UserDefinedFieldGroupDTO> findUdfGroupsByStepId(
			@RequestBody WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO) {

		return workflowStepUdfGroupeService.findUdfGroupsByStepId(workflowStepUdfGroupeDTO);
	}

	/**
	 * Find udf fields by step id.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @param groupId the group id
	 * @return the list
	 */
	@PostMapping("/find-udf-fields-by-step-id/{groupId}")
	public List<UserDefinedFieldsDTO> findUdfFieldsByStepId(
			@RequestBody WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO,
			@PathVariable("groupId") Long groupId) {

		return workflowStepUdfGroupeService.findUdfFieldsByStepId(workflowStepUdfGroupeDTO,
				groupId);
	}
}
