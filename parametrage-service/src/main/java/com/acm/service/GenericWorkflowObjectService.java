/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.GenericWorkFlowObjectDTO;

/**
 * {@link GenericWorkflowObjectService} class.
 *
 * @since 1.0.14
 */
public interface GenericWorkflowObjectService {

	/**
	 * Save.
	 *
	 * @param genericWorkFlowObjectDTO the generic work flow object DTO
	 * @return the generic work flow object DTO
	 */
	GenericWorkFlowObjectDTO save(GenericWorkFlowObjectDTO genericWorkFlowObjectDTO);

	/**
	 * Find all.
	 *
	 * @return the list
	 */
	List<GenericWorkFlowObjectDTO> findAll();

}
