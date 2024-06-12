/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CollectionSettingException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.dtos.CollectionStepDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;

/**
 * {@link SettingWorkFlowService} class.
 *
 * @author yesser somai
 * @since 1.0.10
 */
public interface SettingWorkFlowService {

	/**
	 * Save steps.
	 *
	 * @author yesser somai
	 * @param WorkFlowStepDTOs the work flow step DT os
	 * @param ProductId the product id
	 * @param process the process
	 * @return the list of work flow step DTO
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	List<WorkFlowStepDTO> saveSteps(List<WorkFlowStepDTO> WorkFlowStepDTOs, Long ProductId,
			String process) throws WorkFlowSettingException;

	/**
	 * Find approval steps by product.
	 *
	 * @author yesser somai
	 * @param workFlowStepDTO the work flow step DTO
	 * @return the list
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	List<WorkFlowStepDTO> findSteps(WorkFlowStepDTO workFlowStepDTO)
			throws WorkFlowSettingException;

	/**
	 * Save steps.
	 *
	 * @author Maher Khemissi
	 * @param collectionStepDTOs the collection step DT os
	 * @param productId the product id
	 * @param process the process
	 * @return the list of collection step DTO
	 * @throws CollectionSettingException the collection setting exception
	 */
	List<CollectionStepDTO> saveCollectionSteps(List<CollectionStepDTO> collectionStepDTOs,
			Long productId, String process) throws CollectionSettingException;

	/**
	 * Find collcetion steps by product.
	 *
	 * @author Maher Khemissi
	 * @param collectionStepDTO the collection step DTO
	 * @return the list
	 * @throws CollectionSettingException the collection setting exception
	 */
	List<CollectionStepDTO> findCollectionStepsByProduct(CollectionStepDTO collectionStepDTO)
			throws CollectionSettingException;

	/**
	 * Find setting collection.
	 * 
	 * @author idridi
	 * @param collectionStepDTO the collection step DTO
	 * @return the list
	 */
	List<CollectionStepDTO> findSettingCollection(CollectionStepDTO collectionStepDTO);

}
