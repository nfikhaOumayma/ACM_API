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

import com.acm.client.TransversClient;
import com.acm.exceptions.type.CollectionSettingException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.service.SettingWorkFlowService;
import com.acm.utils.dtos.CollectionStepDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;

/**
 * {@link SettingWorkFlowController} class.
 *
 * @author yesser somai
 * @since 1.0.10
 */

@RestController
@RequestMapping("/setting-workflow")
public class SettingWorkFlowController {

	/** The setting work flow service. */
	@Autowired
	private SettingWorkFlowService settingWorkFlowService;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/**
	 * Save Approval WorkFlow steps.
	 *
	 * @param WorkFlowStepDTOs the work flow step DTos
	 * @param ProductId the product id
	 * @param process the process
	 * @return the list of work flow step
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	@PostMapping("/save-approval-steps/{productId}/{process}")
	public List<WorkFlowStepDTO> saveApprovalSteps(
			@RequestBody List<WorkFlowStepDTO> WorkFlowStepDTOs,
			@PathVariable("productId") Long ProductId, @PathVariable("process") String process)
			throws WorkFlowSettingException {

		return settingWorkFlowService.saveSteps(WorkFlowStepDTOs, ProductId, process);
	}

	/**
	 * Find approval steps.
	 *
	 * @param workFlowStepDTO the work flow step DTO
	 * @return the list
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	@PostMapping("/find")
	public List<WorkFlowStepDTO> findApprovalStepsByProduct(
			@RequestBody WorkFlowStepDTO workFlowStepDTO) throws WorkFlowSettingException {

		return settingWorkFlowService.findSteps(workFlowStepDTO);
	}

	/**
	 * Save Collectop, steps.
	 *
	 * @param collectionStepDTOs the work flow step DTos
	 * @param ProductId the product id
	 * @param process the process
	 * @return the list of collection step
	 * @throws CollectionSettingException the collection setting exception
	 */
	@PostMapping("/save-collection-steps/{productId}/{process}")
	public List<CollectionStepDTO> saveCollectionSteps(
			@RequestBody List<CollectionStepDTO> collectionStepDTOs,
			@PathVariable("productId") Long ProductId, @PathVariable("process") String process)
			throws CollectionSettingException {

		return settingWorkFlowService.saveCollectionSteps(collectionStepDTOs, ProductId, process);
	}

	/**
	 * Find collection steps by product.
	 *
	 * @param collectionStepDTO the collection step DTO
	 * @return the list
	 * @throws CollectionSettingException the collection setting exception
	 */
	@PostMapping("/find-collection-steps")
	public List<CollectionStepDTO> findCollectionStepsByProduct(
			@RequestBody CollectionStepDTO collectionStepDTO) throws CollectionSettingException {

		return settingWorkFlowService.findCollectionStepsByProduct(collectionStepDTO);
	}

	/**
	 * Find setting collection.
	 * 
	 * @author idridi
	 * @param collectionStepDTO the collection step DTO
	 * @return the list
	 */
	@PostMapping("/find-setting-collection")
	public List<CollectionStepDTO> findSettingCollection(
			@RequestBody CollectionStepDTO collectionStepDTO) {

		return settingWorkFlowService.findSettingCollection(collectionStepDTO);
	}

}
