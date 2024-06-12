/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.SettingStatutWorkflowService;
import com.acm.utils.dtos.SettingStatutWorkflowDTO;

/**
 * This class @{link SettingStatutWorkflowController} used to control all the SettingStatutWorkflow
 * requests.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
@RestController
@RequestMapping("/setting-statut-workflows")
public class SettingStatutWorkflowController {

	/** The SettingStatutWorkflow service. */
	@Autowired
	private SettingStatutWorkflowService settingStatutWorkflowService;

	/**
	 * Find list SettingStatutWorkflow by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingStatutWorkflowDTO> find(
			@RequestBody SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		return settingStatutWorkflowService.find(settingStatutWorkflowDTO);
	}

	/**
	 * Find list SettingStatutWorkflow by given CLIENT (USED ONLY while saving new loan).
	 *
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	@PostMapping("/find-loan-process")
	public List<SettingStatutWorkflowDTO> findLoanProcess(
			@RequestBody SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		settingStatutWorkflowDTO.setIsNewLoan(Boolean.TRUE);
		return settingStatutWorkflowService.find(settingStatutWorkflowDTO);
	}

	/**
	 * Find all list SettingStatutWorkflow.
	 *
	 * @author AbdelkarimTurki
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	@PostMapping("/find-all")
	public List<SettingStatutWorkflowDTO> findAll(
			@RequestBody SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		return settingStatutWorkflowService.findAll(settingStatutWorkflowDTO);
	}

}
