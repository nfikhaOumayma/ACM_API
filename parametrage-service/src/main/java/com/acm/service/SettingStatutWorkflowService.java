/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.SettingStatutWorkflowDTO;

/**
 * {@link SettingStatutWorkflowService} interface.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public interface SettingStatutWorkflowService {

	/**
	 * Find {@link List} of {@link SettingStatutWorkflowDTO} by given params. using Querydsl.
	 *
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	List<SettingStatutWorkflowDTO> find(SettingStatutWorkflowDTO settingStatutWorkflowDTO);

	/**
	 * Find All {@link List} of {@link SettingStatutWorkflowDTO} by given params. using Querydsl.
	 *
	 * @author AbdelkarimTurki
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	List<SettingStatutWorkflowDTO> findAll(SettingStatutWorkflowDTO settingStatutWorkflowDTO);

}
