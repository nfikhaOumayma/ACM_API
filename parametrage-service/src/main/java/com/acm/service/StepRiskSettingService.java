/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.StepRiskSettingDTO;

/**
 * The Interface StepRiskSettingService.
 */
public interface StepRiskSettingService {

	/**
	 * Find by item step.
	 *
	 * @param itemDTO the item DTO
	 * @return the list
	 */
	List<StepRiskSettingDTO> findByItemStep(ItemDTO itemDTO);

	/**
	 * Save.
	 *
	 * @param itemWfRiskSettingDTOs the item wf risk setting DT os
	 * @return the list
	 */
	List<StepRiskSettingDTO> save(List<StepRiskSettingDTO> itemWfRiskSettingDTOs);

}
