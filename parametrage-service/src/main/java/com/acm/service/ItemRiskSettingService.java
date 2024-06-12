/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.ItemRiskSettingDTO;

/**
 * The Interface ItemRiskSettingService.
 */
public interface ItemRiskSettingService {

	/**
	 * Find by item.
	 *
	 * @param itemDTO the item DTO
	 * @return the list
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	List<ItemRiskSettingDTO> findByItem(ItemDTO itemDTO) throws WorkFlowSettingException;

	/**
	 * Save.
	 *
	 * @param itemInstanceRiskSettingDTOs the item instance risk setting DT os
	 * @return the list
	 */
	List<ItemRiskSettingDTO> save(List<ItemRiskSettingDTO> itemInstanceRiskSettingDTOs);

}
