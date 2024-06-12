/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import org.dozer.MappingException;

import com.acm.exceptions.type.ConditionalApproveException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.pagination.ItemPaginationDTO;

/**
 * The Interface ItemService.
 */
public interface ItemService {

	/**
	 * Save.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws MappingException the mapping exception
	 */
	ItemDTO save(ItemDTO itemDTO) throws WorkFlowSettingException, MappingException;

	/**
	 * Find pagination.
	 *
	 * @param itemPaginationDTO the item pagination DTO
	 * @return the item pagination DTO
	 */
	ItemPaginationDTO findPagination(ItemPaginationDTO itemPaginationDTO);

	/**
	 * Next step.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws MappingException the mapping exception
	 */
	ItemDTO nextStep(ItemDTO itemDTO)
			throws ConditionalApproveException, WorkFlowSettingException, MappingException;

	/**
	 * Reject.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	ItemDTO reject(ItemDTO itemDTO);

	/**
	 * Review.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	ItemDTO review(ItemDTO itemDTO);

	/**
	 * Assign item.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	ItemDTO assignItem(ItemDTO itemDTO);

}
