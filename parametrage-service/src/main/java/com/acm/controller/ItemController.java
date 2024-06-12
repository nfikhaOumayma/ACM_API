/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.dozer.MappingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ConditionalApproveException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.service.ItemService;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.pagination.ItemPaginationDTO;

/**
 * The Class GenericObjectController.
 */
@RestController
@RequestMapping("/item-generic-workflow")
public class ItemController {

	/** The incentive operation service. */
	@Autowired
	private ItemService itemService;

	/**
	 * Creates the.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws MappingException the mapping exception
	 */
	@PostMapping("/create")
	public ItemDTO create(@RequestBody ItemDTO itemDTO)
			throws WorkFlowSettingException, MappingException {

		return itemService.save(itemDTO);
	}

	/**
	 * Find.
	 *
	 * @param itemPaginationDTO the item pagination DTO
	 * @return the item pagination DTO
	 */
	@PostMapping("/find-pagination")
	public ItemPaginationDTO find(@RequestBody ItemPaginationDTO itemPaginationDTO) {

		return itemService.findPagination(itemPaginationDTO);
	}

	/**
	 * Next step.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 * @throws ConditionalApproveException the conditional approve exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws MappingException the mapping exception
	 */
	@PostMapping("/nextStep")
	public ItemDTO nextStep(@RequestBody ItemDTO itemDTO)
			throws ConditionalApproveException, WorkFlowSettingException, MappingException {

		return itemService.nextStep(itemDTO);
	}

	/**
	 * Reject.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	@PostMapping("/reject")
	public ItemDTO reject(@RequestBody ItemDTO itemDTO) {

		return itemService.reject(itemDTO);
	}

	/**
	 * Review.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	@PostMapping("/review")
	public ItemDTO review(@RequestBody ItemDTO itemDTO) {

		return itemService.review(itemDTO);
	}

	/**
	 * Assign item.
	 *
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	@PostMapping("/assignItem")
	public ItemDTO assignItem(@RequestBody ItemDTO itemDTO) {

		return itemService.assignItem(itemDTO);
	}

}
