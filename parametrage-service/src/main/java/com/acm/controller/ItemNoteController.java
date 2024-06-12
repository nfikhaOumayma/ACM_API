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

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ItemNoteService;
import com.acm.utils.dtos.ItemNoteDTO;

/**
 * The Class CollectionNoteController.
 */
@RestController
@RequestMapping("/item-note")
public class ItemNoteController {
	/** The Customer service. */
	@Autowired
	private ItemNoteService itemNoteService;

	/**
	 * Find.
	 *
	 * @param itemNoteDTO the item note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/")
	public List<ItemNoteDTO> find(@RequestBody ItemNoteDTO itemNoteDTO)
			throws ResourcesNotFoundException {

		return itemNoteService.find(itemNoteDTO);
	}

	/**
	 * Creates the.
	 *
	 * @param itemNoteDTO the item note DTO
	 * @return the collection note DTO
	 */
	@PostMapping("/create")
	public ItemNoteDTO create(@RequestBody ItemNoteDTO itemNoteDTO) {

		return itemNoteService.save(itemNoteDTO);
	}

}
