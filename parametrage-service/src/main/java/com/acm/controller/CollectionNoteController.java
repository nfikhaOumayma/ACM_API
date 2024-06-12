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
import com.acm.service.CollectionNoteService;
import com.acm.utils.dtos.CollectionNoteDTO;

/**
 * The Class CollectionNoteController.
 */
@RestController
@RequestMapping("/collection-note")
public class CollectionNoteController {
	/** The Customer service. */
	@Autowired
	private CollectionNoteService collectionNoteService;

	/**
	 * Find.
	 *
	 * @param collectionNoteDTO the collection note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/")
	public List<CollectionNoteDTO> find(@RequestBody CollectionNoteDTO collectionNoteDTO)
			throws ResourcesNotFoundException {

		return collectionNoteService.find(collectionNoteDTO);
	}

	/**
	 * Creates the.
	 *
	 * @param collectionNoteDTO the collection note DTO
	 * @return the collection note DTO
	 */
	@PostMapping("/create")
	public CollectionNoteDTO create(@RequestBody CollectionNoteDTO collectionNoteDTO) {

		return collectionNoteService.save(collectionNoteDTO);
	}

}
