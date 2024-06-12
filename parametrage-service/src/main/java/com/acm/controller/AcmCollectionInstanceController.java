/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.CollectionInstanceService;
import com.acm.utils.dtos.CollectionInstanceDTO;

/**
 * The Class AcmCollectionInstanceController.
 */
@RestController
@RequestMapping("/collectionInstance")
public class AcmCollectionInstanceController {

	/** The collection service. */
	@Autowired
	private CollectionInstanceService collectionService;

	/**
	 * Update.
	 *
	 * @param acmCollectionDTO the acm collection DTO
	 * @return the collection instance DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public CollectionInstanceDTO update(@RequestBody CollectionInstanceDTO acmCollectionDTO)
			throws ResourcesNotFoundException {

		return collectionService.save(acmCollectionDTO.getId(), acmCollectionDTO);
	}

}
