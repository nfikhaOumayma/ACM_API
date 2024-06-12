/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.CollectionAbacusService;
import com.acm.utils.dtos.AcmCollectionDTO;

/**
 * {@link LoadDataCollectionController} class.
 *
 * @author idridi
 * @since 1.1.9
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataCollectionController {

	/** The collection abacus service. */
	@Autowired
	CollectionAbacusService collectionAbacusService;

	
	/**
	 * Inits the collection abacus.
	 *
	 * @param token the token
	 * @return the integer
	 */
	@GetMapping("/init-list-of-collection")
	Integer initCollectionAbacus(@RequestHeader("Authorization") String token) {

		return collectionAbacusService.initCollectionAbacus();
	}
	
	/**
	 * Gets the collections abacus.
	 *
	 * @author idridi
	 * @param token the token
	 * @param rowIndex the row index
	 * @return the collections abacus
	 */
	@GetMapping("/get-list-of-collection/{rowIndex}")
	AcmCollectionDTO getCollectionFromAbacus(@RequestHeader("Authorization") String token, @PathVariable("rowIndex") Integer rowIndex) {

		return collectionAbacusService.getCollectionFromAbacus(rowIndex);
	}
}
