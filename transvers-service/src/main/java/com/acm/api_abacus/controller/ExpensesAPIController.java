/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.service.ExpensesAPIService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.ExpensesJournalPageDTO;

/**
 * This class @{link ExpensesAPIController}.
 *
 * @author idridi
 * @since 1.0.8
 */
@RestController
@RequestMapping("/load-data-api-abacus")
public class ExpensesAPIController {

	/** The expenses API service. */
	@Autowired
	ExpensesAPIService expensesAPIService;

	/**
	 * Creates the journal page.
	 * 
	 * @author idridi
	 * @param expensesJournalPageDTO the expenses journal page DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	@PostMapping("/create-journal-page")
	public void createExpensesJournalPage(
			@RequestBody ExpensesJournalPageDTO expensesJournalPageDTO)
			throws IOException, ApiAbacusException {

		expensesAPIService.createExpenssesJournalPage(expensesJournalPageDTO);
	}
}
