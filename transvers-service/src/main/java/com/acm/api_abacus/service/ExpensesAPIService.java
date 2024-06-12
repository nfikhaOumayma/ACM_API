/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.ExpensesJournalPageDTO;

/**
 * {@link ExpensesAPIService} interface.
 *
 * @author idridi
 * @since 1.0.8
 */
public interface ExpensesAPIService {

	/**
	 * Creates the expensses journal page.
	 * 
	 * @author idridi
	 * @param expensesJournalPageDTO the expenses journal page DTO
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 */
	void createExpenssesJournalPage(ExpensesJournalPageDTO expensesJournalPageDTO)
			throws IOException, ApiAbacusException;
}
