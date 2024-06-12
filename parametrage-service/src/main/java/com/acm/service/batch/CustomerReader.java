/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.NonTransientResourceException;
import org.springframework.batch.item.ParseException;
import org.springframework.batch.item.UnexpectedInputException;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.models.AcmEnvironnement;

/**
 * The Class CustomerReader.
 */
public class CustomerReader implements ItemReader<CustomerDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The count. */
	private int count = 0;
	private List<CustomerDTO> customers;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public CustomerDTO read() throws Exception, UnexpectedInputException, ParseException,
			NonTransientResourceException {

		if (customers == null) {
			customers = loanDataFromAbacus();
		}

		if (count < customers.size()) {
			CustomerDTO customer = customers.get(count);
			count++;
			return customer;
		}
		else {
			count = 0;
			customers = null;
		}
		return null;
	}

	/**
	 * Loan data from abacus.
	 *
	 * @return the list
	 */
	private List<CustomerDTO> loanDataFromAbacus() {

		try {
			// loading the index where the last job has stopped
			AcmEnvironnementDTO environnementDTO =
					acmEnvironnementService.find("LIMITE_ID_CUSTOMER_EXTERNE");
			// loading list of customers from ABACUS DB
			List<CustomerDTO> abacusCustomers = transversClient.findCustomersForBatch(
					Long.valueOf(environnementDTO != null ? environnementDTO.getValue() : "0"));
			return abacusCustomers;

		}
		catch (ResourcesNotFoundException e) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			logger.error(e.getMessage());
			return null;
		}
	}
}
