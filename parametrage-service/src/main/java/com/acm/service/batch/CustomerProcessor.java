/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.client.TransversClient;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The Class CustomerProcessor.
 */
public class CustomerProcessor implements ItemProcessor<CustomerDTO, CustomerDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerProcessor.class);
	@Autowired
	private CreditClient creditClient;

	@Autowired
	private TransversClient transversClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemProcessor#process(java.lang.Object)
	 */
	@Override
	public CustomerDTO process(CustomerDTO item) throws Exception {

		List<CustomerDTO> customerDTOs =
				creditClient.findCustomerIdExtern(item.getCustomerIdExtern());
		if (ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
			logger.debug(
					"#############  Customer Item Processor customer does not exist{} #####################",
					item);
			return item;
		}
		logger.debug(
				"#############  Customer Item Processor customer exist{} #####################",
				item);
		return null;
	}

}
