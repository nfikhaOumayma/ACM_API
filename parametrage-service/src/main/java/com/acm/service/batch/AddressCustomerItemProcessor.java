/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;

import com.acm.utils.dtos.AddressDTO;

/**
 * {@link AddressCustomerItemProcessor} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class AddressCustomerItemProcessor implements ItemProcessor<AddressDTO, AddressDTO> {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(AddressCustomerItemProcessor.class);

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemProcessor#process(java.lang.Object)
	 */
	@Override
	public AddressDTO process(AddressDTO addressDTO) throws Exception {

		logger.debug("### Address Customer Item Processor {}", addressDTO);
		return addressDTO;
	}
}
