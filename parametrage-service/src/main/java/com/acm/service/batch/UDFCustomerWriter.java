/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link UDFCustomerWriter} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class UDFCustomerWriter implements ItemWriter<UserDefinedFieldsLinksDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UDFCustomerWriter.class);

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends UserDefinedFieldsLinksDTO> userDefinedFieldsLinksListDtos)
			throws Exception {

		logger.debug("###  Writer : list size = {}", userDefinedFieldsLinksListDtos.size());
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksListDtos)) {
			// saving or updating data in DB if not exist
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : userDefinedFieldsLinksListDtos) {
				creditClient.createByBatch(userDefinedFieldsLinksDTO);
			}
			logger.debug("saving or Updating [{}] UDF Link for customer in ACM-DB :: DONE",
					userDefinedFieldsLinksListDtos.size());
		}
		logger.debug("### Writer :: DONE");
	}
}
