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
 * {@link UDFLoanWriter} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class UDFLoanWriter implements ItemWriter<UserDefinedFieldsLinksDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UDFLoanWriter.class);

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends UserDefinedFieldsLinksDTO> userDefinedFieldsLinksList)
			throws Exception {

		logger.debug("### UDFLoanWriter : list size = {}", userDefinedFieldsLinksList.size());
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksList)) {
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : userDefinedFieldsLinksList) {
				creditClient.createByBatch(userDefinedFieldsLinksDTO);
			}
			logger.debug("saving or Updating [{}] UDF Link for Loan in ACM-DB :: DONE",
					userDefinedFieldsLinksList.size());
		}
		logger.debug("### UDFLoanWriter :: DONE");
	}
}
