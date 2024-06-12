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
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link AddressCustomerWriter} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class AddressCustomerWriter implements ItemWriter<AddressDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(AddressCustomerWriter.class);

	/** The batch credit client. */
	@Autowired
	private CreditClient batchCreditClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends AddressDTO> addressDTOList) throws Exception {

		logger.debug("###  Writer : list size = {}", addressDTOList.size());
		if (!ACMValidationUtils.isNullOrEmpty(addressDTOList)) {
			// saving data in DB
			saveAddressDTOCustomers(addressDTOList);
		}
		logger.debug("### Writer :: DONE");
	}

	/**
	 * Save addressDTOList.
	 * 
	 * @author HaythemBenizid
	 * @param addressDTOList the addressDTOList
	 */
	private void saveAddressDTOCustomers(List<? extends AddressDTO> addressDTOList) {

		for (AddressDTO addressDTO : addressDTOList) {
			logger.debug("{}", addressDTO);
			batchCreditClient.createByBatch(addressDTO, addressDTO.getToken());
		}
		logger.debug("saving [{}] new addressDTOList in ACM-DB :: DONE", addressDTOList.size());
	}
}
