/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.models.AcmEnvironnement;

import feign.FeignException;

/**
 * {@link AddressCustomerReader} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class AddressCustomerReader implements ItemReader<AddressDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(AddressCustomerReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The count. */
	private int count = 0;

	/** The token. */
	private String token = "NOT";

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public AddressDTO read() {

		logger.debug("### init reader process");
		if ("NOT".equals(token)) {
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}
		List<AddressDTO> addressAbacus = load();
		// reading founded list if not empty
		if (count < addressAbacus.size()) {
			AddressDTO addressDTO = addressAbacus.get(count);
			addressDTO.setToken(token);
			count++;
			return addressDTO;
		}
		else {
			count = 0;
		}
		return null;
	}

	/**
	 * Load data from ABACUS.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	private List<AddressDTO> load() {

		try {
			// loading the index where the last job has stopped
			AcmEnvironnementDTO environnementDTO =
					acmEnvironnementService.find("LIMITE_ID_ADDRESS");
			logger.debug("{}", environnementDTO);
			// loading list address customer from ABACUS DB

			List<AddressDTO> list = transversClient.loadAddressForCustomer(token,
					Long.valueOf(environnementDTO != null ? environnementDTO.getValue() : "0"));
			logger.debug("list size = {}", list.size());
			return list;
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// re-generate token if expired
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}
		catch (ResourcesNotFoundException e) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			logger.error(e.getMessage());
			return new ArrayList<>();
		}
		return new ArrayList<>();
	}
}
