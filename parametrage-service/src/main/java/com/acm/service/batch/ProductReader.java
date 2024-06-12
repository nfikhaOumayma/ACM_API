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
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link ProductReader} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class ProductReader implements ItemReader<ProductDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(ProductReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The products abacus. */
	private List<ProductDTO> productsAbacus = new ArrayList<>();

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
	public ProductDTO read() {

		logger.debug("### init reader process");
		if ("NOT".equals(token)) {
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}

		// loading list product from ABACUS DB
		try {
			if (ACMValidationUtils.isNullOrEmpty(productsAbacus)) {
				productsAbacus = transversClient.find(token);
			}
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// re-generate token if expired
			token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
		}
		// reading founded list if not empty
		if (count < productsAbacus.size()) {
			ProductDTO productDTO = productsAbacus.get(count);
			productDTO.setToken(token);
			count++;
			return productDTO;
		}
		else {
			count = 0;
		}
		return null;
	}
}
