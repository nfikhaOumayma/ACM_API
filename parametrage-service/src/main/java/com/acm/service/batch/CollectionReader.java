/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import org.hibernate.jdbc.BatchFailedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.NonTransientResourceException;
import org.springframework.batch.item.ParseException;
import org.springframework.batch.item.UnexpectedInputException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.AcmCollectionDTO;

/**
 * The Class CollectionReader.
 */
public class CollectionReader implements ItemReader<AcmCollectionDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CollectionReader.class);

	/** The next collection. */
	private int nextCollection = 0;

	/** The count collection. */
	private Integer countCollection = null;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;
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
	public AcmCollectionDTO read() throws Exception, UnexpectedInputException, ParseException,
			NonTransientResourceException {

		if (countCollection == null) {
			countCollection = initCollectionAbacus();
		}
		AcmCollectionDTO acmCollectionDTO = null;
		if (nextCollection < countCollection) {
			nextCollection++;
		}
		else {
			// send 0 to remove temp collection table from Abacus
			nextCollection = 0;
			countCollection = null;
		}
		acmCollectionDTO = getCollectionFromAbacus(nextCollection);

		return acmCollectionDTO;
	}

	/**
	 * Gets the collections from abacus.
	 *
	 * @return the collections from abacus
	 */
	private Integer initCollectionAbacus() {

		token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);

		// loading list of customers from ABACUS DB
		Integer abacusCollection = 0;
		try {
			abacusCollection = transversClient.initCollectionAbacus(token);
		}

		catch (Exception e) {
			logger.error("Failed to Initialize Collections list on Abacus");
			throw new BatchFailedException("Error Init Collections On Abacus");

		}
		logger.info("{} : Collections from abacus was founded", abacusCollection);

		if (abacusCollection == 0) {
			logger.error("Failed to Initialize Collections list on Abacus");
			throw new BatchFailedException("Error Init Collections On Abacus");
		}
		return abacusCollection;
	}

	/**
	 * Gets the collections from abacus.
	 *
	 * @param index the index
	 * @return the collections from abacus
	 */
	private AcmCollectionDTO getCollectionFromAbacus(int index) {

		int nbTry = 0;
		while (nbTry < 2) {
			if (nbTry > 0) {
				token = "Bearer " + CommonFunctions.generateToken(urlServeurAuthentification);
				logger.warn("New Generated Token for Collection Batch : {} ", token);
			}
			// loading list of customers from ABACUS DB
			AcmCollectionDTO abacusCollection = null;
			try {
				abacusCollection = transversClient.getCollectionFromAbacus(token, index);
				logger.info("Collection {} / {} : {} loaded from abacus ", index, countCollection,
						abacusCollection.getAccountNumber());

				return abacusCollection;
			}
			catch (Exception e) {
				if (nbTry < 2) {
					nbTry++;
					logger.error(
							"Failed to get Collection list from Abacus : regenarate token and retry ...");
				}
				else {
					logger.error(
							"Failed to get Collection from Abacus after retry  : return empty collection !! ");
					return new AcmCollectionDTO();
				}
			}
		}
		return null;
	}

	/**
	 * Reset.
	 */
	public void reset() {

		logger.info("Reset Collection Reader - old values : {} / {} ", nextCollection,
				countCollection);
		nextCollection = 0;
		countCollection = null;

	}
}
