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

import com.acm.constants.common.CommonConstants;
import com.acm.service.AcmCollectionService;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.enums.CollectionStatus;

/**
 * The Class CollectionReader.
 */
public class LegalCollectionReader implements ItemReader<AcmCollectionDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LegalCollectionReader.class);

	/** The next collection. */
	private int nextCollection;

	/** The acm collection DT os. */
	private List<AcmCollectionDTO> acmCollectionDTOs;

	/** The acm collection service. */
	@Autowired
	private AcmCollectionService acmCollectionService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public AcmCollectionDTO read() throws Exception, UnexpectedInputException, ParseException,
			NonTransientResourceException {

		if (acmCollectionDTOs == null) {
			acmCollectionDTOs = getLegalCollections();
		}
		AcmCollectionDTO acmCollectionDTO = null;
		if (nextCollection < acmCollectionDTOs.size()) {
			acmCollectionDTO = acmCollectionDTOs.get(nextCollection);
			nextCollection++;

			return acmCollectionDTO;
		}
		else {
			nextCollection = 0;
			acmCollectionDTOs = null;
		}
		return acmCollectionDTO;
	}

	/**
	 * Gets the collections from abacus.
	 *
	 * @return the collections from abacus
	 */
	private List<AcmCollectionDTO> getLegalCollections() {

		// find by account number
		AcmCollectionDTO params = new AcmCollectionDTO();

		// Get only Collections records
		params.setCollectionType(CommonConstants.COLLECTION_CATEGORY);
		// check for Status Completed
		params.setStatus(CollectionStatus.COMPLETED.statusId());
		List<AcmCollectionDTO> acmCollections = acmCollectionService.find(params, Boolean.TRUE);

		logger.info("{} : Collections Ready to Legal Process was founded", acmCollections.size());
		return acmCollections;
	}
}
