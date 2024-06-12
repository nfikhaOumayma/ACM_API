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

import com.acm.constants.common.CommonConstants;
import com.acm.service.AcmCollectionService;
import com.acm.utils.models.AcmCollection;

/**
 * The Class CollectionWriter.
 */
public class CollectionWriter implements ItemWriter<AcmCollection> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CollectionWriter.class);
	@Autowired
	private AcmCollectionService acmCollectionService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends AcmCollection> items) throws Exception {

		// create new collections / update old collections
		acmCollectionService.saveAll((List<AcmCollection>) items, CommonConstants.COLLECTION_CATEGORY);
		
		logger.info("Collection writer batch :: {} Collections ", items.size());
	}

}
