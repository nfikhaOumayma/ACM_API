/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import com.acm.utils.dtos.AcmCollectionDTO;

/**
 * The Interface CollectionAbacusService.
 */
public interface CollectionAbacusService {

	/**
	 * Gets the collection abacus.
	 * 
	 * @author idridi
	 * @return the collection abacus
	 */
	Integer initCollectionAbacus();

	/**
	 * Gets the collection from abacus.
	 *
	 * @param index the index
	 * @return the collection from abacus
	 */
	AcmCollectionDTO getCollectionFromAbacus(int index);
}
