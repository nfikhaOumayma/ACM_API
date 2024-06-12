/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.ThirdPartyMappingData;

/**
 * {@link ThirdPartyMappingDataRepository } class.
 *
 * @author kouali
 * @since 0.1.0
 */

@Repository
public interface ThirdPartyMappingDataRepository
		extends JpaRepository<ThirdPartyMappingData, Long> {

	/**
	 * Find by original data and category.
	 *
	 * @author kouali
	 * @param nationality the nationality
	 * @param string the string
	 * @return the third party mapping data
	 */
	ThirdPartyMappingData findByOriginalDataAndCategory(String nationality, String string);

}
