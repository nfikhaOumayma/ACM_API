/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import com.acm.exceptions.type.AMLPourcentageConfigurationException;
import com.acm.exceptions.type.IScoreExpiryDayException;
import com.acm.exceptions.type.IScoreExpiryDayFailedErrorException;
import com.acm.exceptions.type.IScoreProductConfigurationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.soap.kyc.model.KycDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.models.ThirdPartyMappingData;

/**
 * {@link ThirdPartyService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
public interface ThirdPartyService {

	/**
	 * Run AML check.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 * @throws AMLPourcentageConfigurationException the AML pourcentage configuration exception
	 */
	ScreeningDTO checkAML(ScreeningDTO screeningDTO) throws AMLPourcentageConfigurationException;

	/**
	 * Run IScore check.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IScoreProductConfigurationException the i score product configuration exception
	 * @throws IScoreExpiryDayException the i score expiry day exception
	 * @throws IScoreExpiryDayFailedErrorException the i score expiry day failed error exception
	 */
	ScreeningDTO checkIScore(ScreeningDTO screeningDTO)
			throws ResourcesNotFoundException, IScoreProductConfigurationException,
			IScoreExpiryDayException, IScoreExpiryDayFailedErrorException;

	/**
	 * Send KYC get person details.
	 *
	 * @author yesser.somai
	 * @param kycDTO the KYC DTO
	 * @return the KYC DTO
	 */
	KycDTO sendKycGetPersonDetails(KycDTO kycDTO);

	/**
	 * Gets the third party mapping.
	 *
	 * @param nationality the nationality
	 * @param category the category
	 * @return the third party mapping
	 */
	ThirdPartyMappingData getThirdPartyMapping(String nationality, String category);

}
