/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.AMLPourcentageConfigurationException;
import com.acm.exceptions.type.IScoreExpiryDayException;
import com.acm.exceptions.type.IScoreExpiryDayFailedErrorException;
import com.acm.exceptions.type.IScoreProductConfigurationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ThirdPartyService;
import com.acm.soap.kyc.model.KycDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.models.ThirdPartyMappingData;

/**
 * This class @{link ThirdPartyController} used to control all the ThirdParty requests.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
@RestController
@RequestMapping("/third-party")
public class ThirdPartyController {

	/** The third party service. */
	@Autowired
	private ThirdPartyService thirdPartyService;

	/**
	 * Send AML request.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 * @throws AMLPourcentageConfigurationException the AML pourcentage configuration exception
	 */
	@PostMapping("/check-aml")
	public ScreeningDTO sendAMLRequest(@RequestBody ScreeningDTO screeningDTO)
			throws AMLPourcentageConfigurationException {

		return thirdPartyService.checkAML(screeningDTO);
	}

	/**
	 * Send ISCORE request.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @return the screening DTO
	 * @throws IScoreProductConfigurationException the i score product configuration exception
	 * @throws IScoreExpiryDayException the i score expiry day exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IScoreExpiryDayFailedErrorException the i score expiry day failed error exception
	 */
	@PostMapping("/check-iscore")
	public ScreeningDTO sendIScoreRequest(@RequestBody ScreeningDTO screeningDTO)
			throws IScoreProductConfigurationException, IScoreExpiryDayException,
			ResourcesNotFoundException, IScoreExpiryDayFailedErrorException {

		return thirdPartyService.checkIScore(screeningDTO);
	}

	/**
	 * Send KYC get person details.
	 *
	 * @author yesser.somai
	 * @param kycDTO the KYC DTO
	 * @return the KYC DTO
	 */
	@PostMapping("/check-kyc")
	public KycDTO sendKycGetPersonDetails(@RequestBody KycDTO kycDTO) {

		return thirdPartyService.sendKycGetPersonDetails(kycDTO);
	}

	/**
	 * Gets the third party mapping.
	 *
	 * @param nationality the nationality
	 * @param category the category
	 * @return the third party mapping
	 */
	@GetMapping("/mapping-third-party/{nationality}/{category}")
	public ThirdPartyMappingData getThirdPartyMapping(
			@PathVariable("nationality") String nationality,
			@PathVariable("category") String category) {

		return thirdPartyService.getThirdPartyMapping(nationality, category);
	}

}
