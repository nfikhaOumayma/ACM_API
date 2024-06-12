/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.kyc.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.soap.kyc.model.KycDTO;
import com.acm.soap.kyc.service.KycSoapService;

/**
 * {@link KycAPIController} class.
 *
 * @author yesser.somai
 * @since 1.0.15
 */

@RestController
@RequestMapping("/kyc")
public class KycAPIController {

	/** The kyc soap service. */
	@Autowired
	private KycSoapService kycSoapService;

	/**
	 * Send request SOAP kyc get person details.
	 *
	 * @param kycDTO the kyc DTO
	 * @return the kyc DTO
	 */
	@PostMapping("/get-person-details")
	public KycDTO sendRequestSOAPKycGetPersonDetails(@RequestBody KycDTO kycDTO) {

		return kycSoapService.requestSOAPKycGetPersonDetails(kycDTO);
	}

}
