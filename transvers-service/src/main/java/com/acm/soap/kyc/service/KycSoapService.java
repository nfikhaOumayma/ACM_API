/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.kyc.service;

import com.acm.soap.kyc.model.KycDTO;

/**
 * {@link KycSoapService} class.
 *
 * @author yesser.somai
 * @since 0.1.0
 */
public interface KycSoapService {

	/**
	 * Request SOAP KYC.
	 *
	 * @author YesserSomai
	 * @param kycDTO the kyc DTO
	 * @return the person details
	 */
	KycDTO requestSOAPKycGetPersonDetails(KycDTO kycDTO);

}
