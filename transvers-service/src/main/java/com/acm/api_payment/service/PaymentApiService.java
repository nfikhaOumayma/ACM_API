/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_payment.service;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import org.springframework.http.ResponseEntity;

import com.acm.utils.dtos.PaymentApiSanadDTO;

/**
 * The Interface PaymentApiService.
 */
public interface PaymentApiService {

	/**
	 * Find target url and payment id.
	 *
	 * @param paymentApiSanadDTO the payment api sanad DTO
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	ResponseEntity<String> findTargetUrlAndPaymentId(PaymentApiSanadDTO paymentApiSanadDTO)
			throws IOException, KeyManagementException, KeyStoreException, NoSuchAlgorithmException,
			URISyntaxException;
}
