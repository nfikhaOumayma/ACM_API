/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_payment.controller;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_payment.service.PaymentApiService;
import com.acm.utils.dtos.PaymentApiSanadDTO;

/**
 * The Class PaymentApiController.
 */
@RestController
@RequestMapping("/payment-api")
public class PaymentApiController {

	/** The payment api service. */
	@Autowired
	private PaymentApiService paymentApiService;

	/**
	 * Find target url and payment id.
	 *
	 * @param paymentApiSanadDTO the payment api sanad DTO
	 * @return the response entity
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@PostMapping("/find-targetUrl-paymentId")
	ResponseEntity<String> findTargetUrlAndPaymentId(
			@RequestBody PaymentApiSanadDTO paymentApiSanadDTO) throws KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException, IOException, URISyntaxException {

		return paymentApiService.findTargetUrlAndPaymentId(paymentApiSanadDTO);
	}

}
