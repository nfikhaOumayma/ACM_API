/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_Mobishastra.service;

import org.springframework.http.ResponseEntity;

/**
 * The Interface MobishastraService.
 */
public interface MobishastraService {

	/**
	 * Send sms otp.
	 *
	 * @param number the number
	 * @param msg the msg
	 * @return the response entity
	 */
	ResponseEntity<String> sendSmsOtp(String number, String msg);
}
