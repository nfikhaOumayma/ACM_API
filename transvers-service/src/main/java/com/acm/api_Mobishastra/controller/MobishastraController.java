/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_Mobishastra.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_Mobishastra.service.MobishastraService;

/**
 * The Class MobishastraController.
 */
@RestController
@RequestMapping("/mobishastra-api")
public class MobishastraController {

	/** The mobishastra service. */
	@Autowired
	private MobishastraService mobishastraService;

	/**
	 * Send sms otp.
	 *
	 * @param number the number
	 * @param msg the msg
	 * @return the response entity
	 */
	@GetMapping("/send-sms-otp")
	public ResponseEntity<String> sendSmsOtp(@RequestParam(value = "number") String number,
			@RequestParam(value = "msg") String msg) {

		return mobishastraService.sendSmsOtp(number, msg);
	}
}
