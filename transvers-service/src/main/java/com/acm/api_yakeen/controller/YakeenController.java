/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_yakeen.controller;

import java.io.IOException;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_yakeen.service.YakeenService;

/**
 * The Class YakeenController.
 */
@RestController
@RequestMapping("/yakeen-api")
public class YakeenController {

	/** The yaken service. */
	@Autowired
	private YakeenService yakenService;

	/**
	 * Authentication.
	 *
	 * @param username the username
	 * @param password the password
	 * @return the response entity
	 */
	@GetMapping("/authentication")
	public ResponseEntity<String> authentication(@RequestParam("Username") String username,
			@RequestParam("Password") String password) {

		return yakenService.authentication(username, password);
	}

	/**
	 * Saudi by passport or nin.
	 *
	 * @param passportNo the passport no
	 * @param passportExpiryDate the passport expiry date
	 * @param nin the nin
	 * @param birthDateG the birth date G
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@GetMapping("/saudiByPassportOrNin")
	public ResponseEntity<String> saudiByPassportOrNin(
			@RequestParam(value = "passportNo", required = false) String passportNo,
			@RequestParam(value = "passportExpiryDate", required = false) String passportExpiryDate,
			@RequestParam(value = "nin", required = false) String nin,
			@RequestParam(value = "birthDateG", required = false) Date birthDateG)
			throws IOException {

		return yakenService.saudiByPassportOrNin(passportNo, passportExpiryDate, nin, birthDateG);
	}

}
