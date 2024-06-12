/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_yakeen.service;

import java.io.IOException;
import java.util.Date;

import org.springframework.http.ResponseEntity;

/**
 * The Interface YakeenService.
 */
public interface YakeenService {

	/**
	 * Authentication.
	 *
	 * @param username the username
	 * @param password the password
	 * @return the response entity
	 */
	ResponseEntity<String> authentication(String username, String password);

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
	ResponseEntity<String> saudiByPassportOrNin(String passportNo, String passportExpiryDate,
			String nin, Date birthDateG) throws IOException;

}
