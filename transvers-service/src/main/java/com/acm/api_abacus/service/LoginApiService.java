/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.service;

import java.io.IOException;

import org.springframework.http.HttpHeaders;

import com.acm.api_abacus.model.LoginAPI;

/**
 * {@link LoginApiService} interface.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public interface LoginApiService {

	/**
	 * Login API abacus. throws KeyManagementException the key management exception && throws
	 * NoSuchAlgorithmException the no such algorithm exception && throws KeyStoreException the key
	 * store exception && throws URISyntaxException
	 *
	 * @author HaythemBenizid
	 * @return the login API
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoginAPI loginAPIAbacus() throws IOException;

	/**
	 * Setting http headers with generating token => Name : X-Fern-Token .
	 * 
	 * @author HaythemBenizid
	 * @param headers the headers
	 * @return the http headers
	 */
	HttpHeaders settingHttpHeaders(HttpHeaders headers);

	/**
	 * Login API abacus.
	 *
	 * @param usernameAbacus the username abacus
	 * @param paymentFrom the payment from
	 * @return the login API
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	LoginAPI loginAPIAbacus(String usernameAbacus, String paymentFrom) throws IOException;

	/**
	 * Setting http headers.
	 *
	 * @param headers the headers
	 * @param usernameAbacus the username abacus
	 * @param paymentFrom the payment from
	 * @return the http headers
	 */
	HttpHeaders settingHttpHeaders(HttpHeaders headers, String usernameAbacus, String paymentFrom);
}
