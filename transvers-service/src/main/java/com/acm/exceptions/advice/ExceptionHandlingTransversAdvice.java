/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.advice;

import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.client.RestClientResponseException;

import com.acm.constants.common.CommonErrorCode;
import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link ExceptionHandlingTransversAdvice} class.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@RestControllerAdvice
public class ExceptionHandlingTransversAdvice {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(ExceptionHandlingTransversAdvice.class);

	/**
	 * Rest client response exception handler / Uri syntax exception handler / No such algorithm
	 * exception handler / Key store exception handler / Key management exception handler / Http
	 * client error exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler({RestClientResponseException.class, URISyntaxException.class,
			NoSuchAlgorithmException.class, KeyStoreException.class, KeyManagementException.class,
			org.springframework.web.client.HttpClientErrorException.class})
	public ResponseEntity<ExceptionResponseMessage> restClientResponseExceptionHandler(
			RestClientResponseException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.API_ABACUS_UNREACHABLE);
		response.setErrorMessage(exception.getMessage());

		logger.error(
				"RestClientResponseException type / URISyntaxException type / NoSuchAlgorithmException type /"
						+ " KeyStoreException type / KeyManagementException type / HttpClientErrorException type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * (com.netflix.discovery.shared.transport.TransportException)Transport exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(com.netflix.discovery.shared.transport.TransportException.class)
	public ResponseEntity<ExceptionResponseMessage> transportException(
			com.netflix.discovery.shared.transport.TransportException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_LOAD_BALANCER_NOT_AVAILABLE);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire com.netflix.discovery.shared.transport.TransportException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}
}
