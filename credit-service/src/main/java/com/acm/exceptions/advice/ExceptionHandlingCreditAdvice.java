/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.advice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.acm.constants.common.CommonErrorCode;
import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link ExceptionHandlingCreditAdvice} class.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@RestControllerAdvice
public class ExceptionHandlingCreditAdvice {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(ExceptionHandlingCreditAdvice.class);

	/**
	 * Activiti exception handler / Activiti optimistic locking exception handler.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	// @ExceptionHandler({ActivitiException.class, ActivitiOptimisticLockingException.class})
	// public ResponseEntity<ExceptionResponseMessage> activitiExceptionHandler(
	// ActivitiException exception) {
	//
	// ExceptionResponseMessage response = new ExceptionResponseMessage();
	// response.setErrorCode(CommonErrorCode.ACTIVITI_EXCEPTION);
	// response.setErrorMessage(exception.getMessage());
	//
	// logger.error("ActivitiException type / ActivitiOptimisticLockingException type");
	// return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	// }

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
