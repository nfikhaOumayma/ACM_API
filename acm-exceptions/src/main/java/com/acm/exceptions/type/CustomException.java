/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CustomException} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class CustomException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1598583973301325766L;

	/** The exception response message. */
	private ExceptionResponseMessage exceptionResponseMessage;

	/**
	 * Instantiates a new custom null pointer exception.
	 */
	public CustomException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new custom not found exception.
	 *
	 * @param message the message
	 */
	public CustomException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new custom exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public CustomException(ExceptionResponseMessage exceptionResponseMessage) {

		super();
		this.exceptionResponseMessage = exceptionResponseMessage;
	}

	/**
	 * Instantiates a new null pointer exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 */
	public CustomException(String exception, ExceptionResponseMessage exceptionResponseMessage) {

		super(exception);
		this.setExceptionResponseMessage(exceptionResponseMessage);
	}

	/**
	 * Gets the exception response message.
	 *
	 * @return the exceptionResponseMessage
	 */
	public ExceptionResponseMessage getExceptionResponseMessage() {

		return exceptionResponseMessage;
	}

	/**
	 * Sets the exception response message.
	 *
	 * @param exceptionResponseMessage the exceptionResponseMessage to set
	 */
	public void setExceptionResponseMessage(ExceptionResponseMessage exceptionResponseMessage) {

		this.exceptionResponseMessage = exceptionResponseMessage;
	}

}
