/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * The Class CancelIssuedLoanException.
 */
public class CancelIssuedLoanException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8767065815885307287L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new cancel issued loan exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CancelIssuedLoanException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Gets the message.
	 *
	 * @return the message
	 */
	public String getMessage() {

		return message;
	}

	/**
	 * Sets the message.
	 *
	 * @param message the new message
	 */
	public void setMessage(String message) {

		this.message = message;
	}

}
