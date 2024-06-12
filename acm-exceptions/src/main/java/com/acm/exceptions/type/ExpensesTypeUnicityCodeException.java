/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * The Class ExpensesTypeUnicityCodeException.
 */
public class ExpensesTypeUnicityCodeException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5681578973264728119L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new expenses type unicity code exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public ExpensesTypeUnicityCodeException(ExceptionResponseMessage exceptionResponseMessage,
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
