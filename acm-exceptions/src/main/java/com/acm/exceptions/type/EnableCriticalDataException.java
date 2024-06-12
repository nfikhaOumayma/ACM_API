/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link EnableCriticalDataException} class.
 *
 * @author idridi
 * @since 0.1.0
 */
public class EnableCriticalDataException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4532682634376830011L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param message the message
	 */
	public EnableCriticalDataException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public EnableCriticalDataException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public EnableCriticalDataException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public EnableCriticalDataException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new parametrage exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public EnableCriticalDataException(String exception,
			ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exception, exceptionResponseMessage);
		this.message = message;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Throwable#getMessage()
	 */
	@Override
	public String getMessage() {

		return message;
	}

	/**
	 * Sets message.
	 *
	 * @param message the message
	 */
	public void setMessage(String message) {

		this.message = message;
	}
}
