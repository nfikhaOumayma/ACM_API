/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CreditException} exception.
 *
 * @author HaythemBenizid
 * @since 1.0.7
 */
public class CreditException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -729605421170751548L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param message the message
	 */
	public CreditException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public CreditException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * instanciate exceptionResponseMessage {@link ExceptionResponseMessage}.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public CreditException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CreditException(ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CreditException(String exception, ExceptionResponseMessage exceptionResponseMessage,
			String message) {

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
