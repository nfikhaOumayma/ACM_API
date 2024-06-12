/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CustomerContactException} exception.
 *
 * @author ManelLamloum
 * @since 1.0.7
 */
public class CustomerContactException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6089352343365726177L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new customer contact exception.
	 *
	 * @param message the message
	 */
	public CustomerContactException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new customer contact exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public CustomerContactException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new customer contact exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CustomerContactException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
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
	 * Sets the message.
	 *
	 * @param message the new message
	 */
	public void setMessage(String message) {

		this.message = message;
	}

}
