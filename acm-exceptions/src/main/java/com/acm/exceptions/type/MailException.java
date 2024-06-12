/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link MailException} Ged exception.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class MailException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2238690153372280006L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new mail exception.
	 *
	 * @param message the message
	 */
	public MailException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new mail exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public MailException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new mail exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public MailException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new mail exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public MailException(ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new mail exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public MailException(String exception, ExceptionResponseMessage exceptionResponseMessage,
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
