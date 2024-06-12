/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link PwdConfirmInvalidException} exception.
 *
 * @author MoezMhiri
 * @since 1.1.1
 */
public class PwdConfirmInvalidException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8728198749564200378L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param message the message
	 */
	public PwdConfirmInvalidException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public PwdConfirmInvalidException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new api abacus exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public PwdConfirmInvalidException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public PwdConfirmInvalidException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

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
	public PwdConfirmInvalidException(String exception,
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
