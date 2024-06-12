/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link ReportingException} exception.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
public class ReportingException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7492496082047390882L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new reporting exception.
	 *
	 * @param message the message
	 */
	public ReportingException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new reporting exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public ReportingException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * instanciate exceptionResponseMessage {@link ExceptionResponseMessage}.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public ReportingException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new reporting exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public ReportingException(ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new reporting exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public ReportingException(String exception, ExceptionResponseMessage exceptionResponseMessage,
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
