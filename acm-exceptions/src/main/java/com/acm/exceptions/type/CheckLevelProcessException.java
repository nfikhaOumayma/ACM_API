/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link CheckLevelProcessException } class.
 *
 * @author AbdelkarimTurki
 * @since 1.0.2
 */
public class CheckLevelProcessException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1429769758772573457L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new check level process exception.
	 *
	 * @param message the message
	 */
	public CheckLevelProcessException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new check level process exception.
	 */
	public CheckLevelProcessException() {

		super();
	}

	/**
	 * Instantiates a new check level process exception.
	 *
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enable suppression
	 * @param writableStackTrace the writable stack trace
	 */
	public CheckLevelProcessException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {

		super(message, cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Instantiates a new check level process exception.
	 *
	 * @param message the message
	 * @param cause the cause
	 */
	public CheckLevelProcessException(String message, Throwable cause) {

		super(message, cause);
	}

	/**
	 * Instantiates a new check level process exception.
	 *
	 * @param cause the cause
	 */
	public CheckLevelProcessException(Throwable cause) {

		super(cause);
	}

	/**
	 * Instantiates a new check level process exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public CheckLevelProcessException(String exception, String message) {

		super(exception);
		this.setMessage(message);
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
	 * @param message the message to set
	 */
	public void setMessage(String message) {

		this.message = message;
	}

}
