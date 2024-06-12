/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link SaveFileException} class Save File Exception.
 *
 * @author YesserSomai
 * @since 1.0.6
 */
public class SaveFileException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3370370465224330733L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new save file exception.
	 */
	public SaveFileException() {

	}

	/**
	 * Instantiates a new save file exception.
	 *
	 * @param message the message
	 */
	public SaveFileException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new save file exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public SaveFileException(String exception, String message) {

		super(exception);
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
