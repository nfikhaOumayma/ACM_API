/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link GuarantorsNotFoundException} class.
 *
 * @author MoezMhiri
 * @since 0.9.0
 */
public class GuarantorsNotFoundException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7081372255055483522L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new custom not found exception.
	 */
	public GuarantorsNotFoundException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new custom not found exception.
	 *
	 * @param message the message
	 */
	public GuarantorsNotFoundException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new null pointer exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public GuarantorsNotFoundException(String exception, String message) {

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
