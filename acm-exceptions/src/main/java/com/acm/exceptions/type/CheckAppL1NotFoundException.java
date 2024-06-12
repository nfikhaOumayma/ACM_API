/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link CheckAppL1NotFoundException} class.
 *
 * @author MoezMhiri
 * @since 0.9.0
 */
public class CheckAppL1NotFoundException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8632463802330738812L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new check app L 1 not found exception.
	 */
	public CheckAppL1NotFoundException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new check app L 1 not found exception.
	 *
	 * @param message the message
	 */
	public CheckAppL1NotFoundException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new null pointer exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public CheckAppL1NotFoundException(String exception, String message) {

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
