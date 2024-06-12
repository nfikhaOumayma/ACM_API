/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link CheckAppL4NotFoundException} class.
 *
 * @author MoezMhiri
 * @since 0.9.0
 */
public class CheckAppL4NotFoundException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3453341807525013331L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new check app L 4 not found exception.
	 */
	public CheckAppL4NotFoundException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new check app L 4 not found exception.
	 *
	 * @param message the message
	 */
	public CheckAppL4NotFoundException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new check app L 4 not found exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public CheckAppL4NotFoundException(String exception, String message) {

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
