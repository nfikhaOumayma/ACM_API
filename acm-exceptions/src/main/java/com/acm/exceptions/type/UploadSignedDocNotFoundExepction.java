/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link UploadSignedDocNotFoundExepction} class.
 *
 * @author MoezMhiri
 * @since 0.9.0
 */
public class UploadSignedDocNotFoundExepction extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3613755909607655730L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new custom not found exception.
	 */
	public UploadSignedDocNotFoundExepction() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new custom not found exception.
	 *
	 * @param message the message
	 */
	public UploadSignedDocNotFoundExepction(String message) {

		super(message);
	}

	/**
	 * Instantiates a new null pointer exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public UploadSignedDocNotFoundExepction(String exception, String message) {

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
