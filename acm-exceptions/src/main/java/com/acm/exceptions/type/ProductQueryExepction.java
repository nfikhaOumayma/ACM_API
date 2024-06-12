/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CalculateAgeException} class.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
public class ProductQueryExepction extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6372780946312939262L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new calculate age exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public ProductQueryExepction(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
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
	 * @param message the message to set
	 */
	public void setMessage(String message) {

		this.message = message;
	}
}
