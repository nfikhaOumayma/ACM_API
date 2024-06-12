/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link MezaCardsExistInDbException} class.
 *
 * @author idridi
 * @since 1.1.3
 */
public class MezaCardsExistInDbException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 995116533786967305L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new meza cards exist in db exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public MezaCardsExistInDbException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new meza cards exist in db exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public MezaCardsExistInDbException(String exception, String message) {

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
