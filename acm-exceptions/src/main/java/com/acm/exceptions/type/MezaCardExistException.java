/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link MezaCardExistException} class.
 *
 * @author MOEZ
 * @since 1.1.3
 */
public class MezaCardExistException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4224937031460793501L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new calculate age exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public MezaCardExistException(ExceptionResponseMessage exceptionResponseMessage,
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
