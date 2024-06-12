/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link LoanAlreadyExistException} class.
 *
 * @author idridi
 * @since 1.1.3
 */
public class LoanAlreadyExistException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8012140896163856001L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new loan already exist exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public LoanAlreadyExistException(ExceptionResponseMessage exceptionResponseMessage,
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
