/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link IScoreExpiryDayFailedErrorException} class.
 *
 * @author MoezMhiri
 * @since 1.1.8
 */
public class IScoreExpiryDayFailedErrorException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6886368172796013594L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new i score expiry day exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public IScoreExpiryDayFailedErrorException(ExceptionResponseMessage exceptionResponseMessage,
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
