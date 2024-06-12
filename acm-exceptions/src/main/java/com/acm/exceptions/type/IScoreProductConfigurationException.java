/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link IScoreProductConfigurationException} class.
 *
 * @author HaythemBenizid
 * @since 1.1.6
 */
public class IScoreProductConfigurationException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1965219512440874344L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new i score product configuration exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public IScoreProductConfigurationException(ExceptionResponseMessage exceptionResponseMessage,
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
