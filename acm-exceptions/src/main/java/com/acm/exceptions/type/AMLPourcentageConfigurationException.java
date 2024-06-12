/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link AMLPourcentageConfigurationException} class.
 *
 * @author HaythemBenizid
 * @since 1.1.6
 */
public class AMLPourcentageConfigurationException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4814825050672267895L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new AML pourcentage configuration exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public AMLPourcentageConfigurationException(ExceptionResponseMessage exceptionResponseMessage,
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
