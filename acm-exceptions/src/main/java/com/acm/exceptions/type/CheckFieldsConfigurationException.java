/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CheckFieldsConfigurationException} class.
 *
 * @author HaythemBenizid
 * @since 1.1.4
 */
public class CheckFieldsConfigurationException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1644445695939632332L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new check fields configuration exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CheckFieldsConfigurationException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new check fields configuration exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public CheckFieldsConfigurationException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
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
