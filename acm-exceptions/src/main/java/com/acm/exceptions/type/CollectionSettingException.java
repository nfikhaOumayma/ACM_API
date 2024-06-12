/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CollectionSettingException} class.
 *
 * @author Maher khemissi
 * @since 1.0.10
 */
public class CollectionSettingException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new collection setting exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CollectionSettingException(ExceptionResponseMessage exceptionResponseMessage,
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
