/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link CheckApprovelLevelException} class.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
public class CheckApprovelLevelException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1198855321018105708L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new check approvel level exception.
	 */
	public CheckApprovelLevelException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new check approvel level exception.
	 *
	 * @param message the message
	 */
	public CheckApprovelLevelException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new check approvel level exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public CheckApprovelLevelException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new check approvel level exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public CheckApprovelLevelException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new check approvel level exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CheckApprovelLevelException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new credit exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CheckApprovelLevelException(String exception,
			ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exception, exceptionResponseMessage);
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
