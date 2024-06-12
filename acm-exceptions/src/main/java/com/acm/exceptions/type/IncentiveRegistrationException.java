/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link IncentiveRegistrationException} class.
 * 
 * @author idridi
 * @since 1.0.8
 */
public class IncentiveRegistrationException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4346637243852518933L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new incentive registration exception.
	 *
	 * @param message the message
	 */
	public IncentiveRegistrationException(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new incentive registration exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public IncentiveRegistrationException(String exception, String message) {

		super(exception);
		this.message = message;
	}

	/**
	 * Instantiates a new incentive registration exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 */
	public IncentiveRegistrationException(ExceptionResponseMessage exceptionResponseMessage) {

		super(exceptionResponseMessage);
	}

	/**
	 * Instantiates a new incentive registration exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public IncentiveRegistrationException(ExceptionResponseMessage exceptionResponseMessage,
			String message) {

		super(exceptionResponseMessage);
		this.message = message;
	}

	/**
	 * Instantiates a new incentive registration exception.
	 *
	 * @param exception the exception
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public IncentiveRegistrationException(String exception,
			ExceptionResponseMessage exceptionResponseMessage, String message) {

		super(exception, exceptionResponseMessage);
		this.message = message;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Throwable#getMessage()
	 */
	@Override
	public String getMessage() {

		return message;
	}

	/**
	 * Sets message.
	 *
	 * @param message the message
	 */
	public void setMessage(String message) {

		this.message = message;
	}
}
