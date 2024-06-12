/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

/**
 * {@link RenewalConditionSettingLoanException} class.
 *
 * @author idridi
 * @since 1.0.8
 */
public class RenewalConditionSettingLoanException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 144708941303589496L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new calculate age exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public RenewalConditionSettingLoanException(ExceptionResponseMessage exceptionResponseMessage,
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
