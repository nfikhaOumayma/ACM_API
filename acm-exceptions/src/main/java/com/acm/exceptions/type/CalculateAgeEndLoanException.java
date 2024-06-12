package com.acm.exceptions.type;

import com.acm.exceptions.model.ExceptionResponseMessage;

public class CalculateAgeEndLoanException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6372780946312939262L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new calculate age exception.
	 *
	 * @param exceptionResponseMessage the exception response message
	 * @param message the message
	 */
	public CalculateAgeEndLoanException(ExceptionResponseMessage exceptionResponseMessage,
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
