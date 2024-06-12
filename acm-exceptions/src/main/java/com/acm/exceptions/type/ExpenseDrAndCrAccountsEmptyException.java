/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * The Class ExpenseDrAndCrAccountsEmptyException.
 */
public class ExpenseDrAndCrAccountsEmptyException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8473527699409855398L;
	/** The message. */
	private String message;

	/**
	 * Instantiates a new custom not found exception.
	 */
	public ExpenseDrAndCrAccountsEmptyException() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new custom not found exception.
	 *
	 * @param message the message
	 */
	public ExpenseDrAndCrAccountsEmptyException(String message) {

		super(message);
	}

	/**
	 * Instantiates a new null pointer exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public ExpenseDrAndCrAccountsEmptyException(String exception, String message) {

		super(exception);
		this.setMessage(message);
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
