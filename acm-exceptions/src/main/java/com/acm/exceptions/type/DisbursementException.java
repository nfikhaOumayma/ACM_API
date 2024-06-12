/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.type;

/**
 * {@link DisbursementException} class.
 *
 * @author yesser.somai
 * @since 1.0.8
 */
public class DisbursementException extends CustomException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1836051193858682437L;

	/** The message. */
	private String message;

	/**
	 * Instantiates a new disbursement exception.
	 *
	 * @param message the message
	 */
	public DisbursementException(String message) {

		super();
		this.message = message;
	}

	/**
	 * Instantiates a new disbursement exception.
	 *
	 * @param exception the exception
	 * @param message the message
	 */
	public DisbursementException(String exception, String message) {

		super(exception);
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
