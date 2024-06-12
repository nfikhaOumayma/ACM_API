/*
 * Copyright(C)TALYS™-All Rights Reserved Unauthorized copying of this file,via any medium
 * is*strictly prohibited Proprietary and confidential
 */

package com.acm.configuration.feignclient;

import java.io.Serializable;

/**
 * The Class TechnicalException.
 */
public class TechnicalException implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -9090562310031761120L;

	/** The error code. */
	private String errorCode;

	/** The error message. */
	private String errorMessage;

	/**
	 * the class exception.
	 */
	private String classException;

	/**
	 * Gets the error code.
	 *
	 * @return the error code
	 */
	public String getErrorCode() {

		return errorCode;
	}

	/**
	 * Sets the error code.
	 *
	 * @param errorCode the new error code
	 */
	public void setErrorCode(String errorCode) {

		this.errorCode = errorCode;
	}

	/**
	 * Gets the error message.
	 *
	 * @return the error message
	 */
	public String getErrorMessage() {

		return errorMessage;
	}

	/**
	 * Sets the error message.
	 *
	 * @param errorMessage the new error message
	 */
	public void setErrorMessage(String errorMessage) {

		this.errorMessage = errorMessage;
	}

	/**
	 * Gets the class exception.
	 *
	 * @return the class exception
	 */
	public String getClassException() {

		return classException;
	}

	/**
	 * Sets the class exception.
	 *
	 * @param classException the new class exception
	 */
	public void setClassException(String classException) {

		this.classException = classException;
	}
}
