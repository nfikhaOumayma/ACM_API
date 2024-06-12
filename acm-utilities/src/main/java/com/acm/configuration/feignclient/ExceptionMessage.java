/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.configuration.feignclient;

import java.util.List;

/**
 * The Class ExceptionMessage.
 */
public class ExceptionMessage {

	/** The error code. */
	private String errorCode;

	/** The error message. */
	private String errorMessage;

	/** The error technical message. */
	private TechnicalException errorTechnicalMessage;

	/** The errors. */
	private List<String> errors;

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
	 * Gets the error technical message.
	 *
	 * @return the error technical message
	 */
	public TechnicalException getErrorTechnicalMessage() {

		return errorTechnicalMessage;
	}

	/**
	 * Sets the error technical message.
	 *
	 * @param errorTechnicalMessage the new error technical message
	 */
	public void setErrorTechnicalMessage(TechnicalException errorTechnicalMessage) {

		this.errorTechnicalMessage = errorTechnicalMessage;
	}

	/**
	 * Gets the errors.
	 *
	 * @return the errors
	 */
	public List<String> getErrors() {

		return errors;
	}

	/**
	 * Sets the errors.
	 *
	 * @param errors the new errors
	 */
	public void setErrors(List<String> errors) {

		this.errors = errors;
	}

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
}
