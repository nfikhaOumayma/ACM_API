/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.model;

import java.io.Serializable;
import java.util.List;

/**
 * {@link ExceptionResponseMessage} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class ExceptionResponseMessage implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 9143130367707076675L;

	/** The error code. */
	private String errorCode;

	/** The error message. */
	private String errorMessage;

	/** The error technical message. */
	private TechnicalException errorTechnicalMessage;

	/** The errors. */
	private List<String> errors;

	/**
	 * Instantiates a new exception response message.
	 */
	public ExceptionResponseMessage() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new response msg.
	 *
	 * @param errorMessage the error message
	 */
	public ExceptionResponseMessage(String errorMessage) {

		this.errorMessage = errorMessage;
	}

	/**
	 * Instantiates a new exception response message.
	 *
	 * @param errorCode the error code
	 * @param errorMessage the error message
	 */
	public ExceptionResponseMessage(String errorCode, String errorMessage) {

		this.errorCode = errorCode;
		this.errorMessage = errorMessage;
	}

	/**
	 * Instantiates a new exception response message.
	 *
	 * @param errorCode the error code
	 * @param errorMessage the error message
	 * @param errorTechnicalMessage the error technical message
	 */
	public ExceptionResponseMessage(String errorCode, String errorMessage,
			TechnicalException errorTechnicalMessage) {

		this.errorCode = errorCode;
		this.errorMessage = errorMessage;
		this.errorTechnicalMessage = errorTechnicalMessage;
	}

	/**
	 * Instantiates a new exception response message.
	 * 
	 * @param errorCode the error code
	 * @param errorMessage the error message
	 * @param errorTechnicalMessage the error technical message {@link TechnicalException}
	 * @param errors the list of errors
	 */
	public ExceptionResponseMessage(String errorCode, String errorMessage,
			TechnicalException errorTechnicalMessage, List<String> errors) {

		this.errorCode = errorCode;
		this.errorMessage = errorMessage;
		this.errorTechnicalMessage = errorTechnicalMessage;
		this.errors = errors;
	}

	/**
	 * Gets the error code.
	 *
	 * @return the errorCode
	 */
	public String getErrorCode() {

		return errorCode;
	}

	/**
	 * Sets the error code.
	 *
	 * @param errorCode the errorCode to set
	 */
	public void setErrorCode(String errorCode) {

		this.errorCode = errorCode;
	}

	/**
	 * Gets the error message.
	 *
	 * @return the errorMessage
	 */
	public String getErrorMessage() {

		return errorMessage;
	}

	/**
	 * Sets the error message.
	 *
	 * @param errorMessage the errorMessage to set
	 */
	public void setErrorMessage(String errorMessage) {

		this.errorMessage = errorMessage;
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
	 * @param errors the errors to set
	 */
	public void setErrors(List<String> errors) {

		this.errors = errors;
	}

	/**
	 * Gets the error technical message.
	 *
	 * @return the errorTechnicalMessage
	 */
	public TechnicalException getErrorTechnicalMessage() {

		return errorTechnicalMessage;
	}

	/**
	 * Sets the error technical message.
	 *
	 * @param errorTechnicalMessage the errorTechnicalMessage to set
	 */
	public void setErrorTechnicalMessage(TechnicalException errorTechnicalMessage) {

		this.errorTechnicalMessage = errorTechnicalMessage;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ExceptionResponseMessage [errorCode=" + errorCode + ", errorMessage=" + errorMessage
				+ ", errorTechnicalMessage=" + errorTechnicalMessage + ", errors=" + errors + "]";
	}
}
