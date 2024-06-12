/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.model;

import java.io.Serializable;

/**
 * {@link ExceptionResponseMessage} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class TechnicalException implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 9001526078266842957L;

	/** The error code. */
	private String errorCode;

	/** The error message. */
	private String errorMessage;

	/**
	 * the class exception.
	 */
	private String classException;

	/**
	 * default constructor.
	 */
	public TechnicalException() {

		/*
		 * Empty
		 */
	}

	/**
	 * constructor to set error code.
	 *
	 * @param errorMessage the error message
	 */
	public TechnicalException(String errorMessage) {

		this.errorMessage = errorMessage;
	}

	/**
	 * Instantiates a new Technical exception.
	 *
	 * @param errorCode the error code
	 * @param errorMessage the error message
	 */
	public TechnicalException(String errorCode, String errorMessage) {

		this.errorCode = errorCode;
		this.errorMessage = errorMessage;
	}

	/**
	 * Instantiates a new Technical exception.
	 *
	 * @param errorCode the error code
	 * @param errorMessage the error message
	 * @param classException the class exception
	 */
	public TechnicalException(String errorCode, String errorMessage, String classException) {

		this.errorCode = errorCode;
		this.errorMessage = errorMessage;
		this.classException = classException;
	}

	/**
	 * Gets error code.
	 *
	 * @return the error code
	 */
	public String getErrorCode() {

		return errorCode;
	}

	/**
	 * Sets error code.
	 *
	 * @param errorCode the error code
	 */
	public void setErrorCode(String errorCode) {

		this.errorCode = errorCode;
	}

	/**
	 * Gets error message.
	 *
	 * @return the error message
	 */
	public String getErrorMessage() {

		return errorMessage;
	}

	/**
	 * Sets error message.
	 *
	 * @param errorMessage the error message
	 */
	public void setErrorMessage(String errorMessage) {

		this.errorMessage = errorMessage;
	}

	/**
	 * Gets class exception.
	 *
	 * @return the class exception
	 */
	public String getClassException() {

		return classException;
	}

	/**
	 * Sets class exception.
	 *
	 * @param classException the class exception
	 */
	public void setClassException(String classException) {

		this.classException = classException;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "TechnicalException [" + (errorCode != null ? "errorCode=" + errorCode + ", " : "")
				+ (errorMessage != null ? "errorMessage=" + errorMessage + ", " : "")
				+ (classException != null ? "classException=" + classException : "") + "]";
	}

}
