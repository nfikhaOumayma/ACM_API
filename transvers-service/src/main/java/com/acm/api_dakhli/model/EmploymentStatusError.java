package com.acm.api_dakhli.model;

// TODO: Auto-generated Javadoc
/**
 * The Class EmploymentStatusError.
 */
public class EmploymentStatusError {

	/** The error code. */
	private String errorCode;

	/** The error title. */
	private String errorTitle;

	/** The error type. */
	private String errorType;

	/** The error message. */
	private String errorMessage;

	/**
	 * Instantiates a new employment status error.
	 *
	 * @param errorCode the error code
	 * @param errorTitle the error title
	 * @param errorType the error type
	 * @param errorMessage the error message
	 */
	public EmploymentStatusError(String errorCode, String errorTitle, String errorType,
			String errorMessage) {

		super();
		this.errorCode = errorCode;
		this.errorTitle = errorTitle;
		this.errorType = errorType;
		this.errorMessage = errorMessage;
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

	/**
	 * Gets the error title.
	 *
	 * @return the error title
	 */
	public String getErrorTitle() {

		return errorTitle;
	}

	/**
	 * Sets the error title.
	 *
	 * @param errorTitle the new error title
	 */
	public void setErrorTitle(String errorTitle) {

		this.errorTitle = errorTitle;
	}

	/**
	 * Gets the error type.
	 *
	 * @return the error type
	 */
	public String getErrorType() {

		return errorType;
	}

	/**
	 * Sets the error type.
	 *
	 * @param errorType the new error type
	 */
	public void setErrorType(String errorType) {

		this.errorType = errorType;
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
}
