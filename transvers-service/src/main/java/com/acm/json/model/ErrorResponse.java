/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link ErrorResponse} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class ErrorResponse {

	/** The error code. */
	@JsonProperty("ErrorCode")
	public String errorCode;

	/** The error description. */
	@JsonProperty("ErrorDescription")
	public String errorDescription;

	/**
	 * Instantiates a new error response.
	 */
	public ErrorResponse() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ErrorResponse [" + (errorCode != null ? "errorCode=" + errorCode + ", " : "")
				+ (errorDescription != null ? "errorDescription=" + errorDescription : "") + "]";
	}

}
