/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AuthResponseSimahApiDTO.
 */
public class AuthResponseSimahApiDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2331726947469712821L;

	/** The is success. */
	@JsonProperty("isSuccess")
	private boolean isSuccess;

	/** The data. */
	@JsonProperty("data")
	private AuthDataSimahApiDTO data;

	/** The message. */
	@JsonProperty("message")
	private String message;

	/**
	 * Gets the success.
	 *
	 * @return the success
	 */
	public boolean getSuccess() {

		return isSuccess;
	}

	/**
	 * Sets the success.
	 *
	 * @param isSuccess the new success
	 */
	public void setSuccess(boolean isSuccess) {

		this.isSuccess = isSuccess;
	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public AuthDataSimahApiDTO getData() {

		return data;
	}

	/**
	 * Sets the data.
	 *
	 * @param data the new data
	 */
	public void setData(AuthDataSimahApiDTO data) {

		this.data = data;
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
	 * @param message the new message
	 */
	public void setMessage(String message) {

		this.message = message;
	}

	/**
	 * Instantiates a new auth response simah api DTO.
	 */
	public AuthResponseSimahApiDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AuthResponseSimahApiDTO [isSuccess=" + isSuccess + ", data=" + data + ", message="
				+ message + "]";
	}

}
