/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;

/**
 * {@link LoginAPI} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public class LoginAPI implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4005399012494445650L;

	/** The user id. */
	private Long userID;

	/** The token. */
	private String token;

	/** The terminal id. */
	private Long terminalId;

	/** The message. */
	private String message;

	/** The status code. */
	private String statusCode;

	/**
	 * Instantiates a new login API.
	 */
	public LoginAPI() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new login API.
	 *
	 * @param message the message
	 * @param statusCode the status code
	 */
	public LoginAPI(String message, String statusCode) {

		this.message = message;
		this.statusCode = statusCode;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoginAPI [userID=" + userID + ", token=" + token + ", terminalId=" + terminalId
				+ ", message=" + message + ", statusCode=" + statusCode + "]";
	}

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the token to set
	 */
	public void setToken(String token) {

		this.token = token;
	}

	/**
	 * Gets the terminal id.
	 *
	 * @return the terminalId
	 */
	public Long getTerminalId() {

		return terminalId;
	}

	/**
	 * Sets the terminal id.
	 *
	 * @param terminalId the terminalId to set
	 */
	public void setTerminalId(Long terminalId) {

		this.terminalId = terminalId;
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

	/**
	 * Gets the user ID.
	 *
	 * @return the userID
	 */
	public Long getUserID() {

		return userID;
	}

	/**
	 * Sets the user ID.
	 *
	 * @param userID the userID to set
	 */
	public void setUserID(Long userID) {

		this.userID = userID;
	}

	/**
	 * Gets the status code.
	 *
	 * @return the statusCode
	 */
	public String getStatusCode() {

		return statusCode;
	}

	/**
	 * Sets the status code.
	 *
	 * @param statusCode the statusCode to set
	 */
	public void setStatusCode(String statusCode) {

		this.statusCode = statusCode;
	}

}
