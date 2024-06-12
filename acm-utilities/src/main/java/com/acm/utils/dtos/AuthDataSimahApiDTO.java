/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class AuthDataSimahApiDTO.
 */
public class AuthDataSimahApiDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7630394852494106216L;

	/** The token. */
	private String token;

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
	 * @param token the new token
	 */
	public void setToken(String token) {

		this.token = token;
	}

	/**
	 * Instantiates a new auth data simah api DTO.
	 */
	public AuthDataSimahApiDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AuthDataSimahApiDTO [token=" + token + "]";
	}

}
