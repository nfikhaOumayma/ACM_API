/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class LoginRequestSimahApiDTO.
 */
public class LoginRequestSimahApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2045616812689863042L;

	/** The username. */
	@JsonProperty("username")
	private String username;

	/** The password. */
	@JsonProperty("password")
	private String password;

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUsername() {

		return username;
	}

	/**
	 * Sets the username.
	 *
	 * @param username the new username
	 */
	public void setUsername(String username) {

		this.username = username;
	}

	/**
	 * Gets the password.
	 *
	 * @return the password
	 */
	public String getPassword() {

		return password;
	}

	/**
	 * Sets the password.
	 *
	 * @param password the new password
	 */
	public void setPassword(String password) {

		this.password = password;
	}

	/**
	 * Instantiates a new login request simah api DTO.
	 */
	public LoginRequestSimahApiDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoginRequestSimahApiDTO [username=" + username + ", password=" + password + "]";
	}

}
