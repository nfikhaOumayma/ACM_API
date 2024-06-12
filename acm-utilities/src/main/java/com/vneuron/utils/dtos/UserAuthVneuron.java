/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link UserAuthVneuron } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class UserAuthVneuron {

	/** The user name. */
	private String user_name;

	/** The password. */
	private String password;

	/**
	 * Gets the user name.
	 *
	 * @return the user_name
	 */
	public String getUser_name() {

		return user_name;
	}

	/**
	 * Sets the user name.
	 *
	 * @param user_name the user_name to set
	 */
	public void setUser_name(String user_name) {

		this.user_name = user_name;
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
	 * @param password the password to set
	 */
	public void setPassword(String password) {

		this.password = password;
	}

}
