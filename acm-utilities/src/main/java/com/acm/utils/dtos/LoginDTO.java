package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class LoginDTO.
 */
public class LoginDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The login. */
	private String login;

	/** The password. */
	private String password;

	/**
	 * Gets the login.
	 *
	 * @return the login
	 */
	public String getLogin() {

		return login;
	}

	/**
	 * Sets the login.
	 *
	 * @param login the new login
	 */
	public void setLogin(String login) {

		this.login = login;
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

}
