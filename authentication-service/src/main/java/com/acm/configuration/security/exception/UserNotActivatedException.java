/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.security.exception;

import org.springframework.security.core.AuthenticationException;

/**
 * {@link UserNotActivatedException} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class UserNotActivatedException extends AuthenticationException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1847794190813295260L;

	/**
	 * Instantiates a new user not activated exception.
	 *
	 * @param msg the msg
	 * @param t the t
	 */
	public UserNotActivatedException(String msg, Throwable t) {

		super(msg, t);
	}

	/**
	 * Instantiates a new user not activated exception.
	 *
	 * @param msg the msg
	 */
	public UserNotActivatedException(String msg) {

		super(msg);
	}

}
